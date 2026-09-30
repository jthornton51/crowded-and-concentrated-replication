suppressPackageStartupMessages({library(data.table);library(ggplot2);library(diptest);library(fixest)})
setDTthreads(4);setFixest_nthreads(4)
args<-commandArgs(TRUE);stopifnot(length(args)==4L)
root<-normalizePath(args[1],winslash="/",mustWork=TRUE)
cp<-normalizePath(args[2],winslash="/",mustWork=TRUE)
gp<-normalizePath(args[3],winslash="/",mustWork=TRUE)
out<-args[4];dir.create(out,recursive=TRUE,showWarnings=FALSE)
raw<-fread(file.path(root,'data/processed/pz_constituency_measures.csv'),select=c('EIN','TAXYEAR','FTYPE','CBSA','ntee_broad_clean','private_contributions','public_grants','client_services','GOVT_GRANTS','TOTREV'))
raw[,EIN:=as.numeric(EIN)]
for(v in c('private_contributions','public_grants','client_services','GOVT_GRANTS','TOTREV'))set(raw,j=v,value=as.numeric(raw[[v]]))
stopifnot(nrow(raw)==4720874)
src<-unique(fread(file.path(root,'data/external/government_grants.csv'),select=c('EIN','TAXYEAR')));src[,EIN:=as.numeric(EIN)]
raw[,source_present:=FALSE];raw[src,on=.(EIN,TAXYEAR),source_present:=TRUE]
acct<-function(x,label){r<-x[,.(N=.N,source_present=sum(source_present),field_present=sum(!is.na(GOVT_GRANTS)),explicit_zero=sum(GOVT_GRANTS==0,na.rm=TRUE),positive=sum(GOVT_GRANTS>0,na.rm=TRUE),negative=sum(GOVT_GRANTS<0,na.rm=TRUE),missing=sum(is.na(GOVT_GRANTS))),by=.(TAXYEAR,FTYPE)];r[,scenario:=label];r}
orig<-acct(raw,'original_rows');fwrite(orig,file.path(out,'reporting_original_by_year_form.csv'))
x<-unique(raw,by=c('EIN','TAXYEAR'))
l<-fread(file.path(cp,'duplicate_grant_lookup_used.csv'));l[,EIN:=as.numeric(EIN)];l[,selected_raw_grant:=as.numeric(selected_raw_grant)]
x[,unresolved:=FALSE];x[l,on=.(EIN,TAXYEAR),`:=`(GOVT_GRANTS=ifelse(i.policy=='unresolved_do_not_impute',NA_real_,i.selected_raw_grant),unresolved=i.policy=='unresolved_do_not_impute')]
x[,public_grants:=ifelse(unresolved,NA_real_,pmax(0,fcoalesce(GOVT_GRANTS,0)))];x[,private_contributions:=pmax(0,private_contributions-public_grants)]
stopifnot(nrow(x)==4673706,sum(x$unresolved)==12)
fwrite(acct(x,'selected_policy'),file.path(out,'reporting_selected_by_year_form.csv'))
both<-rbind(orig,acct(x,'selected_policy'));annual<-both[,lapply(.SD,sum),by=.(scenario,TAXYEAR),.SDcols=c('N','source_present','field_present','explicit_zero','positive','negative','missing')]
stopifnot(all(annual$field_present==annual$explicit_zero+annual$positive+annual$negative),all(annual$N==annual$field_present+annual$missing))
annual[,`:=`(field_pct=100*field_present/N,positive_pct=100*positive/N,source_pct=100*source_present/N)]
fwrite(annual,file.path(out,'reporting_annual.csv'))
fwrite(x[,.(N=.N,median_total_revenue=median(TOTREV),total_revenue=sum(TOTREV)),by=FTYPE],file.path(out,'filing_size.csv'))
sect<-function(d){d[,{
 z<-list(N=.N,markets=uniqueN(paste(CBSA,ntee_broad_clean,TAXYEAR)),unknown_private_public=sum(unresolved))
 for(s in c('private','public','client')){v<-get(switch(s,private='private_contributions',public='public_grants',client='client_services'));z[[paste0(s,'_positive_pct')]]<-100*mean(v>0,na.rm=TRUE);z[[paste0(s,'_N_observed')]]<-sum(!is.na(v));z[[paste0(s,'_mean_positive')]]<-mean(v[v>0],na.rm=TRUE);z[[paste0(s,'_total')]]<-sum(v,na.rm=TRUE)};z
 },by=ntee_broad_clean]}
fwrite(sect(x),file.path(out,'table1_sector.csv'))
cr<-x[,lapply(.SD,function(v){t<-sum(v);if(is.na(t)||t<=0)NA_real_ else sum(head(sort(v,decreasing=TRUE),4))/t}),by=.(CBSA,ntee_broad_clean,TAXYEAR),.SDcols=c('private_contributions','public_grants','client_services')];setnames(cr,names(cr)[4:6],paste0('cr4_',c('private','public','client')))
m<-readRDS(file.path(cp,'market_measures_and_coupling.rds'))[scenario=='selected_policy'];m<-merge(m,cr,by=c('CBSA','ntee_broad_clean','TAXYEAR'))
for(s in c('private','public','client'))m[is.na(get(paste0('total_',s)))|get(paste0('total_',s))<=0,(paste0('hhi_',s)):=NA_real_]
acs<-fread(file.path(root,'output/revisions/NVSQ_R1_2026-07/tables/cbsa_acs5_controls_2012_2021.csv'),select=c('CBSA','TAXYEAR','pop_total'))
m<-merge(m,acs,by=c('CBSA','TAXYEAR'),all.x=TRUE);m[,density:=n_orgs/pop_total*100000]
recent<-m[TAXYEAR>=2020 & flag_positive_revenue==1 & unresolved_orgs==0]
t2<-recent[,.(N=.N,mean_density=mean(density,na.rm=TRUE),median_orgs=as.numeric(median(n_orgs)),median_private_equiv=median(1/hhi_private,na.rm=TRUE),private_hhi=mean(hhi_private,na.rm=TRUE),public_hhi=mean(hhi_public,na.rm=TRUE),client_hhi=mean(hhi_client,na.rm=TRUE),private_cr4=mean(cr4_private,na.rm=TRUE),public_cr4=mean(cr4_public,na.rm=TRUE),client_cr4=mean(cr4_client,na.rm=TRUE),pp=mean(net_pp_pearson,na.rm=TRUE),pc=mean(net_pc_pearson,na.rm=TRUE),qc=mean(net_qc_pearson,na.rm=TRUE)),by=ntee_broad_clean];fwrite(t2,file.path(out,'table2_recent_sector.csv'))
fwrite(m[,.(pp=cor(hhi_private,hhi_public,use='complete.obs'),pc=cor(hhi_private,hhi_client,use='complete.obs'),qc=cor(hhi_public,hhi_client,use='complete.obs')),by=ntee_broad_clean],file.path(out,'table3_hhi_correlations.csv'))
fwrite(data.table(side=c('private','public','client'),correlation=sapply(c('private','public','client'),function(s)cor(m[[paste0('hhi_',s)]],m[[paste0('cr4_',s)]],use='complete.obs'))),file.path(out,'hhi_cr4_check.csv'))
m[,ntee_broad_clean:=gsub('_',' ',sub('^[^_]+_','',ntee_broad_clean))]
cols<-c(private='#294f73',public='#986c27',client='#497d64')
long<-melt(m,id.vars=c('ntee_broad_clean','n_orgs','flag_hq_public_2020_2021'),measure.vars=paste0('hhi_',names(cols)),variable.name='side',value.name='HHI');long[,side:=factor(sub('hhi_','',side),levels=c('private','public','client'))];long<-long[is.finite(HHI)]
long[,size:=factor(as.character(cut(n_orgs,c(0,1,2,4,9,24,Inf),labels=c('1','2','3-4','5-9','10-24','25+'))),levels=c('1','2','3-4','5-9','10-24','25+'))]
theme_set(theme_bw(base_size=11));saveplot<-function(p,name,w=9,h=7)ggsave(file.path(out,name),p,width=w,height=h,dpi=180,bg='white')
saveplot(ggplot(long,aes(HHI,fill=side))+geom_histogram(binwidth=.05,boundary=0)+facet_grid(ntee_broad_clean~side,scales='free_y')+scale_fill_manual(values=cols,guide='none')+labs(x='Active-side HHI',y='Market-years'), 'figure1.png',9,12)
co<-melt(m[unresolved_orgs==0],id.vars=c('ntee_broad_clean','TAXYEAR'),measure.vars=c('net_pp_pearson','net_pc_pearson','net_qc_pearson'),variable.name='pair',value.name='correlation');co[,pair:=factor(pair,levels=c('net_pp_pearson','net_pc_pearson','net_qc_pearson'),labels=c('Private-public','Private-client','Public-client'))];co<-co[is.finite(correlation)]
saveplot(ggplot(co,aes(correlation))+geom_histogram(binwidth=.1,boundary=-1,fill='#294f73')+facet_wrap(~pair,ncol=1)+labs(x='Within-market Pearson correlation',y='Market-years'),'figure2.png',7,8)
saveplot(ggplot(co[TAXYEAR>=2020,.(mean=mean(correlation)),by=.(ntee_broad_clean,pair)],aes(mean,ntee_broad_clean,color=pair))+geom_point(position=position_dodge(width=.5),size=2)+labs(x='Mean Pearson correlation, 2020-21',y=NULL,color='Revenue pair')+theme(legend.position='bottom'),'figure3.png',8,6)
saveplot(ggplot(long,aes(HHI,fill=side))+geom_histogram(binwidth=.05,boundary=0)+facet_grid(size~side,scales='free_y')+scale_fill_manual(values=cols,guide='none')+labs(x='Active-side HHI',y='Market-years'),'appendix_b1.png',9,9)
bc<-function(x){n<-length(x);z<-x-mean(x);v<-mean(z^2);if(n<4||v==0)return(NA_real_);g1<-mean(z^3)/v^1.5;g2<-mean(z^4)/v^2-3;G1<-g1*sqrt(n*(n-1))/(n-2);G2<-((n+1)*g2+6)*(n-1)/((n-2)*(n-3));(G1^2+1)/(G2+3*(n-1)^2/((n-2)*(n-3)))}
tests<-function(z){list(N=length(z),BC=bc(z),dip_p=if(length(z)>=10&&sd(z)>1e-8)dip.test(z)$p.value else NA_real_,boundary_pct=100*mean(z>=.9999))}
fwrite(long[,tests(HHI),by=.(side,size)],file.path(out,'bimodality_by_size.csv'));fwrite(long[,tests(HHI),by=.(side,ntee_broad_clean)],file.path(out,'bimodality_by_sector.csv'))
boundary<-long[,.(pct=100*mean(HHI>=.9999)),by=.(size,side)]
saveplot(ggplot(boundary,aes(size,pct,group=side,color=side))+geom_line()+geom_point()+labs(x='Organizations in market',y='Active markets with HHI near 1 (%)',color='Revenue side')+theme(legend.position='bottom'),'appendix_b2.png',7,5)
# Additional health comparisons preserve the existing R1 sensitivity subject.
ds<-readRDS(file.path(gp,'analysis_samples.rds'));d<-copy(ds$selected_policy)
health<-list()
for(samp in c('health_only','excluding_health'))for(spec in c('active_joint','activity_indicators')){
 a<-copy(d[if(samp=='health_only') ntee_broad_clean=='EFGH_Health' else ntee_broad_clean!='EFGH_Health'])
 rhs<-c('lag_hhi_private','lag_hhi_public','lag_hhi_client','log_lag_orgs','log_pop','log_mhi_real_2022','poverty_rate','unemployment_rate')
 a[,lag_hhi_private:=lag_hhi_private_net0]
 for(s in names(cols)){v<-paste0('lag_hhi_',s);active<-a[[paste0('lag_total_',s)]]>0;if(spec=='active_joint')a[!active,(v):=NA_real_] else {mu<-mean(a[[v]][active]);a[,(v):=ifelse(active,get(v)-mu,0)];ind<-paste0('inactive_',s);a[,(ind):=as.integer(!active)];rhs<-c(rhs,ind)}}
 a<-a[complete.cases(a[,..rhs])];if(nrow(a)<30)stop('Unexpected health sample; inspect sector labels')
 fit<-feols(as.formula(paste('net_growth ~',paste(rhs,collapse='+'),'| CBSA + ntee_broad_clean + TAXYEAR')),data=a,cluster=~CBSA,notes=FALSE)
 ct<-as.data.table(coeftable(fit),keep.rownames='term');setnames(ct,names(ct)[2:5],c('estimate','se','statistic','p'));ct[,`:=`(sample=samp,specification=spec,N=nobs(fit))];health[[length(health)+1]]<-ct
}
fwrite(rbindlist(health),file.path(out,'health_growth.csv'))
fwrite(data.table(N_orgyears=nrow(x),N_EIN=uniqueN(x$EIN),N_CBSA=uniqueN(x$CBSA),N_markets=nrow(m),positive_markets=sum(m$flag_positive_revenue==1),unresolved_markets=sum(m$unresolved_orgs>0)),file.path(out,'sample_counts.csv'))
writeLines(capture.output(sessionInfo()),file.path(out,'session_info.txt'));writeLines('Completed reporting accounting, selected-policy sector tables, corrected figures, and health models.',file.path(out,'SUCCESS.txt'));cat('OUTPUT:',out,'\n');print(annual);print(t2)




# Reconcile R1 and R2 density from the repo's original/selected market outputs.
br <- readRDS(file.path(cp,'market_measures_and_coupling.rds'))
br <- merge(br,acs,by=c('CBSA','TAXYEAR'),all.x=TRUE)
br[,density:=n_orgs/pop_total*100000]
bh <- br[TAXYEAR>=2020 & ntee_broad_clean=='IJKLMNOP_Human_Services']
fwrite(bh[,.(markets=.N,density_N=sum(is.finite(density)),mean_density=mean(density,na.rm=TRUE),orgs=sum(n_orgs)),by=.(scenario,flag_positive_revenue,unresolved_orgs)],file.path(out,'density_bridge_groups.csv'))
fwrite(bh[,.(scenario,CBSA,ntee_broad_clean,TAXYEAR,n_orgs,pop_total,density,flag_positive_revenue,unresolved_orgs)],file.path(out,'density_bridge_markets.csv'))
print(bh[flag_positive_revenue==1,.(N=.N,mean_density=mean(density,na.rm=TRUE),orgs=sum(n_orgs)),by=scenario])
