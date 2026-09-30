# Run: Rscript --vanilla SCRIPT REPO COUPLING_DIR GROWTH_DIR OUTPUT_PARENT
# Additive tables for net-private / selected-filing revision; no production writes.
suppressPackageStartupMessages({library(data.table); library(diptest)})
a <- commandArgs(TRUE); stopifnot(length(a)==4)
root <- normalizePath(a[1],winslash="/",mustWork=TRUE)
cp <- normalizePath(a[2],winslash="/",mustWork=TRUE)
gp <- normalizePath(a[3],winslash="/",mustWork=TRUE)
stopifnot(file.exists(file.path(cp,"SUCCESS.txt")),file.exists(file.path(gp,"SUCCESS.txt")))
out <- file.path(a[4],paste0("combined_tables_",format(Sys.time(),"%Y%m%d_%H%M%S")))
stopifnot(!dir.exists(out)); dir.create(out,recursive=TRUE)
m <- readRDS(file.path(cp,"market_measures_and_coupling.rds"))
key <- c("scenario","CBSA","ntee_broad_clean","TAXYEAR")
stopifnot(!anyDuplicated(m[,..key]))
sides <- c("private","public","client")
# Preserve unknown markets; zero revenue means inactive, never HHI zero here.
for(s in sides){v<-paste0("hhi_",s);t<-paste0("total_",s);m[is.na(get(t))|get(t)<=0,(v):=NA_real_]}
stopifnot(m[scenario=="selected_policy" & unresolved_orgs>0,all(is.na(hhi_private)&is.na(hhi_public))])
m[,size:=as.character(cut(n_orgs,c(0,1,2,4,9,24,Inf),labels=c("1","2","3-4","5-9","10-24","25+")))]
long <- melt(m,id.vars=c(key,"n_orgs","size","reporting_pct","unresolved_orgs"),measure.vars=paste0("hhi_",sides),variable.name="side",value.name="hhi")
long[,side:=sub("hhi_","",side)]
describe <- function(d,by){d[,.(N_markets=.N,N_active=sum(is.finite(hhi)),mean_hhi=mean(hhi,na.rm=TRUE),median_hhi=median(hhi,na.rm=TRUE),sd_hhi=sd(hhi,na.rm=TRUE)),by=by]}
overall<-describe(long,c("scenario","side"));fwrite(overall,file.path(out,"descriptives_overall.csv"))
for(g in c("TAXYEAR","ntee_broad_clean","size"))fwrite(describe(long,c("scenario",g,"side")),file.path(out,paste0("descriptives_by_",g,".csv")))
# Same finite-sample BC formula and diagnostic thresholds as the R1 battery.
bc<-function(x){n<-length(x);z<-x-mean(x);m2<-mean(z^2);if(n<4||m2==0)return(NA_real_);g1<-mean(z^3)/m2^1.5;g2<-mean(z^4)/m2^2-3;G1<-g1*sqrt(n*(n-1))/(n-2);G2<-((n+1)*g2+6)*(n-1)/((n-2)*(n-3));(G1^2+1)/(G2+3*(n-1)^2/((n-2)*(n-3)))}
test<-function(x){x<-x[is.finite(x)];n<-length(x);boundary<-if(n)100*mean(x>=.9999) else NA_real_;b<-bc(x);p<-if(n>=10&&sd(x)>1e-8)dip.test(x)$p.value else NA_real_;data.table(N=n,mean_hhi=mean(x),pct_hhi_eq1=boundary,BC=b,dip_p=p,diagnostic=if(!is.finite(p))"degenerate" else if(p<.05&&b>.555){if(boundary>=10)"boundary-sensitive" else "both tests positive"}else if(p<.05||b>.555)"mixed" else "weak")}
bims<-list()
for(sc in unique(m$scenario)){
 d<-long[scenario==sc]
 cuts<-list(full=rep(TRUE,nrow(d)),n_ge5=d$n_orgs>=5,n_ge10=d$n_orgs>=10,recent_2020_21=d$TAXYEAR>=2020,high_reporting_2020_21_n_ge10=d$TAXYEAR>=2020&d$reporting_pct>=20&d$n_orgs>=10)
 for(cut in names(cuts)){r<-d[which(cuts[[cut]]),test(hhi),by=side];r[,`:=`(scenario=sc,sample=cut)];bims[[length(bims)+1]]<-r}
}
bim<-rbindlist(bims);fwrite(bim,file.path(out,"bimodality.csv"))
# Appendix Table B2. Reporting sensitivity, selected policy.
# Panel A: mean active-side HHI by sector and stream in 2020-2021 (Table 2A population) at increasing
# thresholds for the share of organizations that report the grant field.
rec<-m[scenario=="selected_policy"&TAXYEAR>=2020&flag_positive_revenue==1&unresolved_orgs==0]
thr<-rbindlist(lapply(c(0,20,30,40,50),function(k){r<-rec[reporting_pct>=k,.(market_years=.N,private=mean(hhi_private,na.rm=TRUE),public=mean(hhi_public,na.rm=TRUE),client=mean(hhi_client,na.rm=TRUE)),by=ntee_broad_clean];r[,`:=`(min_reporting_pct=k,private_lowest=private<public&private<client)];r}))
setcolorder(thr,c("min_reporting_pct","ntee_broad_clean"));fwrite(thr,file.path(out,"reporting_threshold_concentration.csv"))
# Panel B: share of active public-side HHIs at the boundary, 2020-2021 markets with at least ten organizations.
pb<-long[scenario=="selected_policy"&side=="public"&TAXYEAR>=2020&n_orgs>=10&is.finite(hhi)]
pb[,reporting_group:=fifelse(reporting_pct>=20,"at least 20% report","below 20% report")]
bnd<-rbind(pb[,.(market_years=.N,pct_hhi_eq1=100*mean(hhi>=.9999)),by=reporting_group],pb[,.(reporting_group="all",market_years=.N,pct_hhi_eq1=100*mean(hhi>=.9999))])
fwrite(bnd,file.path(out,"public_boundary_by_reporting.csv"))
# Headline population and active-side composition follow r2_13 exactly.
acs<-fread(file.path(root,"output/revisions/NVSQ_R1_2026-07/tables/cbsa_acs5_controls_2012_2021.csv"),select=c("CBSA","TAXYEAR","pop_total"))
stopifnot(!anyDuplicated(acs[,.(CBSA,TAXYEAR)]))
h<-merge(m[flag_positive_revenue==1&unresolved_orgs==0],acs,by=c("CBSA","TAXYEAR"))
h[,density:=100000*n_orgs/pop_total]
h[,n_active:=rowSums(is.finite(as.matrix(.SD))),.SDcols=paste0("hhi_",sides)]
h[,active_mix:=paste0(ifelse(is.finite(hhi_private),"private", ""),ifelse(is.finite(hhi_public),"+public",""),ifelse(is.finite(hhi_client),"+client",""))]
h[,active_mix:=sub("^\\+","",active_mix)]
h[,composite:=rowMeans(.SD,na.rm=TRUE),.SDcols=paste0("hhi_",sides)]
h<-h[is.finite(composite)&is.finite(density)]
mix<-h[,.(N=.N),by=.(scenario,n_active,active_mix)];mix[,pct:=100*N/sum(N),by=scenario];fwrite(mix,file.path(out,"active_side_composition.csv"))
head<-rbindlist(lapply(c("active_mean","all_three"),function(arm){d<-if(arm=="all_three")h[n_active==3] else h;r<-d[,.(N=.N,r=cor(density,composite),pct_high_high=100*mean(density>median(density)&composite>median(composite)),benchmark=100*mean(density>median(density))*mean(composite>median(composite))),by=scenario];r[,composite:=arm];r}))
ref<-fread(file.path(gp,"combined_correction_headlines.csv"));check<-merge(head,ref,by=c("scenario","composite"),suffixes=c("_new","_ref"));stopifnot(nrow(check)==6)
for(v in c("N","r","pct_high_high","benchmark"))stopifnot(all(abs(check[[paste0(v,"_new")]]-check[[paste0(v,"_ref")]])<1e-9))
fwrite(check,file.path(out,"headline_validation.csv"));fwrite(head,file.path(out,"headline.csv"))
pair<-fread(file.path(cp,"paired_coupling.csv"))
# Include all sample labels in source export; table below highlights the full sample.
fwrite(pair,file.path(out,"concordance_all_samples.csv"))
ct<-fread(file.path(gp,"coefficients.csv"));ns<-fread(file.path(gp,"model_samples.csv"))
ids<-c("selected_policy__full__net_active_joint__HHI_only","selected_policy__full__net_activity_indicators__HHI_only","selected_policy__full__net_activity_indicators__CBSA_sector_FE","selected_policy__common_coupling__net_active_joint__joint")
growth<-merge(ct[model%in%ids&term%in%c(paste0("lag_hhi_",sides),"lag_private_public_coupling")],ns[,.(model,estimated_N,fixed_effects)],by="model")
stopifnot(uniqueN(growth$model)==4,nrow(growth)==13)
fwrite(growth,file.path(out,"growth_display.csv"))
model_labels<-setNames(c("All sides active","Activity indicators","Activity indicators: CBSA×sector FE","All sides active + concordance"),ids)
term_labels<-setNames(c("Private HHI","Public HHI","Client HHI","Private–public concordance"),c(paste0("lag_hhi_",sides),"lag_private_public_coupling"))
growth[,`:=`(model=unname(model_labels[model]),term=unname(term_labels[term]))]
fmt<-function(x,d=3)formatC(x,format="f",digits=d,big.mark=",")
pv<-function(x)ifelse(x<.001,"<.001",fmt(x,3))
md<-function(d){c(paste0("| ",paste(names(d),collapse=" | ")," |"),paste0("| ",paste(rep("---",ncol(d)),collapse=" | ")," |"),apply(d,1,function(r)paste0("| ",paste(r,collapse=" | ")," |")))}
doc<-c("# R2 corrected comparison tables — 2026-09-16","","Generated from completed rebuild outputs. Private contributions exclude government grants; undefined concentration on inactive sides is omitted from active-side summaries. Selected filing results are a documented sensitivity policy, not certified exact-return linkage. These tables are for draft integration; R1 files are unchanged.","","## 1. Density and concentration","",md(head[,.(Scenario=scenario,Composite=composite,N=fmt(N,0),Correlation=fmt(r),`Above both medians (%)`=fmt(pct_high_high,2),`Independence benchmark (%)`=fmt(benchmark,2))]),"","Each row uses its own eligible population and medians. All-three-active rows change both composition and population. Reporter-only rows describe reporting organizations. Selected full-population composites exclude the 11 unresolved market-years.","","## 2. Selected-policy active-side concentration","",md(overall[scenario=="selected_policy",.(Side=side,`All markets`=fmt(N_markets,0),`Active, observed markets`=fmt(N_active,0),Mean=fmt(mean_hhi),Median=fmt(median_hhi),SD=fmt(sd_hhi))]),"","Each side uses its own active observed sample across all reconstructed markets, before the headline ACS/positive-revenue restriction. Missing public/private values in unresolved markets are not coded as inactivity.","","## 3. Distributional diagnostics","",md(bim[scenario=="selected_policy"&sample%in%c("n_ge10","high_reporting_2020_21_n_ge10"),.(Sample=sample,Side=side,N=fmt(N,0),BC=fmt(BC),`Dip p`=pv(dip_p),`HHI near 1 (%)`=fmt(pct_hhi_eq1,2),Diagnostic=diagnostic)]),"","Dip tests assess unimodality; rejection does not establish exactly two modes or a mechanism. BC > .555 is a descriptive diagnostic. The inherited boundary flag marks at least 10% of observations at HHI >= .9999; it signals a possible mechanical contribution, not proof that all multimodality is mechanical. The high-reporting sample requires at least 20% of organizations to report the grant field in 2020-2021.","","## 4. Full selected-policy paired concordance","",md(pair[scenario=="selected_policy"&sample=="full"&measure%in%c("pearson","spearman","partial"),.(Measure=measure,N=fmt(N,0),`Private–public`=fmt(net_pp),`Private–client`=fmt(net_pc),Gap=fmt(gap_net),`95% CI`=paste0("[",fmt(gap_net_lo),", ",fmt(gap_net_hi),"]"))]),"","Gap = private–public minus private–client, paired on common eligible markets within measure. Partial correlations control for log(1 + organizational total revenue). Intervals use 999 CBSA-cluster bootstrap draws; samples differ across measures. Full sensitivity estimates are in concordance_all_samples.csv.","","## 5. Selected-policy net-growth models","",md(growth[,.(Model=model,Term=term,N=fmt(estimated_N,0),Estimate=fmt(estimate,5),SE=fmt(se,5),p=pv(p),`95% CI`=paste0("[",fmt(ci_low,5),", ",fmt(ci_high,5),"]"))]),"","Outcome is proportional net organizational growth; multiply a coefficient by 100 to express a full-unit HHI change in percentage points. Predictors are consecutive-year lags. SEs cluster by CBSA. Models include lagged organization count and demographic controls; the CBSA-sector sensitivity uses CBSA×sector and year fixed effects, while other displayed models use CBSA, sector, and year fixed effects. Activity-indicator coefficients describe concentration among active sides, with separate inactivity indicators. These are descriptive associations, not causal effects or gross entry rates.","","## 6. Selected-policy headline sample: active-side composition","",md(mix[scenario=="selected_policy",.(`Active sides`=n_active,Composition=active_mix,N=fmt(N,0),Percent=fmt(pct,2))]),"","## Provenance","",paste0("Concordance input: `",cp,"`."),"",paste0("Growth input: `",gp,"`."),"","Headline results independently reproduce all six saved r2_13 headline rows within 1e-9. Output includes unrounded machine-readable tables, source-script snapshot, input hashes, and session information.")
writeLines(doc,file.path(out,"R2_COMPARISON_TABLES.md"),useBytes=TRUE)
files<-c(file.path(cp,"market_measures_and_coupling.rds"),file.path(cp,"paired_coupling.csv"),file.path(gp,c("coefficients.csv","model_samples.csv","combined_correction_headlines.csv")))
fwrite(data.table(path=files,md5=unname(tools::md5sum(files))),file.path(out,"input_manifest.csv"))
script<-sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE));file.copy(script,file.path(out,"script_snapshot.R"))
writeLines(capture.output(sessionInfo()),file.path(out,"session_info.txt"))
writeLines(c("Completed combined-policy descriptive checks and generated tables.","Six headline rows reproduced within 1e-9.",format(Sys.time())),file.path(out,"SUCCESS.txt"))
print(overall[scenario=="selected_policy"]);print(bim[scenario=="selected_policy"&sample%in%c("n_ge10","high_reporting_2020_21_n_ge10")]);print(bnd);print(thr[,.(sectors=.N,private_lowest=sum(private_lowest)),by=min_reporting_pct]);cat("OUTPUT:",out,"\n")
