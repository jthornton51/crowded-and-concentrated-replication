# Corrected R2 coupling, common-market comparisons and filing-policy sensitivity.
# Run: Rscript --vanilla SCRIPT REPO POLICY_DIRECTORY COVERAGE_DIRECTORY
# No production or R1 file is overwritten. No normal-tail permutation claims.
suppressPackageStartupMessages(library(data.table))
setDTthreads(4L)
args <- commandArgs(TRUE); stopifnot(length(args)==3)
ROOT <- normalizePath(args[1],winslash="/",mustWork=TRUE)
POLICY <- normalizePath(args[2],winslash="/",mustWork=TRUE)
COVERAGE <- normalizePath(args[3],winslash="/",mustWork=TRUE)
stopifnot(file.exists(file.path(POLICY,"SUCCESS.txt")),file.exists(file.path(COVERAGE,"SUCCESS.txt")))
OUT <- file.path(ROOT,"output/revisions/NVSQ_R2_2026-09/rebuild",paste0("coupling_",format(Sys.time(),"%Y%m%d_%H%M%S")))
stopifnot(!dir.exists(OUT));dir.create(OUT,recursive=TRUE)
logcon<-file(file.path(OUT,"run_log.txt"),"wt");sink(logcon,split=TRUE)
KEY<-c("CBSA","ntee_broad_clean","TAXYEAR");manifest<-list()
read <- function(path,select=NULL) {
  stopifnot(file.exists(path));fi<-file.info(path)
  manifest[[length(manifest)+1L]]<<-data.table(path,bytes=fi$size,modified=as.character(fi$mtime))
  fread(path,select=select,showProgress=FALSE,na.strings=c("","NA"))
}
unique_key<-function(d,k=KEY)stopifnot(!anyNA(d[,..k]),!anyDuplicated(d[,..k]))
safe_cor<-function(a,b){ok<-is.finite(a)&is.finite(b);if(sum(ok)<2L)return(NA_real_);a<-a[ok]-mean(a[ok]);b<-b[ok]-mean(b[ok]);den<-sqrt(sum(a*a)*sum(b*b));if(den==0)return(NA_real_);max(-1,min(1,sum(a*b)/den))}
metrics<-function(a,b,z){
  r<-safe_cor(a,b);rz1<-safe_cor(a,z);rz2<-safe_cor(b,z)
  partial<-if(length(a)>=4L && all(is.finite(c(r,rz1,rz2))) && abs(rz1)<=.995 && abs(rz2)<=.995)
    max(-1,min(1,(r-rz1*rz2)/sqrt((1-rz1^2)*(1-rz2^2)))) else NA_real_
  pos<-a>0 & b>0
  c(pearson=r,spearman=safe_cor(rank(a,ties.method="average"),rank(b,ties.method="average")),partial=partial,
    participation_phi=safe_cor(as.numeric(a>0),as.numeric(b>0)),
    positive_spearman=if(sum(pos)>=3L)safe_cor(rank(a[pos]),rank(b[pos])) else NA_real_,n_joint_positive=sum(pos))
}
summarize_markets<-function(org,label){
  cat("Constructing market measures:",label,"rows",nrow(org),"\n")
  org[,net:=pmax(0,gross-government)]
  x<-org[,{
    unknown<-anyNA(government)
    vals<-list(n_orgs=.N,total_gross=sum(gross),total_private=sum(net),total_public=sum(government),total_client=sum(client),
       reporting_pct=100*mean(reported),strict_revenue_pct=if(sum(gross)>0)100*sum(gross[strict])/sum(gross) else NA_real_,
       unresolved_orgs=sum(is.na(government)),period_unverified_orgs=sum(period_unverified),
       floored_orgs=sum(government>gross,na.rm=TRUE),floor_dollars=sum(pmax(government-gross,0),na.rm=TRUE))
    for(side in c("gross","private","public","client")){
      a<-switch(side,gross=gross,private=net,public=government,client=client)
      total<-sum(a)
      vals[[paste0("hhi_",side)]]<-if(is.na(total))NA_real_ else if(total>0)sum((a/total)^2) else 0
    }
    z<-log1p(pmax(TOTREV,0))
    for(arm in c("gross","net"))for(pair in c("pp","pc","qc")){
      a<-if(pair=="qc")government else if(arm=="gross")gross else net
      b<-if(pair=="pp")government else client
      stats<-if(unknown)setNames(rep(NA_real_,6),c("pearson","spearman","partial","participation_phi","positive_spearman","n_joint_positive")) else metrics(a,b,z)
      for(nm in names(stats))vals[[paste(arm,pair,nm,sep="_")]]<-unname(stats[nm])
    }
    vals
  },by=KEY]
  x[,scenario:=label];unique_key(x);x
}
paired_summary<-function(d,cols,label,scenario,measure,B=999L){
  ok<-complete.cases(d[,..cols]);x<-d[ok]
  if(nrow(x)==0L)return(data.table())
  a<-as.matrix(x[,..cols]);gap0<-a[,1]-a[,2];gap1<-a[,3]-a[,4]
  v<-data.table(CBSA=x$CBSA,gap_gross=gap0,gap_net=gap1,gap_change=gap1-gap0)
  cluster<-v[,.(n=.N,gap_gross=sum(gap_gross),gap_net=sum(gap_net),gap_change=sum(gap_change)),by=CBSA]
  means<-colMeans(a)
  result<-data.table(scenario,sample=label,measure,N=nrow(x),CBSAs=nrow(cluster),
      gross_pp=means[1],gross_pc=means[2],net_pp=means[3],net_pc=means[4],
      gap_gross=mean(gap0),gap_net=mean(gap1),gap_change=mean(gap1-gap0))
  if(nrow(cluster)>=2L){
    set.seed(20260916L)
    boot<-replicate(B,{i<-sample.int(nrow(cluster),replace=TRUE);colSums(as.matrix(cluster[i,.(gap_gross,gap_net,gap_change)]))/sum(cluster$n[i])})
    for(j in 1:3){nm<-c("gap_gross","gap_net","gap_change")[j];result[[paste0(nm,"_se")]]<-sd(boot[j,]);result[[paste0(nm,"_lo")]]<-unname(quantile(boot[j,],.025));result[[paste0(nm,"_hi")]]<-unname(quantile(boot[j,],.975))}
  }
  result
}
main<-function(){
  cat("Output:",OUT,"\n")
  raw<-read(file.path(ROOT,"data/processed/pz_constituency_measures.csv"),c("EIN",KEY,"private_contributions","public_grants","client_services","TOTREV","GOVT_GRANTS"))
  setnames(raw,c("private_contributions","public_grants","client_services"),c("gross","government","client"))
  for(v in c("gross","government","client","TOTREV","GOVT_GRANTS"))set(raw,j=v,value=as.numeric(raw[[v]]))
  raw<-raw[TAXYEAR%in%2012:2021];raw[,EIN:=as.numeric(EIN)]
  stopifnot(nrow(raw)==4720874L,all(is.finite(raw$gross)),all(is.finite(raw$government)),all(is.finite(raw$client)),all(raw$gross>=0),all(raw$government>=0),all(raw$client>=0))
  raw[,`:=`(reported=!is.na(GOVT_GRANTS),period_unverified=FALSE)]
  dk<-read(file.path(COVERAGE,"source_determinability_keys.csv"),c("EIN","TAXYEAR","cls"));unique_key(dk,c("EIN","TAXYEAR"))
  raw[,strict:=FALSE];raw[dk,on=.(EIN,TAXYEAR),strict:=startsWith(i.cls,"determinable")]
  # Validate all attributes retained while collapsing repeated organization keys.
  attrs<-c("EIN","TAXYEAR","CBSA","ntee_broad_clean","gross","client","TOTREV")
  invariant<-unique(raw[,..attrs]);bad<-invariant[,.N,by=.(EIN,TAXYEAR)][N>1]
  fwrite(bad,file.path(OUT,"conflicting_panel_attributes.csv"));stopifnot(nrow(bad)==0)
  selected<-unique(raw,by=c("EIN","TAXYEAR"))
  lookup<-read(file.path(POLICY,"duplicate_grant_lookup.csv"));lookup[,EIN:=as.numeric(EIN)]
  lookup[,selected_raw_grant:=as.numeric(selected_raw_grant)]
  unique_key(lookup,c("EIN","TAXYEAR"));stopifnot(nrow(lookup)==46158L,nrow(selected)==4673706L)
  stopifnot(nrow(selected[lookup,on=.(EIN,TAXYEAR),nomatch=0L])==nrow(lookup))
  lookup[,new_grant:=ifelse(policy=="unresolved_do_not_impute",NA_real_,pmax(0,fcoalesce(as.numeric(selected_raw_grant),0)))]
  selected[lookup,on=.(EIN,TAXYEAR),`:=`(government=i.new_grant,reported=!is.na(i.selected_raw_grant)&i.policy!="unresolved_do_not_impute",period_unverified=i.policy=="retain_common_amount_period_unverified")]
  stopifnot(sum(is.na(selected$government))==12L)
  file.copy(file.path(POLICY,"duplicate_grant_lookup.csv"),file.path(OUT,"duplicate_grant_lookup_used.csv"))
  original_m<-rbindlist(lapply(2012:2021,function(y){cat("Original year",y,"\n");summarize_markets(raw[TAXYEAR==y],"original_rows")}))
  saveRDS(original_m,file.path(OUT,"original_market_checkpoint.rds"))
  cert<-read(file.path(ROOT,"data/processed/market_level_summaries.csv"),c(KEY,"n_orgs","hhi_private","hhi_public","hhi_client","total_private","total_public","total_client"));unique_key(cert)
  ref<-merge(original_m,cert,by=KEY,suffixes=c("","_cert"));stopifnot(nrow(ref)==nrow(cert))
  checks<-data.table(quantity=c("n_orgs","hhi_gross","hhi_public","hhi_client","total_gross","total_public","total_client"),max_abs_diff=c(max(abs(ref$n_orgs-ref$n_orgs_cert)),max(abs(ref$hhi_gross-ref$hhi_private_cert)),max(abs(ref$hhi_public-ref$hhi_public_cert)),max(abs(ref$hhi_client-ref$hhi_client_cert)),max(abs(ref$total_gross-ref$total_private_cert)),max(abs(ref$total_public-ref$total_public_cert)),max(abs(ref$total_client-ref$total_client_cert))))
  fwrite(checks,file.path(OUT,"reference_market_validation.csv"));stopifnot(all(checks[grepl("hhi",quantity)]$max_abs_diff<1e-9),all(checks[!grepl("hhi",quantity)]$max_abs_diff<1))
  legacy<-read(file.path(ROOT,"output/revisions/NVSQ_R1_2026-07/tables/coupling_validation_appendix.csv"))
  coupling_validation<-rbindlist(lapply(c("pearson","spearman","partial"),function(measure)rbindlist(lapply(c("pp","pc","qc"),function(pair){
    # Explicit vectors below avoid data.table column/local-name ambiguity.
    pair_name<-switch(pair,pp="private_public",pc="private_client",qc="public_client")
    metric_name<-measure
    expected<-legacy[legacy$sample=="full panel" & legacy$pair==pair_name & legacy$measure==metric_name]
    z<-original_m[[paste("gross",pair,measure,sep="_")]];z<-z[is.finite(z)]
    stopifnot(nrow(expected)==1L,length(z)==expected$N_markets,abs(mean(z)-expected$mean)<.00051)
    data.table(pair=pair_name,measure,N=length(z),mean=mean(z),reference_mean=expected$mean)
  }))))
  fwrite(coupling_validation,file.path(OUT,"reference_coupling_validation.csv"));cat("R1 Pearson, rank and partial correlations reproduced.\n")
  selected_m<-rbindlist(lapply(2012:2021,function(y){cat("Selected year",y,"\n");summarize_markets(selected[TAXYEAR==y],"selected_policy")}))
  saveRDS(selected_m,file.path(OUT,"selected_market_checkpoint.rds"))
  # Independent check against the earlier gross selected-policy market reconstruction.
  old<-read(file.path(POLICY,"selected_market_measures.csv"));unique_key(old)
  z<-merge(selected_m,old,by=KEY,suffixes=c("","_old"));stopifnot(nrow(z)==77489L,all(z$n_orgs==z$n_orgs_old),sum(z$unresolved_orgs>0)==11L)
  stopifnot(max(abs(z$hhi_gross-z$hhi_private_old),na.rm=TRUE)<1e-9,max(abs(z$hhi_public-z$hhi_public_old),na.rm=TRUE)<1e-9,max(abs(z$hhi_client-z$hhi_client_old),na.rm=TRUE)<1e-9)
  fwrite(data.table(check="selected-policy gross market reconstruction",N=nrow(z),unresolved_markets=sum(z$unresolved_orgs>0),status="PASS"),file.path(OUT,"selected_policy_validation.csv"))
  reporter_m<-summarize_markets(selected[reported==TRUE & !is.na(government)],"selected_reporters_only")
  mk<-rbindlist(list(original_m,selected_m,reporter_m));rm(raw,selected,invariant);invisible(gc())
  sf<-read(file.path(ROOT,"output/revisions/NVSQ_R1_2026-07/tables/sample_flags.csv"));unique_key(sf)
  mk<-merge(mk,sf[,c(KEY,"flag_positive_revenue","flag_hq_coupling_pp","flag_hq_public_2020_2021"),with=FALSE],by=KEY,all.x=TRUE)
  stopifnot(!anyNA(mk$flag_positive_revenue))
  saveRDS(mk,file.path(OUT,"market_measures_and_coupling.rds"))
  fwrite(mk[,.(n_orgs=sum(n_orgs),market_years=.N,unknown_markets=sum(unresolved_orgs>0),floored_orgs=sum(floored_orgs),floor_dollars=sum(floor_dollars),period_unverified_markets=sum(period_unverified_orgs>0)),by=scenario],file.path(OUT,"scenario_audit.csv"))
  rows<-list();unpaired<-list()
  for(sc in unique(mk$scenario)){
    # High-reporting samples: 2020-2021 markets in which at least 20% of organizations report the grant
    # field (reporting_pct). They replace the R1 flag_hq_coupling_pp samples, which selected on positive
    # grant receipt rather than on reporting.
    x<-mk[scenario==sc];samples<-list(full=rep(TRUE,nrow(x)),n_ge_10=x$n_orgs>=10,years_2020_21=x$TAXYEAR>=2020,high_reporting_2020_21=x$TAXYEAR>=2020 & x$reporting_pct>=20,high_reporting_2020_21_n_ge_10=x$TAXYEAR>=2020 & x$reporting_pct>=20 & x$n_orgs>=10,strict_revenue_95=x$strict_revenue_pct>=95,strict_revenue_100=x$strict_revenue_pct>=100-1e-9)
    for(lab in names(samples))for(meas in c("pearson","spearman","partial","participation_phi","positive_spearman")){
      d<-x[which(samples[[lab]])]
      cols<-paste(rep(c("gross","net"),each=2),rep(c("pp","pc"),2),meas,sep="_")
      rows[[length(rows)+1L]]<-paired_summary(d,cols,lab,sc,meas)
      for(col in cols){v<-d[[col]];ok<-is.finite(v);unpaired[[length(unpaired)+1L]]<-data.table(scenario=sc,sample=lab,metric=col,N=sum(ok),mean=if(any(ok))mean(v[ok]) else NA_real_)}
    }
  }
  fwrite(rbindlist(rows,fill=TRUE),file.path(OUT,"paired_coupling.csv"));fwrite(rbindlist(unpaired),file.path(OUT,"available_sample_means.csv"))
  # Corrected original-vs-selected policy comparison on exactly common market keys.
  common<-merge(mk[scenario=="original_rows"],mk[scenario=="selected_policy"],by=KEY,suffixes=c("_original","_selected"))
  policy_rows<-list()
  for(meas in c("pearson","spearman","partial")){
    cols<-paste0(paste("net",rep(c("pp","pc"),2),meas,sep="_"),rep(c("_original","_selected"),each=2))
    policy_rows[[meas]]<-paired_summary(common,cols,"common_original_selected","policy_comparison",meas)
  }
  fwrite(rbindlist(policy_rows,fill=TRUE),file.path(OUT,"common_policy_coupling.csv"))
  fwrite(rbindlist(manifest),file.path(OUT,"input_manifest.csv"));writeLines(capture.output(sessionInfo()),file.path(OUT,"session_info.txt"))
  file.copy(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),file.path(OUT,"script_snapshot.R"))
  writeLines(c("Completed: R1 correlation and market baselines reproduced; selected gross markets independently reproduced.","Paired gaps use common eligible markets within each measure, 999 CBSA cluster bootstrap draws, percentile intervals.","Policy-comparison gross/net columns denote ORIGINAL/SELECTED corrected quantities respectively.","High-reporting samples require at least 20% of organizations to report the grant field, 2020-2021; R1 HQ flags are kept only for the R1 reference check in r2_13. Strict coverage is a conservative EIN/year audit, not exact filing linkage.","Reporter-only results describe observed reporters, not full markets. Positive-recipient ranks describe a selected subset.","No permutation-tail, top-overlap, causal, or equivalence claim is made. Unknown grants are not inactivity."),file.path(OUT,"SUCCESS.txt"))
  cat("SUCCESS:",OUT,"\n")
}
tryCatch(main(),error=function(e){writeLines(conditionMessage(e),file.path(OUT,"ERROR.txt"));stop(e)},finally={sink();close(logcon)})
