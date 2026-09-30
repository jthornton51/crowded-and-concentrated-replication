# R2 net-growth comparisons using r2_12 corrected concordance and filing scenarios.
# Run: Rscript --vanilla SCRIPT REPO COMPLETED_COUPLING_DIRECTORY
# Includes R1 coefficient certification, common samples, lag-year reporting checks.
suppressPackageStartupMessages({library(data.table);library(fixest)})
setDTthreads(4L);setFixest_nthreads(4L)
args<-commandArgs(TRUE);stopifnot(length(args)==2L)
ROOT<-normalizePath(args[1],winslash="/",mustWork=TRUE)
INPUT<-normalizePath(args[2],winslash="/",mustWork=TRUE)
stopifnot(file.exists(file.path(INPUT,"SUCCESS.txt")))
OUT<-file.path(ROOT,"output/revisions/NVSQ_R2_2026-09/rebuild",paste0("net_growth_",format(Sys.time(),"%Y%m%d_%H%M%S")))
stopifnot(!dir.exists(OUT));dir.create(OUT,recursive=TRUE)
logcon<-file(file.path(OUT,"run_log.txt"),"wt");sink(logcon,split=TRUE)
KEY<-c("CBSA","ntee_broad_clean","TAXYEAR");CTRL<-c("log_lag_orgs","log_pop","log_mhi_real_2022","poverty_rate","unemployment_rate")
FE<-"CBSA + ntee_broad_clean + TAXYEAR"
fits<-list();coefficients<-list();samples<-list();centers<-list();effects<-list();reference_checks<-list()
# fixest captures its calling environment (including whole analysis tables).
# Retain estimates, covariances and fitted vectors without serializing that workspace.
# Refit/update via this script and analysis_samples.rds rather than the saved call.
compact_model<-function(x){
  if(is.environment(x))return(baseenv())
  if(inherits(x,"formula")){environment(x)<-baseenv();return(x)}
  if(is.list(x))for(i in seq_along(x))if(!is.null(x[[i]]))x[[i]]<-compact_model(x[[i]])
  x
}
fit_model<-function(a,rhs,id,fes=FE){
  needed<-unique(c("net_growth",rhs,"CBSA","ntee_broad_clean","TAXYEAR"))
  a<-a[complete.cases(a[,..needed])]
  if(nrow(a)<30L || uniqueN(a$CBSA)<2L){samples[[id]]<<-data.table(model=id,eligible_N=nrow(a),estimated_N=NA_integer_,CBSAs=uniqueN(a$CBSA),status="insufficient sample");return(NULL)}
  f<-as.formula(paste("net_growth ~",paste(rhs,collapse=" + "),"|",fes),env=baseenv())
  fit<-tryCatch(feols(f,data=a,cluster=~CBSA,notes=FALSE),error=function(e)e)
  if(inherits(fit,"error")){
    if(!grepl("singleton|collinear|constant|not enough|no observation|all observations",conditionMessage(fit),ignore.case=TRUE))stop(fit)
    samples[[id]]<<-data.table(model=id,eligible_N=nrow(a),estimated_N=NA_integer_,CBSAs=uniqueN(a$CBSA),status=paste("not estimable:",conditionMessage(fit)))
    return(NULL)
  }
  ct<-as.data.table(coeftable(fit),keep.rownames="term");setnames(ct,names(ct)[2:5],c("estimate","se","statistic","p"))
  ci<-as.data.table(confint(fit),keep.rownames="term");setnames(ci,names(ci)[2:3],c("ci_low","ci_high"));ct<-merge(ct,ci,by="term");ct[,model:=id]
  coefficients[[id]]<<-ct;fits[[id]]<<-compact_model(fit)
  samples[[id]]<<-data.table(model=id,eligible_N=nrow(a),estimated_N=nobs(fit),CBSAs=uniqueN(a$CBSA),r2=as.numeric(fitstat(fit,"r2")[[1]]),within_r2=as.numeric(fitstat(fit,"wr2")[[1]]),status="estimated",collinear_terms=paste(fit$collin.var,collapse=";"),fixed_effects=fes)
  e<-rbindlist(lapply(intersect(c("lag_hhi_private","lag_hhi_public","lag_hhi_client","lag_private_public_coupling"),names(coef(fit))),function(v){
    q<-quantile(a[[v]],c(.25,.75));data.table(model=id,term=v,eligible_p25=q[1],eligible_p75=q[2],effect_pp=100*coef(fit)[v]*(q[2]-q[1]),eligible_mean_growth_pp=100*mean(a$net_growth))
  }),fill=TRUE);effects[[id]]<<-e
  fit
}
headline<-function(m,acs,scenario,arm){
  d<-merge(m[flag_positive_revenue==1 & unresolved_orgs==0],acs[,.(CBSA,TAXYEAR,pop_total)],by=c("CBSA","TAXYEAR"))
  totals<-as.matrix(d[,.(total_private,total_public,total_client)])
  h<-as.matrix(d[,.(hhi_private,hhi_public,hhi_client)]);h[totals<=0]<-NA_real_
  comp<-rowMeans(h,na.rm=TRUE);if(arm=="all_three")comp[rowSums(!is.na(h))!=3L]<-NA_real_
  density<-100000*d$n_orgs/d$pop_total;ok<-is.finite(comp)&is.finite(density);comp<-comp[ok];density<-density[ok]
  data.table(scenario,composite=arm,N=length(comp),r=cor(density,comp),pct_high_high=100*mean(density>median(density)&comp>median(comp)),benchmark=100*mean(density>median(density))*mean(comp>median(comp)))
}
prepare<-function(m,ctl){
  m<-copy(m[flag_positive_revenue==1 & unresolved_orgs==0]);setorderv(m,KEY)
  lagvars<-c("n_orgs","hhi_gross","hhi_private","hhi_public","hhi_client","total_private","total_public","total_client","gross_pp_pearson","net_pp_pearson","reporting_pct","strict_revenue_pct","floored_orgs","TAXYEAR")
  m[,(paste0("lag_",lagvars)):=shift(.SD),by=.(CBSA,ntee_broad_clean),.SDcols=lagvars]
  m<-m[TAXYEAR-lag_TAXYEAR==1 & lag_n_orgs>0]
  m[,`:=`(net_growth=(n_orgs-lag_n_orgs)/lag_n_orgs,log_lag_orgs=log1p(lag_n_orgs))]
  m<-merge(m,ctl[,c("CBSA","TAXYEAR",setdiff(CTRL,"log_lag_orgs")),with=FALSE],by=c("CBSA","TAXYEAR"),all.x=TRUE)
  m<-m[is.finite(net_growth) & complete.cases(m[,..CTRL])]
  m[,`:=`(lag_hhi_private_net0=lag_hhi_private,lag_hhi_public0=lag_hhi_public,lag_hhi_client0=lag_hhi_client)]
  m
}
main<-function(){
  cat("Output:",OUT,"\nInput:",INPUT,"\n")
  mk<-readRDS(file.path(INPUT,"market_measures_and_coupling.rds"))
  ctl<-fread(file.path(ROOT,"output/revisions/NVSQ_R1_2026-07/tables/cbsa_acs5_controls_2012_2021.csv"))
  stopifnot(!anyDuplicated(ctl[,.(CBSA,TAXYEAR)]))
  h<-rbindlist(lapply(c("original_rows","selected_policy","selected_reporters_only"),function(sc)rbindlist(lapply(c("active_mean","all_three"),function(arm)headline(mk[scenario==sc],ctl,sc,arm)))))
  stopifnot(h[scenario=="original_rows"&composite=="active_mean"]$N==75806L,abs(h[scenario=="original_rows"&composite=="active_mean"]$r-(-.449158451955958))<1e-9)
  fwrite(h,file.path(OUT,"combined_correction_headlines.csv"))
  datasets<-lapply(c("original_rows","selected_policy"),function(sc)prepare(mk[scenario==sc],ctl));names(datasets)<-c("original_rows","selected_policy")
  common_keys<-merge(datasets[[1]][,..KEY],datasets[[2]][,..KEY],by=KEY)
  for(sc in names(datasets)){datasets[[sc]][,common_policy:=FALSE];datasets[[sc]][common_keys,on=KEY,common_policy:=TRUE]}
  ref<-copy(datasets$original_rows)
  ref[,`:=`(lag_hhi_private=lag_hhi_gross,lag_private_public_coupling=lag_gross_pp_pearson)]
  common<-ref[is.finite(lag_private_public_coupling)];hq<-common[flag_hq_public_2020_2021==1]
  stopifnot(nrow(ref)==67969L,nrow(common)==42017L,nrow(hq)==7049L)
  hh<-c("lag_hhi_private","lag_hhi_public","lag_hhi_client");pp<-"lag_private_public_coupling"
  refs<-list("M1 HHI-only (common)"=list(common,c(hh,CTRL)),"M2 concordance-only (common)"=list(common,c(pp,CTRL)),"M3 joint (common)"=list(common,c(hh,pp,CTRL)),"M4 HHI-only (full sample)"=list(ref,c(hh,CTRL)),"M5 HQ 2020-21 joint"=list(hq,c(hh,pp,CTRL)))
  target<-fread(file.path(ROOT,"output/revisions/NVSQ_R1_2026-07/tables/entry_rerun_coefficients.csv"))
  for(nm in names(refs)){
    id<-paste0("R1_reference__",nm);fit<-fit_model(refs[[nm]][[1]],refs[[nm]][[2]],id)
    t<-target[target$model==nm];stopifnot(!is.null(fit),all(nobs(fit)==t$N))
    delta<-abs(unname(coef(fit)[t$var])-t$coef);stopifnot(all(delta<.000051))
    reference_checks[[nm]]<-data.table(model=nm,term=t$var,reference=t$coef,rebuilt=unname(coef(fit)[t$var]),N=nobs(fit),abs_difference=delta)
  }
  fwrite(rbindlist(reference_checks),file.path(OUT,"reference_model_validation.csv"));cat("Five R1 specifications reproduced.\n")
  for(sc in names(datasets)){
    d<-datasets[[sc]]
    selections<-list(full=rep(TRUE,nrow(d)),common_policy=d$common_policy,
       common_coupling=is.finite(d$lag_gross_pp_pearson)&is.finite(d$lag_net_pp_pearson),
       outcome_2020_21=d$TAXYEAR>=2020,predictor_2020_21=d$lag_TAXYEAR>=2020,
       predictor_reporting20=d$lag_TAXYEAR>=2020 & d$lag_reporting_pct>=20,
       predictor_reporting30=d$lag_TAXYEAR>=2020 & d$lag_reporting_pct>=30,
       predictor_reporting40=d$lag_TAXYEAR>=2020 & d$lag_reporting_pct>=40,
       strict_revenue95=d$lag_strict_revenue_pct>=95,
       strict_revenue100=d$lag_strict_revenue_pct>=100-1e-9,
       no_floor_in_predictor=d$lag_floored_orgs==0)
    for(samp in names(selections)){
      base<-d[which(selections[[samp]])]
      specs<-if(samp%in%c("full","common_policy","common_coupling"))c("gross_zero","net_zero","net_active_joint","net_activity_indicators") else c("net_active_joint","net_activity_indicators")
      for(spec in specs){
        a<-copy(base);terms<-hh
        a[,`:=`(lag_hhi_private=if(spec=="gross_zero")lag_hhi_gross else lag_hhi_private_net0,lag_hhi_public=lag_hhi_public0,lag_hhi_client=lag_hhi_client0,
                 lag_private_public_coupling=if(spec=="gross_zero")lag_gross_pp_pearson else lag_net_pp_pearson)]
        for(side in c("private","public","client")){
          v<-paste0("lag_hhi_",side);active<-a[[paste0("lag_total_",side)]]>0
          if(spec=="net_active_joint")a[!active,(v):=NA_real_]
          if(spec=="net_activity_indicators"){
            center<-if(any(active))mean(a[[v]][active]) else 0
            a[,(v):=ifelse(active,get(v)-center,0)]
            ind<-paste0("no_observed_",side);a[,(ind):=as.integer(!active)];terms<-c(terms,ind)
            centers[[length(centers)+1L]]<-data.table(scenario=sc,sample=samp,side,center,n_active=sum(active))
          }
        }
        variants<-if(samp=="common_coupling")c("HHI_only","joint") else "HHI_only"
        for(variant in variants){id<-paste(sc,samp,spec,variant,sep="__");cat("Fitting",id,"\n");fit_model(a,c(terms,CTRL,if(variant=="joint")pp),id)}
        if(samp=="full" && spec=="net_activity_indicators")fit_model(a,c(terms,CTRL),paste(sc,"full","net_activity_indicators","CBSA_sector_FE",sep="__"),"CBSA^ntee_broad_clean + TAXYEAR")
      }
    }
    for(side in c("private","public","client")){
      a<-copy(d[get(paste0("lag_total_",side))>0]);a[,lag_hhi_private:=lag_hhi_private_net0]
      fit_model(a,c(paste0("lag_hhi_",side),CTRL),paste(sc,"side_specific",side,sep="__"))
    }
  }
  fwrite(rbindlist(coefficients,fill=TRUE),file.path(OUT,"coefficients.csv"))
  fwrite(rbindlist(samples,fill=TRUE),file.path(OUT,"model_samples.csv"))
  fwrite(rbindlist(centers),file.path(OUT,"active_hhi_centers.csv"))
  fwrite(rbindlist(effects,fill=TRUE),file.path(OUT,"iqr_effects.csv"))
  saveRDS(fits,file.path(OUT,"models.rds"));saveRDS(datasets,file.path(OUT,"analysis_samples.rds"))
  writeLines(capture.output(sessionInfo()),file.path(OUT,"session_info.txt"))
  writeLines(c(file.path(INPUT,"market_measures_and_coupling.rds"),file.path(ROOT,"output/revisions/NVSQ_R1_2026-07/tables/cbsa_acs5_controls_2012_2021.csv")),file.path(OUT,"inputs.txt"))
  file.copy(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),file.path(OUT,"script_snapshot.R"))
  writeLines(c("Completed: five original specifications reproduce stored N and rounded coefficients.","Consecutive-year net growth, contemporaneous ACS controls, lag-year HHIs/concordance/reporting; CBSA clustered inference.","Gross_zero is the baseline coding; net_zero isolates the revenue correction; net_active_joint and net_activity_indicators handle inactivity.","Common_policy holds eligible market-year keys fixed across original/selected, but filing corrections can change outcomes and regressors.","Common_coupling holds gross and corrected concordance eligibility fixed within each filing scenario.","Unknown selected grants exclude affected market-years before lags, not as zero activity. Reporter-only headlines do not describe full markets.","Strict coverage and reporter thresholds change the population. No causal interpretation or equivalence claim."),file.path(OUT,"SUCCESS.txt"))
  cat("SUCCESS:",OUT,"\n")
}
tryCatch(main(),error=function(e){writeLines(conditionMessage(e),file.path(OUT,"ERROR.txt"));stop(e)},finally={sink();close(logcon)})
