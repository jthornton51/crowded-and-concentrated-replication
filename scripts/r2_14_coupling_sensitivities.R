# Follow-up to r2_12: negative-difference and period-uncertainty exclusions,
# plus one fixed sample across Pearson, Spearman and partial correlations.
suppressPackageStartupMessages(library(data.table));setDTthreads(4L)
a<-commandArgs(TRUE);stopifnot(length(a)==2L)
ROOT<-normalizePath(a[1],winslash="/",mustWork=TRUE);INPUT<-normalizePath(a[2],winslash="/",mustWork=TRUE)
stopifnot(file.exists(file.path(INPUT,"SUCCESS.txt")))
OUT<-file.path(ROOT,"output/revisions/NVSQ_R2_2026-09/rebuild",paste0("coupling_sensitivity_",format(Sys.time(),"%Y%m%d_%H%M%S")))
stopifnot(!dir.exists(OUT));dir.create(OUT,recursive=TRUE)
m<-readRDS(file.path(INPUT,"market_measures_and_coupling.rds"))
paired<-function(d,cols,sc,lab,meas){
  x<-d[complete.cases(d[,..cols])];if(nrow(x)==0)return(NULL)
  v<-as.matrix(x[,..cols]);g<-v[,1]-v[,2];n<-v[,3]-v[,4]
  b<-data.table(CBSA=x$CBSA,gross=g,net=n,change=n-g)
  z<-b[,.(N=.N,gross=sum(gross),net=sum(net),change=sum(change)),by=CBSA]
  mat<-as.matrix(z[,.(gross,net,change)]);counts<-z$N
  set.seed(20260916);boot<-replicate(999L,{i<-sample.int(nrow(z),replace=TRUE);colSums(mat[i,,drop=FALSE])/sum(counts[i])})
  means<-colMeans(v)
  out<-data.table(scenario=sc,sample=lab,measure=meas,N=nrow(x),CBSAs=nrow(z),gross_pp=means[1],gross_pc=means[2],net_pp=means[3],net_pc=means[4],gap_gross=mean(g),gap_net=mean(n),gap_change=mean(n-g))
  for(j in 1:3){nm<-c("gap_gross","gap_net","gap_change")[j];out[[paste0(nm,"_lo")]]<-unname(quantile(boot[j,],.025));out[[paste0(nm,"_hi")]]<-unname(quantile(boot[j,],.975))}
  out
}
res<-list()
allcols<-unlist(lapply(c("pearson","spearman","partial"),function(meas)paste(rep(c("gross","net"),each=2),rep(c("pp","pc"),2),meas,sep="_")))
for(sc in c("original_rows","selected_policy")){
  d<-m[scenario==sc]
  selections<-list(no_floor=d$floored_orgs==0,period_verified=d$period_unverified_orgs==0,
                    common_three_metrics=complete.cases(d[,..allcols]),
                    common_three_metrics_no_floor=complete.cases(d[,..allcols]) & d$floored_orgs==0)
  for(lab in names(selections))for(meas in c("pearson","spearman","partial")){
    cols<-paste(rep(c("gross","net"),each=2),rep(c("pp","pc"),2),meas,sep="_")
    res[[length(res)+1L]]<-paired(d[which(selections[[lab]])],cols,sc,lab,meas)
  }
}
fwrite(rbindlist(res),file.path(OUT,"paired_sensitivities.csv"))
fwrite(m[scenario=="selected_policy"&floored_orgs>0][order(-floor_dollars)][1:min(.N,50),.(CBSA,ntee_broad_clean,TAXYEAR,n_orgs,floored_orgs,floor_dollars,total_gross,total_public)],file.path(OUT,"largest_floor_market_years.csv"))
writeLines(capture.output(sessionInfo()),file.path(OUT,"session_info.txt"))
file.copy(sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),file.path(OUT,"script_snapshot.R"))
writeLines(c("Completed 999-draw CBSA cluster-bootstrap sensitivity comparisons.",paste("Input:",INPUT),"Exclusions change the population; they are not proof that records removed are erroneous.","Period_verified excludes flagged unmatched repeated keys, not a claim of exact filing linkage for every retained record."),file.path(OUT,"SUCCESS.txt"))
cat("SUCCESS:",OUT,"\n")
