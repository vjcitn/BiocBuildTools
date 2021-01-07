
library(BiocBuildTools)
demo_srcs = dir(system.file("demo_srcs", package="BiocBuildTools"), full=TRUE)
#ps = PackageSet(bioc_coreset())
#populate_local_gits(ps, "~/local_312")
if (!exists("rcc1")) {
 rcc1 = lapply(demo_srcs, rcmdcheck::rcmdcheck)
 names(rcc1) = basename(demo_srcs)
 }
if (!exists("bcc1")) {
 bcc1 = lapply(demo_srcs, BiocCheck::BiocCheck)
 names(bcc1) = basename(demo_srcs)
 bcc1 = lapply(seq_along(bcc1), function(x) {y = bcc1[[x]]; y$package = names(bcc1)[x]; y})
 }
if (!exists("cov1")) 
 {
 cov1 = lapply(demo_srcs, function(x) {tmp = covr::package_coverage(x); covr::coverage_to_list(tmp)})
 names(cov1) = basename(demo_srcs)
 }

covs_to_dataframes = function(clist) {
  tmp = lapply(clist, function(x) {
       fls = names(x$filecoverage)
       pcts = as.numeric(x$filecoverage)
       data.frame(file=fls, coverage_pct=pcts)
       } ) 
  ns = unlist(lapply(tmp, nrow))
#  names(tmp) = names(clist)
#  tmp
  tmp = do.call(base:::rbind.data.frame, c(tmp, make.row.names=FALSE))
  tmp$package = rep(names(clist), ns)
  list(covg=tmp)
}
      
rcdfs = rcc_to_dataframes(rcc1)
bcdfs = bcc_to_dataframes(bcc1)
names(bcdfs) = paste("bc", names(bcdfs), sep="")
cvdfs = covs_to_dataframes(cov1)
status_db_init("/tmp/demo2.sqlite", rcdfs)
status_db_init("/tmp/demo2.sqlite", bcdfs, exists_ok=TRUE)
status_db_init("/tmp/demo2.sqlite", cvdfs, exists_ok=TRUE)

