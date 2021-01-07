stopifnot(version$version.string >= "4.1")
library(BiocBuildTools)
library(RSQLite)
ps = PackageSet(bioc_coreset())
print(ps@pkgnames)
tag = "demo16"
gitfolder = paste0("/tmp/", tag, "_srcs")
sqlitetarget = paste0("/tmp/", tag, ".sqlite")
pnettarget = paste0("/tmp/", tag, "_pnet.rds")
populate_local_gits(ps, gitfolder)
alldirs = dir(gitfolder, full=TRUE)

# as of 12/31/2020 rcmdcheck will return a list with fields
#
#c("stdout", "stderr", "status", "duration", "timeout", "rversion", 
#"platform", "errors", "warnings", "notes", "description", "package", 
#"version", "cran", "bioc", "checkdir", "test_fail", "install_out", 
#"session_info", "cleaner")
#

fullrun = function(srcs, target_sqlite_path) {
     ii = rownames(installed.packages())
     getdeps = lapply(basename(srcs), function(x) try(fulfill_deps(x)))
     rcc1 = lapply(srcs, safe_rcmdcheck)
     names(rcc1) = basename(srcs)
     rcc1_safe = rcc1
     save(rcc1_safe, file="rcc1_safe.rda")
     
     bcc1 = lapply(srcs, function(x) try(BiocCheck::BiocCheck(x)))
     names(bcc1) = basename(srcs)
     bcc1 = lapply(seq_along(bcc1), function(x) {y = bcc1[[x]]; y$package = names(bcc1)[x]; y})
     
     cov1 = lapply(srcs, function(x) {
              tmp = try(covr::package_coverage(x)); 
              if (inherits(tmp, "try-error")) return(list(filecoverage=NA, totalcoverage=NA))
              covr::coverage_to_list(tmp)
              })
     names(cov1) = basename(srcs)
    
    covs_to_dataframes = function(clist) {
      tmp = lapply(clist, function(x) {
           if (is.na(x$filecoverage)) {
             return(data.frame(file=NA, coverage_pct=NA))
             }
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
    status_db_init(target_sqlite_path, rcdfs)
    status_db_init(target_sqlite_path, bcdfs, exists_ok=TRUE)
    status_db_init(target_sqlite_path, cvdfs, exists_ok=TRUE)
    try(make_pnet_object(srcs, target=pnettarget))
}
fullrun(alldirs, sqlitetarget)
    
