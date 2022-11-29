
#' longrunning R CMD check and BiocCheck of every package in a packageset
#' @param pset BiocBuildTools PackageSet instance
#' @param srcdir character(1) path to folder that can receive all sources using BiocBuildTools::populate_local_gits
#' @param update_sources logical(1) if TRUE, use update_gits to pull new changes in each package in srcdir
#' @param prefix character(1) path to folder within which check artifacts are produced,
#' will be created if nonexistent; defaults to `tempdir("checkPS")`
#' @param BPPARAM instance of BiocParallelParam
#' @param BPOPTIONS a list, see BiocParallel::bpoptions, defaults to empty list
#' @param shuffle logical(1) if TRUE, the list of packages is randomly permuted to avoid always
#' restarting on the same set, defaults to FALSE
#' @param sqlite_target path to SQLite database to be built using BiocBuildTools::build_sqlite_db
#' @return a list with elements sqlite_target, chk, and dbatt which are strings or try-error or
#' outputs of BiocBuildTools::get_checks2() and BiocBuildTools::build_sqlite_db() if these
#' succeed.  In good circumstances this function is called for the side effect of creating
#' the SQLite database
#' @note The function will test Sys.getenv("CI") to avoid a tcltk query from knitr.
#' @examples
#' pset = PackageSet(c("parody", "eds"))
#' srcdir = tempdir("demo_srcs")
#' spar = BiocParallel::SnowParam(2)
#' bplog(spar) = TRUE
#' bpstopOnError(spar) = FALSE
#' dir.create(ldir <- tempdir("chkps_logs"))
#' system(paste("chmod 777", ldir))
#' bplogdir(spar) = tempdir("chkps_logs")
#' cptry = check_PackageSet(pset, srcdir=srcdir, BPPARAM=spar,
#' BPOPTIONS=list(exports=c("chkdest", "bdest", "bobdest")))
#' cptry
#' @export
check_PackageSet = function(pset, srcdir, update_sources=FALSE, 
     prefix=tempdir("checkPS"), BPPARAM, BPOPTIONS=list(), shuffle=FALSE,
     sqlite_target = paste0(prefix, "/test.sqlite")) {
   civ = Sys.getenv("CI")
   stopifnot(civ=="true")
   
   ps = pset
   
   td = srcdir
   
   if (!update_sources) populate_local_gits(pset, srcdir)
   else {
      allgz = dir(srcdir, patt="tar.gz", full=TRUE)
      if (length(allgz)>0) unlink(allgz)
      update_gits(srcdir) # can take a long time
      }
   
   if (!dir.exists(prefix)) dir.create(prefix)
   chkdest = paste0(prefix, "/rchk")
   if (!dir.exists(chkdest)) dir.create(chkdest)
   
   bdest = paste0(prefix, "/bchk")
   if (!dir.exists(bdest)) dir.create(bdest)
   
   bobdest = paste0(prefix, "/bobdest")
   if (!dir.exists(bobdest)) dir.create(bobdest)
 
   chk = ""
   dbatt = ""
   chk = try(get_checks2(ps, sources.folder=td, checks.destination=chkdest, 
      bcchecks.destination=bdest, bcobj.destination=bobdest, BPPARAM=BPPARAM,
      BPOPTIONS=BPOPTIONS, shuffle=shuffle))
   
   dbatt = try(build_sqlite_db(sqlite_target, rcmd=chkdest, bcc=bobdest))
   list(sqlite_target=sqlite_target, chk=chk, dbatt=dbatt)
}
