
bcc_process_package_path = function(x, bcchecks.destination, bcobj.destination) {
        print(x)
        futile.logger::flog.info(paste0("bioccheck 'x' = ", x))
        futile.logger::flog.error(paste0("bioccheck 'x' = ", x))
        dest = paste0(bcobj.destination, "/", paste0(basename(x), 
            "_chk.rds"))
        prechk = try(pkgbuild::build(x, clean_doc=TRUE))
        if (inherits(prechk, "try-error")) {  # severe issue usually with latex
             saveRDS(prechk, dest)
             return(NULL)
             }
        unlink(prechk)
        tmpans = try(BiocCheck::BiocCheck(x, checkDir = bcchecks.destination,
          `no-check-deprecated`=TRUE, `no-check-formatting`=TRUE,
           `no-check-CRAN`=TRUE, `no-check-bioc-help`=TRUE)) 
        attr(tmpans, "last_commit_date") <- last_commit_date(x)
        attr(tmpans, "check_date") <- Sys.time()
        attr(tmpans, "pkgversion") <- try(tmpans$metadata$PackageVersion)
        if (inherits(tmpans, "try-error")) {
            saveRDS(tmpans, dest)
            return(NULL)
            }
        ans = try(bco2df(tmpans))
        saveRDS(ans, dest)
        NULL
    }

rcc_process_package_path = function(x, checks.destination) {
          futile.logger::flog.info(paste0("rcmdcheck 'x' = ", x))
          dest= paste0(checks.destination, "/", basename(x), "_chk.rds")
          prechk = try(pkgbuild::build(x, clean_doc=TRUE))
          if (inherits(prechk, "try-error")) {  # severe issue usually with latex
             saveRDS(prechk, dest)
             return(NULL)
             }
          z = try(rcmdcheck::rcmdcheck(prechk, error_on="never")) # try(safe_rcmdcheck(x)); 
          unlink(prechk)
          futile.logger::flog.error(paste0("rcmdcheck 'x' = ", x))
          attr(z, "last_commit_date") <- last_commit_date(x)
          attr(z, "check_date") <- Sys.time()
          attr(z, "pkgversion") <- try(z$version)
          saveRDS(z, paste0(checks.destination, "/", basename(x), "_chk.rds"))
          NULL
          }
 
          


# RUN WITH xvfb-run
#   
#   library(BiocParallel)
#   library(rcmdcheck)
#   library(BiocBuildTools)
#
#checks.destination = "~/BBS_space/chks316_3"
#sources.folder = "~/BBS_space/REL_316_srcs"
#
#spar = SnowParam(45)
#bplog(spar) = TRUE
#bplogdir(spar) ="~/BBS_space/BPLOGS"
#bptimeout(spar) = 600
#bpthreshold(spar) = "DEBUG"
#
#   register(spar)
#
##register(SerialParam())
#source("build_packageset_3.16e.R")
#
#pset = readRDS("PackageSet_3.16e.rds")
#   
#pset@pkgnames = pset@pkgnames
#
#if (!dir.exists(checks.destination)) dir.create(checks.destination)
#if (!dir.exists(sources.folder)) stop("can't find sources in sources.folder; folder does not exist")
#get_checks(pset, sources.folder, checks.destination)

#' use gert to get last commit date for a repo
#' @param repo character(1) path to a checkout from git
last_commit_date = function(repo) { 
  ans = Sys.time() + NA # if gert not available, produce a classed NA
  if (requireNamespace("gert")) ans = gert::git_log(repo=repo, max=1)$time 
  ans
}


#' run rcmdcheck and BiocCheck on all sources associated with a PackageSet and drop the RDS outputs in separate folders
#' @importFrom methods slot
#' @param pkgset instance of PackageSet
#' @param sources.folder path to git repos
#' @param checks.destination path to folder holding RDS of rcmdcheck output
#' @param bcchecks.destination character(1) path to a writeable folder where BiocCheck logs are written
#' @param bcobj.destination character(1) path to a folder where RDS files with data frames are written
#' @param shuffle.pks logical(1) if TRUE the packages are randomized before checking
#' @param BPPARAM defaults to bpparam()
#' @param BPOPTIONS defaults to bpoptions()
#' @export
get_checks2 = function(pkgset, sources.folder, checks.destination,
   bcchecks.destination, bcobj.destination, shuffle.pks=FALSE,
   BPPARAM=bpparam(), BPOPTIONS=bpoptions()) {
   
   allp = slot(pkgset, "pkgnames")
   N_SRCS = length(dir(sources.folder))
   N_DONE = length(dir(bcobj.destination))
   done_pkgs = character()
   if (N_DONE > 0) done_pkgs = gsub("_chk.rds", "", dir(bcobj.destination))
   
   shuffle = function(x) sample(x, size=length(x), replace=FALSE)
   if (!shuffle.pks) shuffle=force
  
   
   todo <- setdiff(allp, done_pkgs)
     print(length(todo))
     print(head(todo,50))
     tryall = bplapply(paste0(sources.folder, "/", shuffle(todo)), function(x) {
     rcc_process_package_path(x, checks.destination=checks.destination) 
     bcc_process_package_path(x, bcchecks.destination=bcchecks.destination, bcobj.destination=bcobj.destination) 
          NULL
          }, BPPARAM=BPPARAM, BPOPTIONS=BPOPTIONS)
   }
          

