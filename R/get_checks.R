
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
#' run rcmdcheck on all sources associated with a PackageSet and drop the RDS outputs in a folder
#' @param pkgset instance of PackageSet
#' @param sources.folder path to git repos
#' @param checks.destination path to folder holding RDS of rcmdcheck output
#' @export
get_checks = function(pkgset, sources.folder, checks.destination) {
   
   allp = slot(pkgset, "pkgnames")
   N_SRCS = length(dir(sources.folder))
   N_DONE = length(dir(checks.destination))
   done_pkgs = character()
   if (N_DONE > 0) done_pkgs = gsub("_chk.rds", "", dir(checks.destination))
   
   shuffle = function(x) sample(x, size=length(x), replace=FALSE)
   
   todo <- setdiff(allp, done_pkgs)
     print(length(todo))
     print(head(todo))
     tryall = bplapply(paste0(sources.folder, "/", shuffle(todo)), function(x) {
          futile.logger::flog.info(paste0("'x' = ", x))
          z = try(rcmdcheck::rcmdcheck(x, error_on="never")) # try(safe_rcmdcheck(x)); 
          futile.logger::flog.error(paste0("'x' = ", x))
          saveRDS(z, paste0(checks.destination, "/", basename(x), "_chk.rds"))
          NULL
          })
   }
          

