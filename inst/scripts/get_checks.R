
# RUN WITH xvfb-run
   
   library(BiocParallel)
   library(rcmdcheck)
   library(BiocBuildTools)

checks.destination = "./chks316_2"
sources.folder = "./srcs316_2"

spar = SnowParam(35)
bplog(spar) = TRUE
bplogdir(spar) ="./BPLOGS"
bptimeout(spar) = 600
bpthreshold(spar) = "DEBUG"

   register(spar)

#register(SerialParam())
source("build_exp_ps.R")

pset = readRDS("PackageSet_3.16d.rds")
   
pset@pkgnames = pset@pkgnames[1:70]


populate_local_gits(pset, sources.folder)

if (!dir.exists(checks.destination)) dir.create(checks.destination)
if (!dir.exists(sources.folder)) stop("can't find sources in sources.folder; folder does not exist")

get_checks = function(pkgset, sources.folder, checks.destination) {
   
   allp = slot(pkgset, "pkgnames")
   N_SRCS = length(dir(sources.folder))
   N_DONE = length(dir(checks.destination))
   done_pkgs = character()
   if (N_DONE > 0) done_pkgs = gsub("_chk.rds", "", dir(checks.destination))
   
   shuffle = function(x) sample(x, size=length(x), replace=FALSE)
   
   cur = 0
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
          

get_checks(pset, sources.folder, checks.destination)
