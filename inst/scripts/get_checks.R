
   
   
   
   library(BiocParallel)
   library(rcmdcheck)
   library(BiocBuildTools)
   register(SnowParam(40)) # seems more robust
   
   checks.destination = "~/BBS_space/chks316"
   sources.folder = "~/BBS_space/REL_316_srcs"

#source("build_packageset_3.16a.R")

pset = readRDS("PackageSet_3.16a.rds")	

#populate_local_gits(pset, sources.folder)

   
get_checks = function(pkgset, sources.folder, checks.destination, N.TRIES=5) {
   
   allp = slot(pkgset, "pkgnames")
   N_SRCS = length(dir(sources.folder))
   N_DONE = length(dir(checks.destination))
   done_pkgs = gsub("_chk.rds", "", dir(checks.destination))
   
   shuffle = function(x) sample(x, size=length(x), replace=FALSE)
   
   cur = 0
   todo <- setdiff(allp, done_pkgs)
   while(length(todo)>0) {
     print(length(todo))
     print(head(todo))
     od = getwd()
     setwd(sources.folder)
     print(getwd())
     tryall = bplapply(shuffle(todo), function(x) {
          z = try(safe_rcmdcheck(x)); 
          saveRDS(z, paste0(checks.destination, "/", x, "_chk.rds"))
          })
     cur = cur+1
     if (cur > N_TRIES) break
     done_pkgs = gsub("_chk.rds", "", dir(checks.destination))
     todo <- setdiff(allp, done_pkgs)
   }
          
  } 

get_checks(pset, sources.folder, checks.destination)