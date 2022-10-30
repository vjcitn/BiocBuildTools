

library(BiocBuildTools)
library(rcmdcheck)
library(BiocCheck)


sources.folder = "./srcs316_2"
bcchecks.destination = "./bcchecks_2"
bcobj.destination = "./bcobj_2"

library(BiocParallel)
spar = SnowParam(30, threshold="DEBUG")
bplog(spar) = TRUE
bplogdir(spar) = "~/BBS_space/BPLOGS_bc"
bptimeout(spar) = 600
register(spar)

#register(SerialParam(threshold="DEBUG"))


set.seed(1234)
goal = dir(sources.folder)
done = gsub(".BiocCheck$", "", dir(bcchecks.destination))
todo = setdiff(goal, done)
print(head(todo))
print(length(todo))
todopaths = paste0(sources.folder, "/", todo)
shuffle = function(x) sample(x, size=length(x), replace=FALSE)
allbc3 = bplapply(shuffle(todopaths), function(x) {
   print(x)
   futile.logger::flog.info(paste0("'x' = ", x))
   futile.logger::flog.error(paste0("'x' = ", x))
   ans = try(bco2df(BiocCheck::BiocCheck(x, checkDir=bcchecks.destination))) 
   dest = paste0(bcobj.destination, "/", paste0(basename(x), "_chk.rds"))
   saveRDS(ans, dest)
   NULL
   })

