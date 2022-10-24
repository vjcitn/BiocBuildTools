
library(BiocParallel)
library(BiocBuildTools)
register(SnowParam(40)) # seems more robust
od = getwd()
setwd("~/BBS_space/REL_315_srcs")
allpk = dir()
savedir = "~/BBS_space/chks315"
try2000 = bplapply(allpk[1201:2000], function(x) {z = try(safe_rcmdcheck(x)); saveRDS(z, paste0(savedir, "/", x, "_chk.rds"))})
setwd(od)
save(try2000, file="try2000.rda")

