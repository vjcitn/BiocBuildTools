library(BiocParallel)
library(BiocBuildTools)
register(SnowParam(8)) # seems more robust
od = getwd()
setwd("~/BBS_space/REL_315_srcs")
allpk = dir()
try16 = bplapply(allpk[1:16], safe_rcmdcheck)
setwd(od)
save(try16, file="try16.rda")

