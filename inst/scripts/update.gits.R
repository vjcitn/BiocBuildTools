# this is provisional ... there are probably new admissions to 3.17 that are ignored
library(gert)
allsrcs = dir("~/BBS_space/NOV5_FULL/fullgits_nov10") # anywhere all srcs are checked out
setwd("~/BBS_space/NOV5_FULL/fullgits_nov10")
uu = lapply(allsrcs, function(x) {cat(x, "\n"); zz = try(git_pull(repo=x)); od=getwd(); if (inherits(zz, "try-error")){setwd(x); system("git pull"); setwd(od)}})

