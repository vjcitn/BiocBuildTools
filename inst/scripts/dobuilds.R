library(BiocBuildTools)

alld = dir("../gits", full=TRUE)

library(BiocParallel)

mp = MulticoreParam(50)

register(mp)

xx = bplapply(alld, function(x) try(build1_with_buildsink(x)))
