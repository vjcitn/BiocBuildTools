# continue after crash
library(BiocBuildTools)

alld = dir("../gits", full=TRUE)

library(BiocParallel)

mp = MulticoreParam(55)

register(mp)

done = dir(patt="tar.gz$")
donn = gsub("_.*", "", done)
todo = setdiff(basename(alld), donn)

xx = bplapply(todo, function(x) try(build1_with_buildsink(x)))
