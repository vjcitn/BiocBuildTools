

library(BiocBuildTools)
library(rcmdcheck)
library(BiocCheck)

if (!dir.exists("bcchecks")) dir.create("bcchecks")

   checks.destination = "~/BBS_space/chks316"
 sources.folder = "~/BBS_space/REL_316_srcs"
bcchecks.destination = "./bcchecks"

library(parallel)
options(mc.cores=30)

goal = dir(sources.folder)
done = gsub(".BiocCheck$", "", dir(bcchecks.destination))
todo = setdiff(goal, done)
print(head(todo))
todopaths = paste0(sources.folder, "/", todo)
allbc2 = mclapply(todopaths, function(x) try(BiocCheck::BiocCheck(x, checkDir=bcchecks.destination)))
save(allbc2, file="allbc2.rda")

