

library(BiocBuildTools)
civ = Sys.getenv("CI")
stopifnot(civ=="true")

#ps = PackageSet(biocmaint, biocversion="3.17", branchname="master")
#ps = add_dependencies(ps)

ps = readRDS("PackageSet_3.17.rds")

#td = tempfile("fulldemo")
td = "~/BBS_space/NOV5_FULL/fulldemoacf8912304544" # updated 10 Nov

#uu = lapply(xx[1501:2000], function(x) {cat(x, "\n"); zz = try(git_pull(repo=x)); od=getwd(); if (inherits(zz, "try-error")){setwd(x); system("git pull"); setwd(od)}})
#uu = lapply(xx[2001:length(xx)], function(x) {cat(x, "\n"); zz = try(git_pull(repo=x)); od=getwd(); if (inherits(zz, "try-error")){setwd(x); system("git pull"); setwd(od)}})
#dir.create(td)

#populate_local_gits(ps, td)

chkdest = "~/BBS_space/NOV10_FULL/rchk"
#dir.create(chkdest)

bdest = "~/BBS_space/NOV10_FULL/bchk"
dir.create(bdest)

bobdest = "~/BBS_space/NOV10_FULL/bobs"
dir.create(bobdest)

library(BiocParallel)
spar = SnowParam(30)
bplog(spar) = TRUE
bplogdir(spar) = "~/BBS_space/BPLOGS.bioc3.17"
register(spar)


get_checks2(ps, sources.folder=td, checks.destination=chkdest, 
   bcchecks.destination=bdest, bcobj.destination=bobdest,
   BPOPTIONS=bpoptions(exports=c("chkdest", "bdest", "bobdest")))

build_sqlite_db("biocfull4.sqlite", rcmd=chkdest, bcc=bobdest)


