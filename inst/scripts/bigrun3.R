
library(BiocBuildTools)
civ = Sys.getenv("CI")
stopifnot(civ=="true")

#ps = PackageSet(biocmaint, biocversion="3.17", branchname="master")
#ps = add_dependencies(ps)

ps = readRDS("PackageSet_3.17.rds")

td = tempfile("fulldemo")
td = "~/BBS_space/NOV5_FULL/fulldemoacf8912304544"
#dir.create(td)

#populate_local_gits(ps, td)

chkdest = tempfile("fullrchks")
chkdest = "~/BBS_space/NOV7_FULL/rchk"
#dir.create(chkdest)

bdest = "~/BBS_space/NOV7_FULL/bchk"
dir.create(bdest)

bobdest = "~/BBS_space/NOV7_FULL/bobs"
dir.create(bobdest)

library(BiocParallel)
spar = SnowParam(35)
bplog(spar) = TRUE
bplogdir(spar) = "~/BBS_space/BPLOGS.bioc3.17"
register(spar)


get_checks2(ps, sources.folder=td, checks.destination=chkdest, 
   bcchecks.destination=bdest, bcobj.destination=bobdest,
   BPOPTIONS=bpoptions(exports=c("chkdest", "bdest", "bobdest")))

build_sqlite_db("biocfull2.sqlite", rcmd=chkdest, bcc=bobdest)



