
packs = c("parody", "vsn", "eds", "BiocFileCache")

library(BiocBuildTools)
civ = Sys.getenv("CI")
stopifnot(civ=="true")

ps = PackageSet(packs, biocversion="3.16", branchname="master")
ps = add_dependencies(ps)

td = tempfile("litdemo")
dir.create(td)

populate_local_gits(ps, td)

chkdest = tempfile("litrchks")
dir.create(chkdest)

bdest = tempfile("litbcchks")
dir.create(bdest)

bobdest = tempfile("litbobjs")
dir.create(bobdest)

library(BiocParallel)
spar = SnowParam(4)
register(spar)

get_checks2(ps, sources.folder=td, checks.destination=chkdest,
   bcchecks.destination=bdest, bcobj.destination=bobdest,
   BPOPTIONS=bpoptions(exports=c("chkdest", "bdest", "bobdest")))

build_sqlite_db("chk2.sqlite", rcmd=chkdest, bcc=bobdest)

