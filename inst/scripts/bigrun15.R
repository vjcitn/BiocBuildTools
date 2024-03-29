






library(BiocBuildTools)
civ = Sys.getenv("CI")
stopifnot(civ=="true")

#ps = PackageSet(biocmaint, biocversion="3.17", branchname="master")
#ps = add_dependencies(ps)

ps = readRDS("PackageSet_3.17.rds")

#td = tempfile("fulldemo")
td = "~/BBS_space/NOV5_FULL/fullgits_nov10"  # updated nov27 with update_gits.R

#uu = lapply(xx[1501:2000], function(x) {cat(x, "\n"); zz = try(git_pull(repo=x)); od=getwd(); if (inherits(zz, "try-error")){setwd(x); system("git pull"); setwd(od)}})
#uu = lapply(xx[2001:length(xx)], function(x) {cat(x, "\n"); zz = try(git_pull(repo=x)); od=getwd(); if (inherits(zz, "try-error")){setwd(x); system("git pull"); setwd(od)}})
#dir.create(td)

#populate_local_gits(ps, td)

if (!dir.exists("~/BBS_space/NOV28_FULL")) dir.create("~/BBS_space/NOV28_FULL")
chkdest = "~/BBS_space/NOV28_FULL/rchk"
if (!dir.exists(chkdest)) dir.create(chkdest)

bdest = "~/BBS_space/NOV28_FULL/bchk"
if (!dir.exists(bdest)) dir.create(bdest)

bobdest = "~/BBS_space/NOV28_FULL/bobs"
if (!dir.exists(bobdest)) dir.create(bobdest)

library(BiocParallel)
spar = SnowParam(18)
bplog(spar) = TRUE
bpstopOnError(spar) = FALSE
bplogdir(spar) = "~/BBS_space/BPLOGS.bioc3.17c"
register(spar)


get_checks2(ps, sources.folder=td, checks.destination=chkdest, 
   bcchecks.destination=bdest, bcobj.destination=bobdest,
   BPOPTIONS=bpoptions(exports=c("chkdest", "bdest", "bobdest")), shuffle=TRUE)

build_sqlite_db("biocfull15.sqlite", rcmd=chkdest, bcc=bobdest)


