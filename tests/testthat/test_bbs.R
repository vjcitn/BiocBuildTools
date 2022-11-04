library(BiocBuildTools)

#context("enumerate software packages")
#
#test_that(
#  "BiocPkgTools functions to enumerate packages and dependencies", {
#  ll = bioc_software_packagelist()
#  expect_true(inherits(ll, "character"))
#  expect_true(length(ll)>1700)
#}) 

context("try a build")

#test_that(
#  "getpk retrieves a source image of a package and build1 works", {
#  curwd = getwd()
#  td = tempdir()
#  setwd(td)
#  getpk("graph")
#  expect_true(dir.exists("graph"))
#  expect_true(file.exists("graph/DESCRIPTION"))
#  bb = build1("graph", ".")
#  setwd(curwd)
#})


test_that(
  "build1_with_buildsink produces build log", {
  curwd = getwd()
  td = tempdir()
  setwd(td)
  getpk("graph")
  expect_true(dir.exists("graph"))
  expect_true(file.exists("graph/DESCRIPTION"))
  bb = build1_with_buildsink("graph", ".")
  expect_true(file.exists("graph.bldlog.txt"))
  setwd(curwd)
})


test_that("sqlite builds", {
   packs = c("parody", "vsn", "eds")
   
   library(BiocBuildTools)
   civ = Sys.getenv("CI")
   stopifnot(civ=="true")
   
   ps = PackageSet(packs, biocversion="3.16", branchname="master")
   ps = add_dependencies(ps)
   
   td = tempfile("litdemo")
   dir.create(td)
   
   populate_local_gits(ps, td)
   
   chkdest <<- tempfile("litrchks")
   dir.create(chkdest)
   
   bdest <<- tempfile("litbcchks")
   dir.create(bdest)
   
   bobdest <<- tempfile("litbobjs")
   dir.create(bobdest)
   
   library(BiocParallel)
   spar = MulticoreParam(4)
   register(spar)
   
   get_checks(ps, sources.folder=td, checks.destination=chkdest, 
       BPOPTIONS=bpoptions(exports=c("chkdest", "bdest", "bobdest")))
   expect_true(length(dir(chkdest))>0)
   
   get_bcc(sources.folder=td, bcchecks.destination=bdest, bcobj.destination=bobdest,
      BPOPTIONS=bpoptions(exports=c("chkdest", "bdest", "bobdest")))
   expect_true(length(dir(bobdest))>0)
  # 
   tsql = tempfile("tmp.sqlite")
   build_sqlite_db(tsql, rcmd=chkdest, bcc=bobdest)
   expect_true(file.exists(tsql))
   expect_error(build_sqlite_db(tsql, rcmd=chkdest, bcc=bobdest))
  # rm(chkdest)
  # rm(bdest)
  # rm(bobdest)
})
   
