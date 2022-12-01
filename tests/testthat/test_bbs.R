library(BiocBuildTools)

context("try a build")


test_that(
  "build1_with_buildsink produces build log", {
  curwd = getwd()
  td = tempdir()
  setwd(td)
  if (!dir.exists("graph")){
   unlink("graph", recursive=TRUE)
   getpk("graph")
   }
  expect_true(dir.exists("graph"))
  expect_true(file.exists("graph/DESCRIPTION"))
  bb = build1_with_buildsink("graph", ".")
  expect_true(file.exists("graph.bldlog.txt"))
  setwd(curwd)
})



context("test comprehensive build from PackageSet")

test_that("check PackageSet works", {
 pset = PackageSet(c("parody", "eds"))
 srcdir = tempfile("demo_srcs")
 dir.create(srcdir)
 spar = BiocParallel::SnowParam(2)
 BiocParallel::bplog(spar) = TRUE
 BiocParallel::bpstopOnError(spar) = FALSE
 BiocParallel::bpexportvariables(spar) = TRUE
 BiocParallel::bpexportglobals(spar) = TRUE
 dir.create(ldir <- tempfile("chkps_logs"))
 system(paste("chmod 777", ldir))
 BiocParallel::bplogdir(spar) = ldir
 cptry = check_PackageSet(pset, srcdir=srcdir, BPPARAM=spar)
 expect_true("sqlite_target" %in% names(cptry))
#> library(RSQLite)
#1/0 packages newly attached/loaded, see sessionInfo() for details.
#> dbConnect(SQLite(), cptry$sqlite_target) -> con
#> dbReadTable(con, "basic")
#  package version date_commit date_check version.1
#1     eds   1.0.0  2022-11-01 2022-12-01     1.0.0
#2  parody  1.56.0  2022-11-01 2022-12-01    1.56.0
 con = RSQLite::dbConnect(RSQLite::SQLite(), cptry$sqlite_target)
 expect_true( nrow( RSQLite::dbReadTable(con, "basic") ) == 2 )
})
