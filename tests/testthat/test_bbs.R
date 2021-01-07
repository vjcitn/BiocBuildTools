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
