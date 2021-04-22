
test_that("box.scale computes, etc.", {
 bs = box.scale(15)
 expect_true( bs > 2.23 )
 expect_true( bs < 2.231 )
 hs = hamp.scale.3( 15, .05)
 expect_true(hs > 6.337 & hs < 6.338)
 expect_error({ hs <- hamp.scale.3( 15, .15)})
 expect_true(tukeyor(mtcars$mpg)[2] > 39.13)
})

test_that("calout.detect computes", {
 expect_warning({ x <- calout.detect(mtcars$mpg) })
 expect_true(is.na(x$ind))
 expect_true(is.na(x$val))
 x = calout.detect(c(mtcars$mpg,1000), method="GESD")
 expect_true(x$ind == 33)
 expect_true(x$val == 1000)
 x = calout.detect(c(mtcars$mpg,1000), method="shorth")
 expect_true(length(x$outlier.region)==2)
 expect_true(x$outlier.region[1] < -3.351)
 expect_true(x$outlier.region[2] > 40.05)
})

test_that("mv.calout.detect computes", {
  data(bushfire)
  x = mv.calout.detect(bushfire)
  expect_true(length(x$inds) == 5)
  expect_true(x$k == 18)
})
