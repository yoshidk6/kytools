set.seed(1234)
xx <- stats::rnorm(30, 5, 1)

xx.calc_stat_normal <- calc_stat_normal(xx)

test_that("CI matches with Rmisc", {
  ci.calc_stat_normal <- xx.calc_stat_normal$xx[c(5, 1, 4)]

  # https://rcompanion.org/handbook/C_03.html
  ci.rmisc <- Rmisc::CI(x, ci = 0.95)
  names(ci.rmisc) <- NULL

  expect_equal(ci.calc_stat_normal,
               ci.rmisc, tolerance = 0.001, scale = x)
})

test_that("error with calc_stat_lognormal if non-positive exist", {

})
