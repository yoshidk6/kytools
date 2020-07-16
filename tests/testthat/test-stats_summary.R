set.seed(1234)
xx <- stats::rnorm(30, 5, 1)

xx.calc_stat_normal <- calc_stat_normal(xx)
xx.calc_stat_lognormal <- calc_stat_lognormal(xx)

test_that("CI matches with Rmisc", {
  ci.calc_stat_normal <- xx.calc_stat_normal$xx[c(5, 1, 4)]

  # https://rcompanion.org/handbook/C_03.html
  ci.rmisc <- Rmisc::CI(xx, ci = 0.95)
  names(ci.rmisc) <- NULL

  expect_equal(ci.calc_stat_normal,
               ci.rmisc,
               tolerance = 0.001, scale = ci.rmisc)
})

test_that("geoCI matches with DescTools", {
  ci.calc_stat_lognormal <- xx.calc_stat_lognormal$xx[c(1, 4, 5)]

  # https://rcompanion.org/handbook/C_03.html
  ci.desctools <- DescTools::Gmean(xx, conf.level = 0.95)
  names(ci.desctools) <- NULL

  expect_equal(ci.calc_stat_lognormal,
               ci.desctools,
               tolerance = 0.001, scale = ci.desctools)
})

test_that("CV matches with PKNCA", {
  cv.calc_stat_normal <- xx.calc_stat_lognormal$xx[3]

  expect_equal(cv.calc_stat_normal,
               PKNCA::geocv(xx) / 100,
               tolerance = 0.001, scale = cv.calc_stat_normal)
})

test_that("error if NA or non-positive (for lognormal) exist", {
  expect_error(calc_stat_normal(c(1, NA)), "Use `na.rm = TRUE`")
  expect_error(calc_stat_lognormal(c(1, NA)), "Use `na.rm = TRUE`")
  expect_error(calc_stat_lognormal(c(1, 0)), "Input cannot contain non-positive numbers")
})
