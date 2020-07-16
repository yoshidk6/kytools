#' Calculate summary metrics for a numeric vector
#'
#' @describeIn calc_stat_normal Calculate summary for normal distribution
#' @export
#' @param x A numeric vector
#' @return A dataframe containing summary metrics
#' @examples
#' calc_stat_normal(1:10)
calc_stat_normal <- function(x, ci = 0.95, na.rm = FALSE) {

  x_name <- rlang::quo_name(rlang::enquo(x))
  x_name_stat <- paste0(x_name, "_stat")

  if(na.rm) {
    x <- stats::na.omit(x)
  } else {
    if (any(is.na(x))) stop("Use `na.rm = TRUE` to ignore NA values")
  }

  # Calc CI: https://github.com/Novartis/xgxr/blob/master/R/xgx_conf_int.R
  percentile_value <- ci + (1 - ci) / 2

  mu  <- mean(x)
  sd  <- stats::sd(x)
  cv  <- sd / mu
  qtt <- stats::qt(percentile_value, length(x))
  s_v <- sqrt(stats::var(x) / length(x))

  tibble::tibble(!!x_name := c(mu, sd, cv,
                               mu - qtt * s_v,
                               mu + qtt * s_v),
                 !!x_name_stat := c("mean", "sd", "cv",
                                    "ci_lower", "ci_upper"))
}


#' @describeIn calc_stat_normal Calculate summary for lognormal distribution
#' @export
#' @examples
#' calc_stat_lognormal(1:10)
calc_stat_lognormal <- function(x, ci = 0.95, na.rm = FALSE) {

  x_name <- rlang::quo_name(rlang::enquo(x))
  x_name_stat <- paste0(x_name, "_stat")

  if (any(x <= 0, na.rm = TRUE)) stop("Input cannot contain non-positive numbers")

  if(na.rm) {
    x <- stats::na.omit(x)
  } else {
    if (any(is.na(x))) stop("Use `na.rm = TRUE` to ignore NA values")
  }

  # Calc CI: https://github.com/Novartis/xgxr/blob/master/R/xgx_conf_int.R
  # Calc geo CV https://github.com/billdenney/pknca/blob/master/R/002-pk.business.rules.R

  percentile_value <- ci + (1 - ci) / 2

  xlog <- log(x)

  mu  <- sum(xlog) / length(x)
  sd  <- stats::sd(xlog)
  cv  <- sqrt(exp(stats::sd(xlog) ^ 2) - 1)
  qtt <- stats::qt(percentile_value, length(xlog))
  s_v <- sqrt(stats::var(xlog) / length(xlog))

  tibble::tibble(!!x_name := c(exp(mu),
                               exp(sd),
                               cv,
                               exp(mu - qtt * s_v),
                               exp(mu + qtt * s_v)),
                 !!x_name_stat := c("geomean", "geosd", "geocv",
                                    "geoci_lower", "geoci_upper"))
}
