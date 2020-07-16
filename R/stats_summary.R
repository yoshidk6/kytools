#' Calculate summary metrics for a numeric vector
#'
#' This function assumes the numbers follow normal distribution
#'
#' @export
#' @param x A numeric vector
#' @return A dataframe containing summary metrics
#' @examples
#' add(1, 1)
#' add(10, 1)
calc_stat_normal <- function(x, ci = 0.95, na.rm = FALSE) {

  x_name <- rlang::quo_name(rlang::enquo(x))
  x_name_stat <- paste0(x_name, "_stat")

  ## Calc CI: https://github.com/Novartis/xgxr/blob/master/R/xgx_conf_int.R
  percentile_value <- ci + (1 - ci) / 2

  mu  <- mean(x, na.rm = na.rm)
  sd  <- stats::sd(x, na.rm = na.rm)
  cv  <- sd / mu
  qtt <- stats::qt(percentile_value, length(x))
  s_v <- sqrt(stats::var(x, na.rm = na.rm) / length(x))

  tibble::tibble(!!x_name := c(mu, sd, cv,
                               mu - qtt * s_v,
                               mu + qtt * s_v),
                 !!x_name_stat := c("mean", "sd", "cv",
                                    "ci_lower", "ci_upper"))
}
