% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats_summary.R
\name{calc_stat_normal}
\alias{calc_stat_normal}
\alias{calc_stat_lognormal}
\title{Calculate summary metrics for a numeric vector}
\usage{
calc_stat_normal(x, ci = 0.95, na.rm = FALSE)

calc_stat_lognormal(x, ci = 0.95, na.rm = FALSE)
}
\arguments{
\item{x}{A numeric vector}
}
\value{
A dataframe containing summary metrics
}
\description{
Calculate summary metrics for a numeric vector
}
\section{Functions}{
\itemize{
\item \code{calc_stat_normal}: Calculate summary for normal distribution

\item \code{calc_stat_lognormal}: Calculate summary for lognormal distribution
}}

\examples{
calc_stat_normal(1:10)
calc_stat_lognormal(1:10)
}
