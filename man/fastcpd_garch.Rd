% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fastcpd_wrappers.R
\name{fastcpd_garch}
\alias{fastcpd_garch}
\alias{fastcpd.garch}
\title{Find change points efficiently in GARCH(p, q) models}
\usage{
fastcpd_garch(data, order = c(0, 0), ...)

fastcpd.garch(data, order = c(0, 0), ...)
}
\arguments{
\item{data}{A numeric vector, a matrix, a data frame or a time series object.}

\item{order}{A positive integer vector of length two specifying the order of
the GARCH model.}

\item{...}{Other arguments passed to \code{\link{fastcpd}}, for example,
\code{segment_count}.}
}
\value{
A class \code{fastcpd} object.
}
\description{
\code{"fastcpd_garch"} and \code{"fastcpd.garch"} are
wrapper functions of \code{\link{fastcpd}} to find change points in
GARCH(p, q) models. The function is similar to \code{"fastcpd"}
except that the data is by default a one-column matrix or univariate vector
and thus a formula is not required here.
}
\examples{
\donttest{
if (!requireNamespace("ggplot2", quietly = TRUE)) utils::install.packages(
  "ggplot2", repos = "https://cloud.r-project.org", quiet = TRUE
)

set.seed(1)
n <- 400
sigma_2 <- rep(1, n + 1)
x <- rep(0, n + 1)
for (i in seq_len(200)) {
  sigma_2[i + 1] <- 20 + 0.5 * x[i]^2 + 0.1 * sigma_2[i]
  x[i + 1] <- rnorm(1, 0, sqrt(sigma_2[i + 1]))
}
for (i in 201:400) {
  sigma_2[i + 1] <- 1 + 0.1 * x[i]^2 + 0.5 * sigma_2[i]
  x[i + 1] <- rnorm(1, 0, sqrt(sigma_2[i + 1]))
}
result <- suppressWarnings(
  fastcpd.garch(x[-1], c(1, 1), include.mean = FALSE)
)
summary(result)
plot(result)
}
}
