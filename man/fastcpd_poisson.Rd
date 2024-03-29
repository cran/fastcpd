% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fastcpd_wrappers.R
\name{fastcpd_poisson}
\alias{fastcpd_poisson}
\alias{fastcpd.poisson}
\title{Find change points efficiently in Poisson regression models}
\usage{
fastcpd_poisson(data, ...)

fastcpd.poisson(data, ...)
}
\arguments{
\item{data}{A matrix or a data frame with the response variable as the first
column.}

\item{...}{Other arguments passed to \code{\link{fastcpd}}, for example,
\code{segment_count}.}
}
\value{
A class \code{fastcpd} object.
}
\description{
\code{"fastcpd_poisson"} and \code{"fastcpd.poisson"} are
wrapper functions of \code{\link{fastcpd}} to find change points in
Poisson regression models. The function is similar to \code{"fastcpd"}
except that the data is by default a matrix or data frame with the response
variable as the first column and thus a formula is not required here.
}
\examples{
\donttest{
for (package in c("ggplot2", "mvtnorm")) {
  if (!requireNamespace(package, quietly = TRUE)) utils::install.packages(
    package, repos = "https://cloud.r-project.org", quiet = TRUE
  )
}

set.seed(1)
n <- 1100
p <- 3
x <- mvtnorm::rmvnorm(n, rep(0, p), diag(p))
delta <- rnorm(p)
theta_0 <- c(1, 0.3, -1)
y <- c(
  rpois(500, exp(x[1:500, ] \%*\% theta_0)),
  rpois(300, exp(x[501:800, ] \%*\% (theta_0 + delta))),
  rpois(200, exp(x[801:1000, ] \%*\% theta_0)),
  rpois(100, exp(x[1001:1100, ] \%*\% (theta_0 - delta)))
)
result <- fastcpd.poisson(cbind(y, x))
summary(result)
plot(result)
}
}
