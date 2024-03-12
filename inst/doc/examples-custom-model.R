## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(fastcpd)

## ----package_installation-----------------------------------------------------
for (package in c("ggplot2", "mvtnorm")) {
  if (!requireNamespace(package, quietly = TRUE)) utils::install.packages(
    package, repos = "https://cloud.r-project.org", quiet = TRUE
  )
}

## ----fastcpd_binomial---------------------------------------------------------
set.seed(1)
x <- matrix(rnorm(1500, 0, 1), ncol = 5)
theta <- rbind(rnorm(5, 0, 1), rnorm(5, 2, 1))
y <- c(
  rbinom(125, 1, 1 / (1 + exp(-x[1:125, ] %*% theta[1, ]))),
  rbinom(175, 1, 1 / (1 + exp(-x[126:300, ] %*% theta[2, ])))
)
binomial_data <- data.frame(y = y, x = x)

result <- fastcpd.binomial(cbind(y, x), r.progress = FALSE, cost_adjustment = NULL)
summary(result)

## ----custom_cost_functions----------------------------------------------------
logistic_loss <- function(data, theta) {
  x <- data[, -1]
  y <- data[, 1]
  u <- x %*% theta
  nll <- -y * u + log(1 + exp(u))
  nll[u > 10] <- -y[u > 10] * u[u > 10] + u[u > 10]
  sum(nll)
}
logistic_gradient <- function(data, theta) {
  x <- data[nrow(data), -1]
  y <- data[nrow(data), 1]
  c(-(y - 1 / (1 + exp(-x %*% theta)))) * x
}
logistic_hessian <- function(data, theta) {
  x <- data[nrow(data), -1]
  prob <- 1 / (1 + exp(-x %*% theta))
  (x %o% x) * c((1 - prob) * prob)
}
result <- fastcpd(
  y ~ . - 1, binomial_data, epsilon = 1e-5, cost = logistic_loss,
  cost_gradient = logistic_gradient, cost_hessian = logistic_hessian,
  r.progress = FALSE
)
summary(result)

## ----ref.label = knitr::all_labels(), echo = TRUE, eval = FALSE---------------
#  knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
#  library(fastcpd)
#  for (package in c("ggplot2", "mvtnorm")) {
#    if (!requireNamespace(package, quietly = TRUE)) utils::install.packages(
#      package, repos = "https://cloud.r-project.org", quiet = TRUE
#    )
#  }
#  set.seed(1)
#  x <- matrix(rnorm(1500, 0, 1), ncol = 5)
#  theta <- rbind(rnorm(5, 0, 1), rnorm(5, 2, 1))
#  y <- c(
#    rbinom(125, 1, 1 / (1 + exp(-x[1:125, ] %*% theta[1, ]))),
#    rbinom(175, 1, 1 / (1 + exp(-x[126:300, ] %*% theta[2, ])))
#  )
#  binomial_data <- data.frame(y = y, x = x)
#  
#  result <- fastcpd.binomial(cbind(y, x), r.progress = FALSE, cost_adjustment = NULL)
#  summary(result)
#  logistic_loss <- function(data, theta) {
#    x <- data[, -1]
#    y <- data[, 1]
#    u <- x %*% theta
#    nll <- -y * u + log(1 + exp(u))
#    nll[u > 10] <- -y[u > 10] * u[u > 10] + u[u > 10]
#    sum(nll)
#  }
#  logistic_gradient <- function(data, theta) {
#    x <- data[nrow(data), -1]
#    y <- data[nrow(data), 1]
#    c(-(y - 1 / (1 + exp(-x %*% theta)))) * x
#  }
#  logistic_hessian <- function(data, theta) {
#    x <- data[nrow(data), -1]
#    prob <- 1 / (1 + exp(-x %*% theta))
#    (x %o% x) * c((1 - prob) * prob)
#  }
#  result <- fastcpd(
#    y ~ . - 1, binomial_data, epsilon = 1e-5, cost = logistic_loss,
#    cost_gradient = logistic_gradient, cost_hessian = logistic_hessian,
#    r.progress = FALSE
#  )
#  summary(result)

