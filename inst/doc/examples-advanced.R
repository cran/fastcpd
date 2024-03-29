## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(fastcpd)

## ----package_installation-----------------------------------------------------
for (package in c("ggplot2", "mvtnorm")) {
  if (!requireNamespace(package, quietly = TRUE)) utils::install.packages(
    package, repos = "https://cloud.r-project.org", quiet = TRUE
  )
}

## ----lasso_setup--------------------------------------------------------------
set.seed(1)
n <- 1500
p_true <- 6
p <- 50
x <- mvtnorm::rmvnorm(480, rep(0, p), diag(p))
theta_0 <- rbind(
  runif(p_true, -5, -2),
  runif(p_true, -3, 3),
  runif(p_true, 2, 5),
  runif(p_true, -5, 5)
)
theta_0 <- cbind(theta_0, matrix(0, ncol = p - p_true, nrow = 4))
y <- c(
  x[1:80, ] %*% theta_0[1, ] + rnorm(80, 0, 1),
  x[81:200, ] %*% theta_0[2, ] + rnorm(120, 0, 1),
  x[201:320, ] %*% theta_0[3, ] + rnorm(120, 0, 1),
  x[321:480, ] %*% theta_0[4, ] + rnorm(160, 0, 1)
)

## ----lasso--------------------------------------------------------------------
result <- fastcpd(
  formula = y ~ . - 1,
  data = data.frame(y = y, x = x),
  family = "lasso",
  r.progress = FALSE
)
summary(result)

## ----lasso_multiple_epochs_setup, include = FALSE-----------------------------
set.seed(1)
n <- 1500
p_true <- 6
p <- 50
x <- mvtnorm::rmvnorm(480, rep(0, p), diag(p))
theta_0 <- rbind(
  runif(p_true, -5, -2),
  runif(p_true, -3, 3),
  runif(p_true, 2, 5),
  runif(p_true, -5, 5)
)
theta_0 <- cbind(theta_0, matrix(0, ncol = p - p_true, nrow = 4))
y <- c(
  x[1:80, ] %*% theta_0[1, ] + rnorm(80, 0, 1),
  x[81:200, ] %*% theta_0[2, ] + rnorm(120, 0, 1),
  x[201:320, ] %*% theta_0[3, ] + rnorm(120, 0, 1),
  x[321:480, ] %*% theta_0[4, ] + rnorm(160, 0, 1)
)

## ----lasso_multiple_epochs----------------------------------------------------
result_multiple_epochs <- fastcpd(
  formula = y ~ . - 1,
  data = data.frame(y = y, x = x),
  family = "lasso",
  multiple_epochs = function(segment_length) if (segment_length < 20) 1 else 0,
  r.progress = FALSE
)
summary(result_multiple_epochs)

## ----lasso_vanilla_percentage_setup, include = FALSE--------------------------
set.seed(1)
n <- 1500
p_true <- 6
p <- 50
x <- mvtnorm::rmvnorm(480, rep(0, p), diag(p))
theta_0 <- rbind(
  runif(p_true, -5, -2),
  runif(p_true, -3, 3),
  runif(p_true, 2, 5),
  runif(p_true, -5, 5)
)
theta_0 <- cbind(theta_0, matrix(0, ncol = p - p_true, nrow = 4))
y <- c(
  x[1:80, ] %*% theta_0[1, ] + rnorm(80, 0, 1),
  x[81:200, ] %*% theta_0[2, ] + rnorm(120, 0, 1),
  x[201:320, ] %*% theta_0[3, ] + rnorm(120, 0, 1),
  x[321:480, ] %*% theta_0[4, ] + rnorm(160, 0, 1)
)

## ----lasso_vanilla_percentage-------------------------------------------------
result_vanilla_percentage <- fastcpd(
  formula = y ~ . - 1,
  data = data.frame(y = y, x = x),
  family = "lasso",
  vanilla_percentage = 0.2,
  r.progress = FALSE
)
summary(result_vanilla_percentage)

## ----linear_regression_multi_dimensional_setup--------------------------------
set.seed(1)
n <- 300
p <- 3
y_count <- 2
x <- mvtnorm::rmvnorm(n, rep(0, p), diag(p))
theta_0 <- array(NA, dim = c(3, y_count, 3))
theta_0[, , 1] <- cbind(c(1, 1.2, -1), c(-1, 0, 0.5))
theta_0[, , 2] <- cbind(c(-1, 0, 0.5), c(0.5, -0.3, 0.2))
theta_0[, , 3] <- cbind(c(0.5, -0.3, 0.2), c(1, 1.2, -1))
y <- rbind(
  x[1:100, ] %*% theta_0[, , 1],
  x[101:200, ] %*% theta_0[, , 2],
  x[201:n, ] %*% theta_0[, , 3]
) + matrix(rnorm(n * y_count), ncol = y_count)
multi_response_linear_loss <- function(data) {
  x <- data[, (ncol(data) - p + 1):ncol(data)]
  y <- data[, 1:(ncol(data) - p)]

  if (nrow(data) <= p) {
    x_t_x <- diag(p)
  } else {
    x_t_x <- crossprod(x)
  }

  norm(y - x %*% solve(x_t_x, t(x)) %*% y, type = "F")^2 / 2
}
result <- fastcpd(
  formula = y ~ x - 1,
  data = data.frame(y = y, x = x),
  beta = (2 * p + 1) * log(n) / 2,
  cost = multi_response_linear_loss,
  cp_only = TRUE,
  r.progress = FALSE
)

testthat::expect_equal(result@cp_set, c(102, 195))

## ----ref.label = knitr::all_labels(), echo = TRUE, eval = FALSE---------------
#  knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
#  library(fastcpd)
#  for (package in c("ggplot2", "mvtnorm")) {
#    if (!requireNamespace(package, quietly = TRUE)) utils::install.packages(
#      package, repos = "https://cloud.r-project.org", quiet = TRUE
#    )
#  }
#  set.seed(1)
#  n <- 1500
#  p_true <- 6
#  p <- 50
#  x <- mvtnorm::rmvnorm(480, rep(0, p), diag(p))
#  theta_0 <- rbind(
#    runif(p_true, -5, -2),
#    runif(p_true, -3, 3),
#    runif(p_true, 2, 5),
#    runif(p_true, -5, 5)
#  )
#  theta_0 <- cbind(theta_0, matrix(0, ncol = p - p_true, nrow = 4))
#  y <- c(
#    x[1:80, ] %*% theta_0[1, ] + rnorm(80, 0, 1),
#    x[81:200, ] %*% theta_0[2, ] + rnorm(120, 0, 1),
#    x[201:320, ] %*% theta_0[3, ] + rnorm(120, 0, 1),
#    x[321:480, ] %*% theta_0[4, ] + rnorm(160, 0, 1)
#  )
#  result <- fastcpd(
#    formula = y ~ . - 1,
#    data = data.frame(y = y, x = x),
#    family = "lasso",
#    r.progress = FALSE
#  )
#  summary(result)
#  set.seed(1)
#  n <- 1500
#  p_true <- 6
#  p <- 50
#  x <- mvtnorm::rmvnorm(480, rep(0, p), diag(p))
#  theta_0 <- rbind(
#    runif(p_true, -5, -2),
#    runif(p_true, -3, 3),
#    runif(p_true, 2, 5),
#    runif(p_true, -5, 5)
#  )
#  theta_0 <- cbind(theta_0, matrix(0, ncol = p - p_true, nrow = 4))
#  y <- c(
#    x[1:80, ] %*% theta_0[1, ] + rnorm(80, 0, 1),
#    x[81:200, ] %*% theta_0[2, ] + rnorm(120, 0, 1),
#    x[201:320, ] %*% theta_0[3, ] + rnorm(120, 0, 1),
#    x[321:480, ] %*% theta_0[4, ] + rnorm(160, 0, 1)
#  )
#  result_multiple_epochs <- fastcpd(
#    formula = y ~ . - 1,
#    data = data.frame(y = y, x = x),
#    family = "lasso",
#    multiple_epochs = function(segment_length) if (segment_length < 20) 1 else 0,
#    r.progress = FALSE
#  )
#  summary(result_multiple_epochs)
#  set.seed(1)
#  n <- 1500
#  p_true <- 6
#  p <- 50
#  x <- mvtnorm::rmvnorm(480, rep(0, p), diag(p))
#  theta_0 <- rbind(
#    runif(p_true, -5, -2),
#    runif(p_true, -3, 3),
#    runif(p_true, 2, 5),
#    runif(p_true, -5, 5)
#  )
#  theta_0 <- cbind(theta_0, matrix(0, ncol = p - p_true, nrow = 4))
#  y <- c(
#    x[1:80, ] %*% theta_0[1, ] + rnorm(80, 0, 1),
#    x[81:200, ] %*% theta_0[2, ] + rnorm(120, 0, 1),
#    x[201:320, ] %*% theta_0[3, ] + rnorm(120, 0, 1),
#    x[321:480, ] %*% theta_0[4, ] + rnorm(160, 0, 1)
#  )
#  result_vanilla_percentage <- fastcpd(
#    formula = y ~ . - 1,
#    data = data.frame(y = y, x = x),
#    family = "lasso",
#    vanilla_percentage = 0.2,
#    r.progress = FALSE
#  )
#  summary(result_vanilla_percentage)
#  set.seed(1)
#  n <- 300
#  p <- 3
#  y_count <- 2
#  x <- mvtnorm::rmvnorm(n, rep(0, p), diag(p))
#  theta_0 <- array(NA, dim = c(3, y_count, 3))
#  theta_0[, , 1] <- cbind(c(1, 1.2, -1), c(-1, 0, 0.5))
#  theta_0[, , 2] <- cbind(c(-1, 0, 0.5), c(0.5, -0.3, 0.2))
#  theta_0[, , 3] <- cbind(c(0.5, -0.3, 0.2), c(1, 1.2, -1))
#  y <- rbind(
#    x[1:100, ] %*% theta_0[, , 1],
#    x[101:200, ] %*% theta_0[, , 2],
#    x[201:n, ] %*% theta_0[, , 3]
#  ) + matrix(rnorm(n * y_count), ncol = y_count)
#  multi_response_linear_loss <- function(data) {
#    x <- data[, (ncol(data) - p + 1):ncol(data)]
#    y <- data[, 1:(ncol(data) - p)]
#  
#    if (nrow(data) <= p) {
#      x_t_x <- diag(p)
#    } else {
#      x_t_x <- crossprod(x)
#    }
#  
#    norm(y - x %*% solve(x_t_x, t(x)) %*% y, type = "F")^2 / 2
#  }
#  result <- fastcpd(
#    formula = y ~ x - 1,
#    data = data.frame(y = y, x = x),
#    beta = (2 * p + 1) * log(n) / 2,
#    cost = multi_response_linear_loss,
#    cp_only = TRUE,
#    r.progress = FALSE
#  )
#  
#  testthat::expect_equal(result@cp_set, c(102, 195))

