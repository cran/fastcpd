## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)
library(fastcpd)

## ----fastcpd_binomial---------------------------------------------------------
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
#  #> Warning: fit_glm: fitted probabilities numerically 0 or 1 occurred
#  
#  #> Warning: fit_glm: fitted probabilities numerically 0 or 1 occurred
#  
#  #> Warning: fit_glm: fitted probabilities numerically 0 or 1 occurred
#  
#  #> Warning: fit_glm: fitted probabilities numerically 0 or 1 occurred
#  summary(result)
#  #>
#  #> Call:
#  #> fastcpd.binomial(data = cbind(y, x), r.progress = FALSE, cost_adjustment = NULL)
#  #>
#  #> Change points:
#  #> 126
#  #>
#  #> Cost values:
#  #> 56.90525 30.76875
#  #>
#  #> Parameters:
#  #>    segment 1 segment 2
#  #> 1  0.7259293  1.878525
#  #> 2 -1.0294802  2.704376
#  #> 3  1.0576503  3.702310
#  #> 4 -0.8812767  2.258796
#  #> 5  0.2419351  2.524173

## ----custom_cost_functions----------------------------------------------------
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
#  #>
#  #> Call:
#  #> fastcpd(formula = y ~ . - 1, data = binomial_data, cost = logistic_loss,
#  #>     cost_gradient = logistic_gradient, cost_hessian = logistic_hessian,
#  #>     epsilon = 1e-05, r.progress = FALSE)
#  #>
#  #> Change points:
#  #> 22 125
#  #>
#  #> Parameters:
#  #>   segment 1  segment 2 segment 3
#  #> 1 -59.20045  0.8170446  1.902379
#  #> 2 -34.56676 -0.9600438  2.751578
#  #> 3 216.53373  0.9353306  3.734179
#  #> 4 -80.96420 -0.7393653  2.247423
#  #> 5  51.25224  0.1390591  2.535372

## ----ref.label = knitr::all_labels(), echo = TRUE-----------------------------
#  knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)
#  library(fastcpd)
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
#  #> Warning: fit_glm: fitted probabilities numerically 0 or 1 occurred
#  
#  #> Warning: fit_glm: fitted probabilities numerically 0 or 1 occurred
#  
#  #> Warning: fit_glm: fitted probabilities numerically 0 or 1 occurred
#  
#  #> Warning: fit_glm: fitted probabilities numerically 0 or 1 occurred
#  summary(result)
#  #>
#  #> Call:
#  #> fastcpd.binomial(data = cbind(y, x), r.progress = FALSE, cost_adjustment = NULL)
#  #>
#  #> Change points:
#  #> 126
#  #>
#  #> Cost values:
#  #> 56.90525 30.76875
#  #>
#  #> Parameters:
#  #>    segment 1 segment 2
#  #> 1  0.7259293  1.878525
#  #> 2 -1.0294802  2.704376
#  #> 3  1.0576503  3.702310
#  #> 4 -0.8812767  2.258796
#  #> 5  0.2419351  2.524173
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
#  #>
#  #> Call:
#  #> fastcpd(formula = y ~ . - 1, data = binomial_data, cost = logistic_loss,
#  #>     cost_gradient = logistic_gradient, cost_hessian = logistic_hessian,
#  #>     epsilon = 1e-05, r.progress = FALSE)
#  #>
#  #> Change points:
#  #> 22 125
#  #>
#  #> Parameters:
#  #>   segment 1  segment 2 segment 3
#  #> 1 -59.20045  0.8170446  1.902379
#  #> 2 -34.56676 -0.9600438  2.751578
#  #> 3 216.53373  0.9353306  3.734179
#  #> 4 -80.96420 -0.7393653  2.247423
#  #> 5  51.25224  0.1390591  2.535372

