linreg <- function (formula, data) {
  stopifnot((class(formula)=="formula") && (class(data)=="data.frame"))


  # uses model.matrix and all.vars on data and formula, and then calculates the regressions coefficients.

  X <- model.matrix(formula, data)

  y_namn <- all.vars(formula, max.names=1L)

  y <- data[[y_namn]]

  del_a <- t(X) %*% X
  del_b <- solve(del_a)  # solve(A) 	Inverse of A where A is a square matrix.

  reg_coef <- del_b %*% t(X) %*% y  # "Regressions coefficients"

  fitted_values <- X %*% reg_coef  # "The fitted values"

  resi <- as.vector(y - fitted_values) # "The residuals"

  n <- length(y)
  p <- length(data)
  deg_free <- n - p         # "The degrees of freedom"

  res_var <- as.vector((t(resi) %*% resi) / deg_free)  # "The residual variance"

  var_reg_coef <- res_var * del_b  # "The variance of the regression coefficients"

  t_each_coef <- reg_coef / sqrt(diag(var_reg_coef))    # "The t-values for each coefficient"

  p_values <- 1-pt(abs(t_each_coef), df = deg_free)   # "p-values for the regressions coefficients"


  ret <- list()
  class(ret) <- "linreg"
  ret$formula <- formula
  ret$data <- data
  ret$data_name <- deparse(substitute(data))
  ret$reg_coef <- reg_coef[,1]
  ret$fitted_values <- as.vector(fitted_values)
  ret$resi <- resi
  ret$deg_free <- deg_free
  ret$res_var <- res_var
  ret$var_reg_coef <- sqrt(diag(var_reg_coef))
  ret$t_each_coef <- t_each_coef
  ret$p_values <- p_values
  return(ret)
}