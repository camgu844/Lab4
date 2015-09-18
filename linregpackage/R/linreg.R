linreg <- function (formula, data) {
  stopifnot((class(formula)=="formula") && (class(data)=="data.frame"))


  # uses model.matrix and all.vars on data and formula, and then calculates the regressions coefficients.
  # to test:
  # formula <- eruptions ~ waiting
  # data <- faithful
  # attach(faithful)
  X <- model.matrix(formula, data)
  X
  y_namn <- all.vars(formula, max.names=1L)
  y_namn
  y <- data[[y_namn]]
  y
  del_a <- t(X) %*% X
  del_b <- solve(del_a)  # solve(A) 	Inverse of A where A is a square matrix.

  reg_coef <- del_b %*% t(X) %*% y  # "Regressions coefficients"
  reg_coef

  fitted_values <- X %*% reg_coef  # "The fitted values"
  fitted_values

  resi <- as.vector(y - fitted_values) # "The residuals"
  resi

  n <- length(y)
  p <- length(data)
  deg_free <- n - p         # "The degrees of freedom"
  deg_free

  res_var <- as.vector((t(resi) %*% resi) / deg_free)  # "The residual variance"
  res_var

  var_reg_coef <- res_var * del_b  # "The variance of the regression coefficients"
  var_reg_coef

  t_each_coef <- reg_coef %/% diag(sqrtm(var_reg_coef))    # "The t-values for each coefficient"
  t_each_coef

  p_values <- pt(reg_coef, df = deg_free)   # "p-values for the regressions coefficients"

  lr <- setRefClass(
  	"linreg",
  	fields = list(
  			coefs = class(reg_coef),
  			fitted_values = class(fitted_values),
  			residuals = class(resi),
  			df = class(deg_free),
  			residuals_var = class(res_var),
  			coefs_var = class(var_reg_coef),
  			coefs_t = class(t_each_coef),
  			p_values = class(p_values)
  		)
  	)
  setMethod('show','linreg',function(object)print('something'))
  print(lr)

  ret <- lr$new()
  ret$coefs <- reg_coef
  ret$fitted_values <- fitted_values
  ret$residuals <- resi
  ret$df <- deg_free
  ret$residuals_var <- res_var
  ret$coefs_var <- var_reg_coef
  ret$coefs_t <- t_each_coef
  ret$p_values <- p_values
  return(ret)
}

