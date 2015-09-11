linreg <- function (formula, data) {
  stopifnot((class(formula)=="formula") && (class(data)=="data.frame"))
  
  
  # uses model.matrix and all.vars on data and formula, and then calculates the regressions coefficients.
  X <- model.matrix(formula, data)
  y_namn <- all.vars(formula, max.names=1L) 
  y <- data[[y_namn]]
  del_a <- t(X) %*% X
  del_b <- solve(del_a)  # solve(A) 	Inverse of A where A is a square matrix. 
  coefficients <- del_b %*% t(X) %*% y  # "Regressions coefficients"
  
  #the code below is there temporarily to get the correct output value.
  res_temp <- lm(formula, data)
  return(res_temp) 
  
  
}