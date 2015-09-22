# code for the different methods
# x <- linreg(formula = Petal.Length~Species, data = iris)

# A function that creates a data frame from an object of class linreg.

as.data.frame.linreg <- function(x, ...) {
  datafr <- data.frame(fitted=x$fitted_values, residuals=x$resi)
  return(datafr)
}

# Method for printing the coefficients and coefficient names.

print.linreg = function(x, ...){
  data_name <- x$data_name
  formula_str = Reduce(paste, deparse(x$formula))
  cat("Call:\n")
  cat(paste("linreg(formula = ",formula_str,",data = ", data_name, ")\n\n"))
  cat("Coefficients:\n")
  print(x$reg_coef)
}

# The plot-method that creates two plots.

plot.linreg <- function(x, ...) {
  form_temp <- as.character(x$formula)
  form <- paste("linreg(",form_temp[2],form_temp[1],form_temp[3],")")
  z <- as.data.frame(x)
  ggplot(z) +
    geom_point(shape=1, size=5, aes(x=fitted, y=residuals)) +
    xlab(paste("Fitted values",form, sep="\n")) +
    ylab("Residuals") +
    ggtitle("Residuals vs Fitted") +
    geom_text(aes(label = tail(z$residuals,1), x=max(z$fitted), y=max(z$residuals)), hjust=1.5, size = 3)

  devAskNewPage()

  mod_residuals <- sqrt(abs(z$residuals / sqrt(x$res_var)))  # squareroot of abs of standardized residuals. Standardized residuals = residual / sqrt of residual variance
  z[,2] <- mod_residuals
  colnames(z)[2] <- "mod_residuals"
  ggplot(z) +
    geom_point(shape=1, size=5, aes(x=fitted, y=mod_residuals)) +
    xlab(paste("Fitted values",form, sep="\n")) +
    ylab("sqrt(|Standardized residuals|") +
    ggtitle("Scale-Location")

}


# A method that returns the vector of residuals e.
residuals.linreg <- function(object, ...){
	return(object$resi)
}

# A method that returns the predicted values y.
predict.linreg <- function(object, ...){
	return(object$fitted_values)
}

# A method that returns the coefficients as a named vector.
coef.linreg <- function(object, ...){
	vars = all.vars(object$formula)
	vars = vars[2:length(vars)]
	vars = c("(Intercept)", vars)
	coefs = object$reg_coef
	names(coefs) = vars
	return(coefs)
}

# A method that prints a summary of the linreg-function.
summary.linreg <- function(object, ...){
	vars = all.vars(object$formula)
	vars = vars[2:length(vars)]
	vars = c("(Intercept)", vars)

	tmp = c(object$reg_coef,object$var_reg_coef,object$t_each_coef,object$p_values)
	tmp = matrix(tmp,nrow=length(object$reg_coef))
	rownames(tmp) = vars
	colnames(tmp) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

	summary = list()
	summary$Coefficients = tmp
	summary$df = object$deg_free
	summary$rse = sqrt(object$res_var)
	summary$formula = Reduce(paste, deparse(object$formula))
	class(summary) <- 'linreg_summary'
	return(summary)
}
print.linreg_summary <- function(x, ...){

	cat("Call:\n")
	cat(paste("linreg(",x$formula,")\n\n"))

	cat('Coefficients: \n')
	print(x$Coefficients)
	cat('\n')

	cat(paste(
		'Residual standard error:',
		round(x$rse,4),
		'on',
		x$df,
		'degrees of freedom'
		))
	cat('\n')
}

# formula <- eruptions ~ waiting
# data <- faithful
# attach(faithful2)
# m1 = linreg(eruptions~waiting, faithful)
# m2 = lm(eruptions~waiting, faithful)
# print(summary(tmp))
# print(summary(l))
