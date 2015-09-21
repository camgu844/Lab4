# code for the different methods

# A function that creates a data frame from an object of class linreg.

as.data.frame.linreg <- function(x) {
  datafr <- data.frame(fitted=x$fitted_values, residuals=x$resi)
  return(datafr)
}

# Method for printing linreg objects.

# The plot-method that creates two plots.

plot.linreg <- function(y, ...) {
  form_temp <- as.character(y$formula)
  form <- paste("linreg(",form_temp[2],form_temp[1],form_temp[3],")")
  z <- as.data.frame(y)
  ggplot(z) +
    geom_point(shape=1, size=5, aes(x=fitted, y=residuals)) +
    xlab(paste("Fitted values",form, sep="\n")) +
    ylab("Residuals") +
    ggtitle("Residuals vs Fitted") +
    geom_text(aes(label = tail(z$residuals,1), x=max(z$fitted), y=max(z$residuals)), hjust=1.5, size = 3)

  mod_residuals <- sqrt(abs(z$residuals / sqrt(y$res_var)))  # squareroot of abs of standardized residuals. Standardized residuals = residual / sqrt of residual variance
  z[,2] <- mod_residuals
  colnames(z)[2] <- "mod_residuals"
  ggplot(z) +
    geom_point(shape=1, size=5, aes(x=fitted, y=mod_residuals)) +
    xlab(paste("Fitted values",form, sep="\n")) +
    ylab("sqrt(|Standardized residuals|") +
    ggtitle("Scale-Location")

}


# A method that print the summary
print.linreg = function(o){
	formula_str = Reduce(paste, deparse(o$formula))
	vars = all.vars(o$formula)
	vars = vars[2:length(vars)]
	vars = c("(Intercept)", vars)
	coefs = round(o$reg_coef, 5)
	names(coefs) = vars

	cat("Call:\n")
	cat(paste("linreg(",formula_str,")\n\n"))
	cat("Coefficients:\n")
	print(coefs)
    # cat(paste(vars , sep="   ",collapse = "\t"))
    # cat(paste(coef, sep="   ",collapse = "\t"))
}

# A method that returns the vector of residuals e.
residuals.linreg <- function(o){
	return(o$resi)
}

# A method that returns the predicted values y.
predict.linreg <- function(o){
	return(o$fitted_values)
}

# A method that returns the coefficients as a named vector.
coef.linreg <- function(o){
	vars = all.vars(o$formula)
	vars = vars[2:length(vars)]
	vars = c("(Intercept)", vars)
	coefs = o$reg_coef
	names(coefs) = vars
	return(coefs)
}

# A method that prints a summary of the linreg-function.
summary.linreg <- function(o){
	vars = all.vars(o$formula)
	vars = vars[2:length(vars)]
	vars = c("(Intercept)", vars)

	tmp = c(o$reg_coef,o$var_reg_coef,o$t_each_coef,o$p_values)
	tmp = matrix(tmp,nrow=length(o$reg_coef))
	rownames(tmp) = vars
	colnames(tmp) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

	summary = list()
	summary$Coefficients = tmp
	summary$df = o$deg_free
	summary$rse = sqrt(o$res_var)
	summary$formula = Reduce(paste, deparse(o$formula))
	class(summary) <- 'linreg_summary'
	return(summary)
}
print.linreg_summary <- function(o){

	cat("Call:\n")
	cat(paste("linreg(",o$formula,")\n\n"))

	cat('Coefficients: \n')
	print(o$Coefficients)
	cat('\n')

	cat(paste(
		'Residual standard error:',
		round(o$rse,4),
		'on',
		o$df,
		'degrees of freedom'
		))
	cat('\n')
}

formula <- eruptions ~ waiting
data <- faithful
# attach(faithful)
m1 = linreg(eruptions~waiting, faithful)
m2 = lm(eruptions~waiting, faithful)
# print(summary(tmp))
# print(summary(l))
