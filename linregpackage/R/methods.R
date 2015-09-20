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

r <- linreg(Petal.Length~Species, data=iris)
plot(r)

# A method that returns the vector of residuals e.

# A method that returns the predicted values y.

# A method that returns the coefficients as a named vector.

# A method that prints a summary of the linreg-function.