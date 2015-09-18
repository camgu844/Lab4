#tries to do the plot operations

data(iris)
mod_object <- lm(Petal.Length~Species, data=iris)
print(mod_object)

as.data.frame.linreg <- function(x) {
  df <- data.frame(fitted=x$fitted_values, residuals=x$resi)
  return(df)
}

plot.linreg <- function(y, ...) {
  y <- as.data.frame(y)
  ggplot(y) + geom_point(shape=1, size=10, aes(x=fitted, y=residuals)) + ggtitle("Residuals vs Fitted")
}

r <- linreg(Petal.Length~Species, data=iris)
plot.linreg(r)