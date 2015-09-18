#tries to do the plot operations

setMethod('as.data.frame', 'linreg', function(x){
	df = data.frame(fitted_values=x$fitted_values,residuals=x$residuals)
	return(df)
})

setMethod('plot','linreg',function(x, ...){
	x = as.data.frame(x)
	ggplot(data=x) +
	geom_point(shape=1, size=10, aes(x=fitted_values, y=residuals)) + ggtitle("Residuals vs Fitted")
})

r = linreg(Sepal.Length ~ Sepal.Width, iris)
plot(r)