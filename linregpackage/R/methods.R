#tries to do the plot operations

ggplot(mod_object) + geom_point(shape=1, size=10, aes(x=mod_object$fitted, y=mod_object$residual)) + ggtitle("Residuals vs Fitted")