\name{linreg}
\alias{linreg}
\title{Multiple Linear Regression}
\usage{
linreg(formula, data)
}
\arguments{
\item{formula}{formula object, e.g. y ~ x}

\item{data}{data set used by the formula}

}
\value{
linreg object
}
\description{
Multiple Linear Regression using Least Square Estimations
}
\examples{
formula <- eruptions ~ waiting
data <- faithful
m1 = linreg(eruptions~waiting, faithful)

summary(m1)
# Call:
# linreg( eruptions ~ waiting )
#
# Coefficients:
#                Estimate  Std. Error   t value Pr(>|t|)
# (Intercept) -1.87401599 0.160143302 -11.70212        0
# waiting      0.07562795 0.002218541  34.08904        0
#
# Residual standard error: 0.4965 on 270 degrees of freedom
}