library(linregpackage)
context("tests the methods coefficients, resid and pred")

data(faithful)
formula <- eruptions ~ waiting
data <- faithful

m1 = linreg(formula, data)
m2 = lm(formula, data)


test_that("Coefficients", {
  expect_that(length(coef(m1)), equals(length(coef(m2)), tolerance  = 0.01))
  expect_that(coef(m1), equals(coef(m2), tolerance  = 0.01))
})

test_that("Residuals", {
  expect_that(length(residuals(m1)), equals(length(residuals(m2)), tolerance  = 0.01))
  expect_that(residuals(m1), equals(residuals(m2), tolerance = 0.01))
})

test_that("Predictions", {
  expect_that(length(predict(m1)), equals(length(predict(m2)), tolerance  = 0.01))
  expect_that((predict(m2)-predict(m1)), equals(0, tolerance = 0.01))
})
