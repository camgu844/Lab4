library(linregpackage)
context("tests the function linreg")

formula <- eruptions ~ waiting
data <- faithful

test_that("return values are correct", {
  expect_that(linreg(formula, data), equals(lm(formula, data)))
  expect_that(class(linreg(formula, data)), equals("linreg"))
})

