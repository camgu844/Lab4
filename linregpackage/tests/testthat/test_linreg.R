library(linregpackage)
context("tests the function linreg")

formula <- eruptions ~ waiting
data <- faithful

test_that("return values are correct", {
  expect_that(linreg(formula, data), equals(lm(formula, data)))
  expect_that(class(linreg(formula, data)), equals("lm"))
})

#gör dessa test någon nytta? formula och data är ju angivna ovan
test_that("linreg has correct input values", {
  expect_that(class(data), equals("data.frame"))
  expect_that(class(formula), equals("formula"))
})

