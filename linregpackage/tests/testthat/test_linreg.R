library(linregpackage)
context("tests the function linreg")

formula <- eruptions ~ waiting
data <- faithful

test_that("the function linreg has the correct return values", {
  expect_that(linreg(formula, data), equals(lm(formula, data)))
  expect_that(class(linreg(formula, data)), equals("lm"))
  expect_that
})

#gör dessa test någon nytta? formula och data är ju angivna ovan
test_that("linreg has correct input values", {
  expect_that(class(data)!="data.frame", throws_error())
  expect_that(class(formula)!="formula", throws_error())
})

