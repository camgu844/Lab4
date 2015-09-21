library(linregpackage)
context("tests the methods coefficients, resid and pred")

formula <- eruptions ~ waiting
data <- faithful

test_that("return values are of correct class", {
  expect_that(mode(coeff), equals("numeric"))
  expect_that(mode(resid), equals("numeric"))
  expect_that(mode(coeff), equals("numeric"))
})

