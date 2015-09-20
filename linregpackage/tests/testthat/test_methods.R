library(linregpackage)
context("tests the methods coefficients, resid and pred")

formula <- eruptions ~ waiting
data <- faithful

test_that("return values are correct", {
  expect_that(class(coeff), equals("integer"|"numeric"))
  # not finished expect_that()
})

