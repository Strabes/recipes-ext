library(testthat)
library(tidymodels.ext)

context("params")

test_that('param ranges', {
  expect_equal(quant_param(c(.1, .9))$range,
               list(lower = 0.1, upper = 0.9))
})
