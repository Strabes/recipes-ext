library(testthat)
library(recipes)
library(dplyr)
library(tidymodels.ext)

context("top_code_quant")

df <- tibble::tibble(
  x = seq(0,100,by=1),
  y = rep(c(1),101),
  z = seq(0,1,by=0.01)^2 + 3)

test_that("top coding by quantile",{
  rec_obj <- df |>
    recipe(y ~ .) |>
    step_top_code_quant(
      all_numeric_predictors(),prob = 0.9)

  rec_obj_prepped <- prep(rec_obj, training = df)

  res <- bake(rec_obj_prepped, df) |>
    select(x,y,z)

  answer <- df |>
    mutate(x = case_when(x > 90 ~ 90, T ~ x),
           z = case_when(z > 3.81 ~ 3.81, T ~z)) |>
    select(x,y,z)

  expect_equal(res,answer)
})

test_that("printing", {
  rec_obj <- df |>
    recipe(y ~ .)
  top_code <- rec_obj |>
    step_top_code_quant(
      all_numeric_predictors(),prob = 0.9)
  expect_output(print(top_code))
  expect_output(prep(top_code, training = df, verbose = TRUE))
})
