library(testthat)
library(dplyr)

test_that("basic summary works", {
  df <- data.frame(x = c(1,2,3,4,5))
  res <- summary_overall(df)
  expect_equal(res$Mean, 3)
  expect_equal(res$Min, 1)
  expect_equal(res$Max, 5)
})

test_that("quantiles correct", {
  df <- data.frame(x = c(1,2,3,4,5))
  res <- summary_overall(df)
  expect_equal(res$Median, 3)
  expect_equal(res$`1st Qu.`, 2)
  expect_equal(res$`3rd Qu.`, 4)
})

# floating point note still applies
test_that("variance correct", {
  df <- data.frame(x = c(1,2,3,4,5))
  res <- summary_overall(df)
  # expect equal allow some leeway compared to indentical
  expect_equal(res$variance, var(c(1,2,3,4,5)))
})

test_that("missing values counted", {
  df <- data.frame(x = c(1, NA, 3))
  res <- summary_overall(df)
  expect_equal(res$missing_values, 1)
})

# NA handling (na.rm = TRUE default)
test_that("handles NA properly for when na.rm = TRUE", {
  df <- data.frame(x = c(1, NA, 3))
  res <- summary_overall(df)
  expect_true(!is.na(res$Mean))
})



test_that("per-column mean works", {
  df <- data.frame(x = c(1,2,3), y = c(4,5,6))
  res <- summary_overall(df)
  expect_equal(res$Mean[res$variable == "x"], 2)
  expect_equal(res$Mean[res$variable == "y"], 5)
})

test_that("row count is correct", {
  df <- data.frame(
    x = c(1,2,3),
    y = c("a","b","c"),
    z = c(4,5,6)
  )
  res <- summary_overall(df)
  expect_equal(unique(res$rows), 3)
})


test_that("non-numeric columns ignored", {
  df <- data.frame(
    x = c(1,2,3),
    y = c("a","b","c"),
    z = c(4,5,6)
  )
  res <- summary_overall(df)
  expect_equal(nrow(res), 2)   # x and z only
  expect_equal(res$Min[res$variable == "x"], 1)
  expect_equal(res$Max[res$variable == "x"], 3)
  expect_equal(res$Min[res$variable == "z"], 4)
  expect_equal(res$Max[res$variable == "z"], 6)
})