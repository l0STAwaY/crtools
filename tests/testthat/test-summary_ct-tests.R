library(testthat)
library(crtools)

test_that("no grouping works", {
  df <- data.frame(x = c(1,2,3,4,5))
  res <- summary_ct(df, "x")
  expect_equal(res$Mean, 3)
  expect_equal(res$Min., 1)
  expect_equal(res$Max., 5)
})

test_that("quantiles correct", {
  df <- data.frame(x = c(1,2,3,4,5))
  res <- summary_ct(df, "x")
  expect_equal(res$Median, 3)
  expect_equal(res$`1st Qu.`, 2)
  expect_equal(res$`3rd Qu.`, 4)
})

# This is a easy case there could be some minor cases such 1/3 that has some floating point differs

test_that("zero proportion", {
  df <- data.frame(x = c(0,0,0,1,2))
  res <- summary_ct(df, "x")
  expect_equal(res$zero_prop, 0.6)
})

test_that("grouping works", {
  df <- data.frame(x = c(1,2,3,4,5), g = c("a","a","b","b","b"))
  res <- summary_ct(df, "x", group = "g")
  expect_equal(nrow(res), 2)
})


# When there is NA by default we should set na.rm equal true and it should not be NA 
test_that("handles NA when na.rm=TRUE by default", {
  df <- data.frame(x = c(1, NA, 3))
  res <- summary_ct(df, "x")
  expect_true(!is.na(res$Mean))
})


