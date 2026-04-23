
# all these test are essentailly test for checking if the class are correct or not
# the only other way to test the class is via the examples I examined visually
# what each of the following cases look like
# If interested documentation for class test case https://testthat.r-lib.org/reference/inheritance-expectations.html

test_that("boxplot without group returns correct ggplot class", {
  
  data(mpg, package = "ggplot2")
  
  p1 <- plot_ct(mpg, "cyl", type = "boxplot")
  
  expect_s3_class(p1, "ggplot")
})

test_that("boxplot with group returns correct ggplot class", {
  
  
  data(mpg, package = "ggplot2")
  
  p2 <- plot_ct(mpg, "cyl", group = "class", type = "boxplot")
  
  expect_s3_class(p2, "ggplot")
})

test_that("violin without group returns correct ggplot class", {
  
  data(mpg, package = "ggplot2")
  
  p3 <- plot_ct(mpg, "cyl", type = "violin")
  
  expect_s3_class(p3, "ggplot")
})

test_that("violin with group returns correct ggplot class", {
  
  data(mpg, package = "ggplot2")
  
  p4 <- plot_ct(mpg, "cyl", group = "class", type = "violin")
  
  expect_s3_class(p4, "ggplot")
})

test_that("barplot without group returns correct ggplot class", {
  
  data(mpg, package = "ggplot2")
  
  p5 <- plot_ct(mpg, "cyl", type = "barplot")
  
  expect_s3_class(p5, "ggplot")
})

test_that("barplot with group returns correct ggplot class", {
  
  data(mpg, package = "ggplot2")
  
  p6 <- plot_ct(mpg, "cyl", group = "class", type = "barplot")
  
  expect_s3_class(p6, "ggplot")
})