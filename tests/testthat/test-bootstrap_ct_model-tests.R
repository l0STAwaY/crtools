test_that("bootstrap_ct_model runs on real dataset", {
  
  data <- read.csv(
    testthat::test_path("test-data/McMillanAcheMonkeyTrips.csv")
  )
  
  model <- fit_ct(
    Kills ~ Age + offset(TripDays),
    data = data,
    family = "zip"
  )
  
  res <- bootstrap_ct_model(model, B = 20)
  


  expect_s3_class(res$raw, "data.frame")
  expect_s3_class(res$ci, "data.frame")
  
  expect_no_error(
    bootstrap_ct_model(model, B = 10)
  )
})