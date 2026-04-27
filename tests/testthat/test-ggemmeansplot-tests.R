

test_that("ggemmeansplot returns ggplot objects for all models", {
  
  library(ggplot2)
  library(ggeffects)
  library(emmeans)
  library(pscl)
  
  set.seed(113)
  
  data <- mtcars
  data$vs <- as.factor(data$vs)
  data$am <- as.factor(mtcars$am)
  
  # make a proper count outcome
  data$y <- rpois(nrow(data), lambda = exp(scale(data$hp)))
  data$exposure <- data$hp + 1
  # add zero inflation
  zero_idx <- sample(1:nrow(data), size = 5)
  data$y[zero_idx] <- 0
  
  
  #===========================
  # cont x categorical poisson
  #===========================
  
  
  
  m1 <- fit_ct(y ~ hp * vs, data = data, family = "poisson")
  
  m2 <-  fit_ct(y ~ hp * vs, data = data, family = "negbin")
  
  
  m3 <-  fit_ct(y ~ hp * vs, data = data, family = "qpoisson")
  
  
  
  m4 <-  fit_ct(y ~ hp * vs, data = data, family = "zip")
  
  
  
  m5 <-  fit_ct(y ~ hp * vs, data = data, family = "zinb")
  
  
  
  
  
  # with offset one from each should be enough
  m1_off <- fit_ct(
    y ~ hp * vs + offset(log(exposure)),
    data = data,
    family = "poisson"
  )
  
  
  m5_off <- fit_ct(
    y ~ hp * vs + offset(log(exposure)),
    data = data,
    family = "zinb"
  )
  
  
  
  # categorical x categorical
  
  m6 <- fit_ct(y ~ vs * am, data = data, family = "poisson")
  
  m7 <- fit_ct(y ~ vs * am, data = data, family = "negbin")
  
  m8 <- fit_ct(y ~ vs * am, data = data, family = "qpoisson")
  
  
  m9 <-  fit_ct(y ~ vs * am, data = data, family = "zip")
  
  # I forgot the zinb case
  m15 <- fit_ct(y ~ vs * am, data = data, family = "zinb")
  
  
  
  m6_off <- fit_ct(y ~ vs * am + offset(log(exposure)), data = data, family = "poisson")
  
  
  
  
  m10 <- fit_ct(y ~ hp * mpg, data = data, family = "poisson")
  
  
  m11 <- fit_ct(y ~ hp * mpg, data = data, family = "negbin")
  
  
  m12 <- fit_ct(y ~ hp * mpg, data = data, family = "qpoisson")
  
  
  m13 <- fit_ct(y ~ hp * mpg, data = data, family = "zip")
  
  
  m14 <- fit_ct(y ~ hp * mpg, data = data, family = "zinb")
  
  
  
  m10_off <- fit_ct(
    y ~ hp * mpg + offset(log(exposure)),
    data = data,
    family = "poisson"
  )
  
  
  m14_off <- fit_ct(
    y ~ hp * mpg + offset(log(exposure)),
    data = data,
    family = "zinb"
  )
  
  # alll plots
  
  
  p1  <- ggemmeansplot(m1,  pred = "hp", moderator = "vs")
  p2  <- ggemmeansplot(m2,  pred = "hp", moderator = "vs")
  p3  <- ggemmeansplot(m3,  pred = "hp", moderator = "vs")
  p4  <- ggemmeansplot(m4,  pred = "hp", moderator = "vs")
  p5  <- ggemmeansplot(m5,  pred = "hp", moderator = "vs")
  
  p1o <- ggemmeansplot(m1_off, pred = "hp", moderator = "vs")
  p5o <- ggemmeansplot(m5_off, pred = "hp", moderator = "vs")
  
  p6  <- ggemmeansplot(m6,  pred = "vs", moderator = "am")
  p7  <- ggemmeansplot(m7,  pred = "vs", moderator = "am")
  p8  <- ggemmeansplot(m8,  pred = "vs", moderator = "am")
  p9  <- ggemmeansplot(m9,  pred = "vs", moderator = "am")
  p15 <- ggemmeansplot(m15, pred = "vs", moderator = "am")
  
  p6o <- ggemmeansplot(m6_off, pred = "vs", moderator = "am")
  
  p10 <- ggemmeansplot(m10, pred = "hp", moderator = "mpg")
  p11 <- ggemmeansplot(m11, pred = "hp", moderator = "mpg")
  p12 <- ggemmeansplot(m12, pred = "hp", moderator = "mpg")
  p13 <- ggemmeansplot(m13, pred = "hp", moderator = "mpg")
  p14 <- ggemmeansplot(m14, pred = "hp", moderator = "mpg")
  
  p10o <- ggemmeansplot(m10_off, pred = "hp", moderator = "mpg")
  p14o <- ggemmeansplot(m14_off, pred = "hp", moderator = "mpg")
  
  
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
  expect_s3_class(p4, "ggplot")
  expect_s3_class(p5, "ggplot")
  
  expect_s3_class(p1o, "ggplot")
  expect_s3_class(p5o, "ggplot")
  
  expect_s3_class(p6, "ggplot")
  expect_s3_class(p7, "ggplot")
  expect_s3_class(p8, "ggplot")
  expect_s3_class(p9, "ggplot")
  expect_s3_class(p15, "ggplot")
  
  expect_s3_class(p6o, "ggplot")
  
  expect_s3_class(p10, "ggplot")
  expect_s3_class(p11, "ggplot")
  expect_s3_class(p12, "ggplot")
  expect_s3_class(p13, "ggplot")
  expect_s3_class(p14, "ggplot")
  
  expect_s3_class(p10o, "ggplot")
  expect_s3_class(p14o, "ggplot")
})