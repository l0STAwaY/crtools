test_that("Interp_ct and chk_ct checking if they still apply when response has NA (no failures, warnings allowed)", {
  
  # I didn't check the NA case for when the data is log not log friendly since 
  
  suppressWarnings({
    
    set.seed(123)
    
    # this is t
    
    
    
    data2 <- read.csv(testthat::test_path("test-data/McMillanAcheMonkeyTrips.csv"))
    
    # make a proper count outcome
    data2$x2 <- rnorm(n=nrow(data2),mean=10,sd=2)
    data2$vs <- as.factor(rbinom(n = nrow(data2), size = 1, prob = 0.5))
    data2$am <- as.factor(rbinom(n = nrow(data2), size = 1, prob = 0.5))
    # add zero inflation
    zero_idx <- sample(1:nrow(data2), size = 10)
    data2$Age[zero_idx] <- 0
    
    # =========================================================
    # CONT × CAT (Kills × vs)
    # =========================================================
    
    m1 <- fit_ct(Age ~ Kills * vs, data2, "poisson")
    m1_off <- fit_ct(Age ~ Kills * vs + offset(log(TripDays)), data2, "poisson")
    expect_error(m1_log <- fit_ct(Age ~ log(Kills) * vs, data2, "poisson"))
    expect_error(m1_off_log <- fit_ct(Age ~ log(Kills) * vs + offset(log(TripDays)), data2, "poisson"))
    
    m2 <- fit_ct(Age ~ Kills * vs, data2, "negbin")
    expect_error(m2_log <- fit_ct(Age ~ log(Kills) * vs, data2, "negbin"))
    
    m3 <- fit_ct(Age ~ Kills * vs, data2, "qpoisson")
    expect_error(m3_log <- fit_ct(Age ~ log(Kills) * vs, data2, "qpoisson"))
    
    m4 <- fit_ct(Age ~ Kills * vs, data2, "zip")
    expect_error(m4_log <- fit_ct(Age ~ log(Kills) * vs, data2, "zip"))
    
    m5 <- fit_ct(Age ~ Kills * vs, data2, "zinb")
    m5_off <- fit_ct(Age ~ Kills * vs + offset(log(TripDays)), data2, "zinb")
    expect_error(m5_log <- fit_ct(Age ~ log(Kills) * vs, data2, "zinb"))
    expect_error(m5_off_log <- fit_ct(Age ~ log(Kills) * vs + offset(log(TripDays)), data2, "zinb"))
    
    # =========================================================
    # CAT × CAT (vs × am)
    # =========================================================
    
    m6 <- fit_ct(Age ~ vs * am, data2, "poisson")
    m7 <- fit_ct(Age ~ vs * am, data2, "negbin")
    m8 <- fit_ct(Age ~ vs * am, data2, "qpoisson")
    m9 <- fit_ct(Age ~ vs * am, data2, "zip")
    m10 <- fit_ct(Age ~ vs * am, data2, "zinb")
    
    m6_off <- fit_ct(Age ~ vs * am + offset(log(TripDays)), data2, "poisson")
    
    
    # =========================================================
    # CONT × CONT (Kills × x2)
    # =========================================================
    
    m11 <- fit_ct(Age ~ Kills * x2, data2, "poisson")
    m12 <- fit_ct(Age ~ Kills * x2, data2, "negbin")
    m13 <- fit_ct(Age ~ Kills * x2, data2, "qpoisson")
    m14 <- fit_ct(Age ~ Kills * x2, data2, "zip")
    m15 <- fit_ct(Age ~ Kills * x2, data2, "zinb")
    
    m11_off <- fit_ct(Age ~ Kills * x2 + offset(log(TripDays)), data2, "poisson")
    m15_off <- fit_ct(Age ~ Kills * x2 + offset(log(TripDays)), data2, "zinb")
    
    
    # =========================================================
    # GLMM (no RE yet)
    # =========================================================
    
    m16 <- fit_ct(Age ~ Kills * x2, data2, "glmpoisson")
    m17 <- fit_ct(Age ~ Kills * x2, data2, "glmnb")
    
    m18 <- fit_ct(Age ~ vs * am, data2, "glmpoisson")
    m19 <- fit_ct(Age ~ vs * am, data2, "glmnb")
    
    m20 <- fit_ct(Age ~ Kills * vs, data2, "glmpoisson")
    m21 <- fit_ct(Age ~ Kills * vs, data2, "glmnb")
    
    
    # =========================================================
    # GLMM + OFFSET
    # =========================================================
    
    m16_off <- fit_ct(Age ~ Kills * x2 + offset(log(TripDays)), data2, "glmpoisson")
    m17_off <- fit_ct(Age ~ Kills * x2 + offset(log(TripDays)), data2, "glmnb")
    
    m18_off <- fit_ct(Age ~ vs * am + offset(log(TripDays)), data2, "glmpoisson")
    m19_off <- fit_ct(Age ~ vs * am + offset(log(TripDays)), data2, "glmnb")
    
    m20_off <- fit_ct(Age ~ Kills * vs + offset(log(TripDays)), data2, "glmpoisson")
    m21_off <- fit_ct(Age ~ Kills * vs + offset(log(TripDays)), data2, "glmnb")
    
    
    # =========================================================
    # RANDOM EFFECT MODELS
    # =========================================================
    
    m22 <- fit_ct(Age ~ Kills * x2 + (1 | vs), data2, "glmpoisson")
    m23 <- fit_ct(Age ~ Kills * x2 + (1 | PID), data2, "glmnb")
    
    m24 <- fit_ct(Age ~ vs * am + (1 | PID), data2, "glmpoisson")
    m25 <- fit_ct(Age ~ vs * am + (1 | PID), data2, "glmnb")
    
    m26 <- fit_ct(Age ~ Kills * vs + (1 | PID), data2, "glmpoisson")
    m27 <- fit_ct(Age ~ Kills * vs + (1 | PID), data2, "glmnb")
    
    
    # =========================================================
    # RANDOM EFFECT + OFFSET
    # =========================================================
    
    m28 <- fit_ct(Age ~ Kills * x2 + (1 | PID) + offset(log(TripDays)), data2, "glmpoisson")
    m29 <- fit_ct(Age ~ Kills * x2 + (1 | PID) + offset(log(TripDays)), data2, "glmnb")
    
    m30 <- fit_ct(Age ~ vs * am + (1 | PID) + offset(log(TripDays)), data2, "glmpoisson")
    m31 <- fit_ct(Age ~ vs * am + (1 | PID) + offset(log(TripDays)), data2, "glmnb")
    
    m32 <- fit_ct(Age ~ Kills * vs + (1 | PID) + offset(log(TripDays)), data2, "glmpoisson")
    m33 <- fit_ct(Age ~ Kills * vs + (1 | PID) + offset(log(TripDays)), data2, "glmnb")
    
    # =========================================================
    # RANDOM EFFECT + OFFSET + LOG
    # =========================================================
    
    expect_error(m28_log <- fit_ct(Age ~ log(Kills) * x2 + (1 | PID) + offset(log(TripDays)), data2, "glmpoisson"))
    
    expect_error(m29_log <- fit_ct(Age ~ log(Kills) * x2 + (1 | PID) + offset(log(TripDays)), data2, "glmnb"))
    
    expect_error(m30_log <- fit_ct(Age ~ log(Kills) * vs + (1 | PID) + offset(log(TripDays)), data2, "glmpoisson"))
    
    expect_error(m31_log <- fit_ct(Age ~ log(Kills) * vs + (1 | PID) + offset(log(TripDays)), data2, "glmnb"))
    
    expect_error(m32_log <- fit_ct(Age ~ log(Kills) * vs + (1 | PID) + offset(log(TripDays)), data2, "glmpoisson"))
    
    expect_error(m33_log <- fit_ct(Age ~ log(Kills) * vs + (1 | PID) + offset(log(TripDays)), data2, "glmnb"))
    # =========================================================
    # RUN INTERP + GGPLOT (ALL MODELS)
    # =========================================================
    
    all_models <- list(
      m1,m2,m3,m4,m5,
      m1_off,
      m6,m7,m8,m9,m10,
      m6_off,
      m11,m12,m13,m14,m15,
      m11_off,m15_off,
      m16,m17,m18,m19,m20,m21,
      m16_off,m17_off,m18_off,m19_off,m20_off,m21_off,
      m22,m23,m24,m25,m26,m27,
      m28,m29,m30,m31,m32,m33
    )
    
    for (m in all_models) {
      
      # checking this also check the ggemmeans
      expect_no_error(interp_ct(m))
      expect_no_error(chk_ct(m))
      
    }
    # no need check diag count if slect_Ct works diag count works
    # I test the most compcated formula should work for mix ed effect
    expect_no_error(select_ct(formula(m33),data2))
    # this covers the cases where we are testing formula for non mixed effect model
    expect_no_error(select_ct(formula(m18),data2))
    
  })
  
  
  
})