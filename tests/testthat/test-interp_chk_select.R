test_that("Interp_ct and chk_ct (no failures, warnings allowed)", {
  
  suppressWarnings({
    
    set.seed(123)
    
    # this is t
    

    
    data2 <- read.csv(testthat::test_path("test-data/McMillanAcheMonkeyTrips.csv"))
    
    

    # MAKE LOG-SAFE VARIABLES
    
    
    data2$x2 <- rnorm(nrow(data2), 10, 2)
    data2$vs <- as.factor(rbinom(nrow(data2), 1, 0.5))
    data2$am <- as.factor(rbinom(nrow(data2), 1, 0.5))

    

    data2$Kills <- abs(data2$Kills) + 1
    

    data2$TripDays <- abs(data2$TripDays) + 1
    

    
    zero_idx <- sample(1:nrow(data2), 10)
    data2$Age[zero_idx] <- 0
    
    # =========================================================
    # CONT × CAT (Kills × vs)
    # =========================================================
    
    m1 <- fit_ct(Age ~ Kills * vs, data2, "poisson")
    m1_off <- fit_ct(Age ~ Kills * vs + offset(log(TripDays)), data2, "poisson")
    m1_log <- fit_ct(Age ~ log(Kills) * vs, data2, "poisson")
    m1_off_log <- fit_ct(Age ~ log(Kills) * vs + offset(log(TripDays)), data2, "poisson")
    
    m2 <- fit_ct(Age ~ Kills * vs, data2, "negbin")
    m2_log <- fit_ct(Age ~ log(Kills) * vs, data2, "negbin")
    
    m3 <- fit_ct(Age ~ Kills * vs, data2, "qpoisson")
    m3_log <- fit_ct(Age ~ log(Kills) * vs, data2, "qpoisson")
    
    m4 <- fit_ct(Age ~ Kills * vs, data2, "zip")
    m4_log <- fit_ct(Age ~ log(Kills) * vs, data2, "zip")
    
    m5 <- fit_ct(Age ~ Kills * vs, data2, "zinb")
    m5_off <- fit_ct(Age ~ Kills * vs + offset(log(TripDays)), data2, "zinb")
    m5_log <- fit_ct(Age ~ log(Kills) * vs, data2, "zinb")
    m5_off_log <- fit_ct(Age ~ log(Kills) * vs + offset(log(TripDays)), data2, "zinb")
    
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
    
    m28_log <- fit_ct(Age ~ log(Kills) * x2 + (1 | PID) + offset(log(TripDays)), data2, "glmpoisson")
    
    m29_log <- fit_ct(Age ~ log(Kills) * x2 + (1 | PID) + offset(log(TripDays)), data2, "glmnb")
    
    m30_log <- fit_ct(Age ~ log(Kills) * vs + (1 | PID) + offset(log(TripDays)), data2, "glmpoisson")
    
    m31_log <- fit_ct(Age ~ log(Kills) * vs + (1 | PID) + offset(log(TripDays)), data2, "glmnb")
    
    m32_log <- fit_ct(Age ~ log(Kills) * vs + (1 | PID) + offset(log(TripDays)), data2, "glmpoisson")
    
    m33_log <- fit_ct(Age ~ log(Kills) * vs + (1 | PID) + offset(log(TripDays)), data2, "glmnb")
    # =========================================================
    # RUN INTERP + GGPLOT (ALL MODELS)
    # =========================================================
    
    all_models <- list(
      m1,m2,m3,m4,m5,
      m1_off,m1_log,m1_off_log,m2_log,m3_log,m4_log,m5_off,m5_log,m5_off_log,
      m6,m7,m8,m9,m10,
      m6_off,
      m11,m12,m13,m14,m15,
      m11_off,m15_off,
      m16,m17,m18,m19,m20,m21,
      m16_off,m17_off,m18_off,m19_off,m20_off,m21_off,
      m22,m23,m24,m25,m26,m27,
      m28,m29,m30,m31,m32,m33,m28_log,m29_log,m30_log,m31_log,m32_log,m33_log
    )
    
    for (m in all_models) {
      
      # checking this also check the ggemmeans
      interp_ct(m)
      chk_ct(m)

    }
    # no need check diag count if slect_Ct works diag count works
    # I test the most compcated formula should work for mix ed effect
    select_ct(formula(m33_log),data2)
    # this covers the cases where we are testing formula for non mixed effect model
    select_ct(formula(m18),data2)
    
  })
  

  
})