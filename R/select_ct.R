select_ct <- function(formula, data, B = 100) {
  
  
  # safe_fit model here gives the error and fit the other models
  # this is here because some times solution dont converge
  # we sometimes face issue where by default the. zip for example can't converge and need a unique specified start value instead
  safe_fit <- function(fit_expr, model_name) {
    tryCatch(
      fit_expr,
      error = function(e) {
        warning(paste0(model_name, " failed: ", e$message))
        NULL
      }
    )
  }
  
  
  # this is a list of fitted model
  models <- list(
    poisson  = safe_fit(fit_ct(formula, data, family = "poisson"), "poisson"),
    qpoisson = safe_fit(fit_ct(formula, data, family = "qpoisson"), "qpoisson"),
    negbin   = safe_fit(fit_ct(formula, data, family = "negbin"), "negbin"),
    zip      = safe_fit(fit_ct(formula, data, family = "zip"), "zip"),
    zinb     = safe_fit(fit_ct(formula, data, family = "zinb"), "zinb")
  )
  

  
  # all the diagnostic
  # this is a list of list
  diags <- list(
    poisson = if (!is.null(models$poisson)) {
      diag_count(models$poisson, B = B)
    } else {
      NULL
    },
    
    qpoisson = if (!is.null(models$qpoisson)) {
      diag_count(models$qpoisson, B = B)
    } else {
      NULL
    },
    
    negbin = if (!is.null(models$negbin)) {
      diag_count(models$negbin, B = B)
    } else {
      NULL
    },
    
    zip = if (!is.null(models$zip)) {
      diag_count(models$zip, B = B)
    } else {
      NULL
    },
    
    zinb = if (!is.null(models$zinb)) {
      diag_count(models$zinb, B = B)
    } else {
      NULL
    }
  )
  
  print(class(diags$zinb))
  
  # sapply returns a vector
  # we get all columns that 
  # goes through each element(which is list) of diags if it is no return That element in the returned vector is false
  valid_names <- names(diags)[!sapply(diags, is.null)]

  perf_df <- dplyr::bind_rows(
    lapply(valid_names, function(name) diags[[name]]$performance)
  )

  perf_df$model <- valid_names
  

  # apply list get there performance
  # this returns a list 

  

  # lr test by level
  # aov also works
  lr_tests <- list()
  
  if (!is.null(models$poisson) && !is.null(models$negbin)) {
    lr_tests$poisson_vs_negbin <- lmtest::lrtest(models$poisson, models$negbin)
  }
  
  if (!is.null(models$poisson) && !is.null(models$zip)) {
    lr_tests$zip_vs_poisson <- lmtest::lrtest(models$poisson, models$zip)
  }
  
  if (!is.null(models$negbin) && !is.null(models$zinb)) {
    lr_tests$zinb_vs_negbin <- lmtest::lrtest(models$negbin, models$zinb)
  }
  
  
  
  p_nb  <- lr_tests$poisson_vs_negbin$`Pr(>Chisq)`[2]
  p_zip <- lr_tests$zip_vs_poisson$`Pr(>Chisq)`[2]
  p_zinb<- lr_tests$zinb_vs_negbin$`Pr(>Chisq)`[2]
  

   # best BIC
   best_model <- perf_df %>%
    dplyr::slice_min(BIC, n = 1, with_ties = FALSE) %>%
    dplyr::pull(model)
  
  recommendation <- list(
    best_model = best_model,
    reason = "Lowest BIC among fitted models"
  )
  

  list(
    models = models,
    diagnostics = diags,
    performance = perf_df,
    lr_tests = lr_tests,
    recommendation = recommendation
  )
}




data <- read.csv("../Private_Dataset/McMillanAcheMonkeyTrips.csv")


res <- select_ct(
  Kills ~ Age + offset(TripDays),
  data = data,
  B = 50
)


res$recommendation


head(res$performance)


res$lr_tests

names(res$models)