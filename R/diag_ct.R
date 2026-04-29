#' Model Diagnostics for Count Models
#'
#' @param model A fitted count model object (from fit_ct)
#' @param B Number of bootstrap replications
#'
#' @returns A list with performance metrics, bootstrap CI, raw bootstrap draws, and model-based CI
#' @import performance
#' @export
  diag_ct <- function(model, B = 100){
  perf <- ct_performance_table(model)
  boot <- bootstrap_ct_model(model, B = B)
  fam <- get_ct_family(model)
  
  

  
  
  # confint confidence interval of the coeffice
  ci_model <- tryCatch({
    ci <- confint(model)
    term <- rownames(ci)
    ci <- as_tibble(ci)
    ci$term <-term
    # I am surprised that return(ci) actually break this
    # seems like return oonly preforms well in functions
    ci
    },
    error = function(e) {
      warning("confint(model) failed: ", e$message)
      NULL
    }
  )
  
  
  # this just prints everything autmatically
  # we might not want to print since it messes with select_ct output
  # print(perf)
  # 
  # if (!is.null(boot)) {
  #   print(boot$ci)
  # }
  # 
  # if (!is.null(ci_model)) {
  #   print(ci_model)
  # }
  # 
  
  # I'm thinking about returning an object as well as returning them
  list(
    performance   = perf,
    bootstrap_ci  = if (!is.null(boot)) boot$ci else NULL,
    bootstrap_raw = if (!is.null(boot)) boot$raw else NULL,
    model_ci      = ci_model
  )
}


ct_performance_table <- function(model){
  data <- model.frame(model)
  response <- all.vars(formula(model))[1]
  fam <- get_ct_family(model)
  
  null_model <- fit_ct(
    as.formula(paste(response, "~ 1")),
    data = data,
    family = fam
  )
  
  lr <-  lmtest::lrtest(null_model, model)
  
  
  # McFadden R2
  ll_null <- lr$LogLik[1] 
  ll_full <- lr$LogLik[2]

  
  r2_mcfadden <- if (is.na(ll_null) || is.na(ll_full)) {
    NA
  } else {
    1 - (ll_full / ll_null)
  }
  
  data.frame(
    AIC = AIC(model),
    AICc = AIC(model),
    BIC = BIC(model),
    logLik = ll_full,
    R2_mcfadden = r2_mcfadden
  )
}
