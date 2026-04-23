#' Title
#'
#' @param model 
#'
#' @returns
#' @import performance
#' @export
#' 
#'
#' @examples
diag_count <- function(model, B = 100){
  
  # the AIC BIC and etc
  perf <- performance::model_performance(model)
  

  
  # boot confidence interval for the specific models
  boot <- bootstrap_count_model(model, B = B)
  
  # ---------------------------
  # 3. Confidence intervals (model-based)
  # ---------------------------
  ci_model <- tryCatch(
    confint(model),
    error = function(e) NULL
  )
  
  # emmean dissect
  # dissect the interaction for them
  
  # lrtest or voungtest for model comparison
  
  # ---------------------------
  # 4. Output
  # ---------------------------
  return(list(
    performance = perf,
    bootstrap_ci = boot$ci,
    bootstrap_raw = boot$raw,
    model_ci = ci_model
  ))
  
  
}





