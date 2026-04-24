#' Model Diagnostics for Count Models
#'
#' @param model A fitted count model object (from fit_ct)
#' @param B Number of bootstrap replications
#'
#' @returns A list with performance metrics, bootstrap CI, raw bootstrap draws, and model-based CI
#' @import performance
#' @export
diag_count <- function(model, B = 100){
  
  perf <- performance::model_performance(model)
  
  
  boot <- bootstrap_ct_model(model, B = B)
  
  
  
  # confint confidence interval of the coeffice
  ci_model <- tryCatch(
    confint(model),
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



# data <- read.csv("../Private_Dataset/McMillanAcheMonkeyTrips.csv")
# 
# 
# model <- fit_ct(
#   Kills ~ Age + offset(TripDays),
#   data = data,
#   family = "zip"
# )
# 
# str(performance::model_performance(model))
# 
# res <- diag_count(model, B = 200)
# 
# 
# # print(res$performance)
# # head(res$bootstrap_raw)
# # print(res$bootstrap_ci)
# # print(res$model_ci)


