fit_ct <- function(formula, data, type = "poisson") {
  
  if (type == "poisson") {
    model <- glm(formula, data = data, family = poisson())
    
  } else if (type == "qpoisson") {
    model <- glm(formula, data = data, family = quasipoisson())
    
  } else if (type == "negbin") {
    model <- MASS::glm.nb(formula, data = data)
    
  } else if (type == "zip") {
    model <- pscl::zeroinfl(formula, data = data, dist = "poisson")
    
  } else if (type == "zinb") {
    model <- pscl::zeroinfl(formula, data = data, dist = "negbin")
    
  } else {
    stop("Invalid model type")
  }
  
  perf <- performance::model_performance(model)
  
  
  return(list(
    model = model,
    summary = summary(model),
    performance = perf # aic for now. but we can return. model performance instead
  ))
}