fit_ct <- function(formula, data, family = "poisson") {
  
  if (family == "poisson") {
    model <- glm(formula, data = data, family = poisson())
    
  } else if (family == "qpoisson") {
    model <- glm(formula, data = data, family = quasipoisson())
    
  } else if (family == "negbin") {
    model <- MASS::glm.nb(formula, data = data)
    
  } else if (family == "zip") {
    model <- pscl::zeroinfl(formula, data = data, dist = "poisson")
    
  } else if (family == "zinb") {
    model <- pscl::zeroinfl(formula, data = data, dist = "negbin")
    
  } else {
    stop("Invalid model type")
  }
  
  model$ct_family = family
  
  return(model)
}




# # Poisson
# data(mpg, package = "ggplot2")
# 
# 
# 
# # Poisson
# fit_ct(cyl ~ displ, mpg, family  = "poisson")
# 
# 
# # Quasai Poisson
# fit_ct(cyl ~ displ, mpg, family  = "qpoisson")
# 
# # Negative Binomial
# fit_ct(cyl ~ displ, mpg, family  = "negbin")
# 
# 
# # Zero Inflation Poisson
# mpg_zip <- mpg 
# zero_idx <- sample(1:nrow(mpg_zip), size = 30)  # inject 5 zeros to the data
# mpg_zip$cyl[zero_idx] <- 0
# fit_ct(cyl ~ displ, mpg_zip, family  = "zip")
# 
# # Zero Inflation Negative Binomial
# 
# fit_ct(cyl ~ displ, mpg_zip, family  = "zinb")
