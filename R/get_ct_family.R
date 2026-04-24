library("dplyr")
#' Get Count Model Family Type
#'
#' Identifies the type of count regression model and returns a standardized
#' family label used across the \code{crtools} package. 
#' 
#' This allows you to pass in both model you fit via fit_ct and also normal models to functions
#'
#' Supports:
#' \itemize{
#'   \item Poisson and quasi-Poisson GLMs
#'   \item Negative binomial models (\code{glm.nb})
#'   \item Zero-inflated Poisson and negative binomial models
#' }
#'
#' @param model A fitted model object. Supported classes include:
#'   \code{glm}, \code{glm.nb} (from MASS), and \code{zeroinfl} (from pscl).
#'
#' @return A character string indicating the model family:
#' \itemize{
#'   \item \code{"poisson"}
#'   \item \code{"qpoisson"}
#'   \item \code{"negbin"}
#'   \item \code{"zip"}
#'   \item \code{"zinb"}
#' }
#' Returns \code{NA} if the model type is unsupported.
#'
#' @export
#'
#' @examples
#' library(MASS)
#' library(pscl)
#'
#' df <- data.frame(x = rnorm(100), y = rpois(100, lambda = 2))
#'
#' m1 <- glm(y ~ x, data = df, family = poisson())
#' get_ct_family(m1)
#'
#' m2 <- glm.nb(y ~ x, data = df)
#' get_ct_family(m2)
#'
#' m3 <- zeroinfl(y ~ x, data = df, dist = "poisson")
#' get_ct_family(m3)
get_ct_family <- function(model) {
  model_name <- NULL

    if (inherits(model, "negbin")) {
      return("negbin")
      
    } else  if (inherits(model, "glm")) {
      model_name  <- model$family$family
      
      if(model_name=="poisson"){
        return("poisson")
      } 
      if(model_name=="quasipoisson"){
      return("qpoisson")
      }
    
  }  else if (inherits(model, "zeroinfl")) {
    if (model$dist == "poisson") {
      return("zip")
    } else if (model$dist == "negbin") {
      return("zinb")
    }
  }
  
  return(NA)
}





# 
# df <- tibble(
#   y = rpois(100, 2),
#   x = rnorm(100)
# )
# 
# zero_idx <- sample(1:nrow(df), size = 0.3 * nrow(df))
# df$y_zi <- df$y
# df$y_zi[zero_idx] <- 0
# 
# df[zero_idx]
# # GLM Poisson
# m_pois <- glm(y ~ x, data = df, family = poisson())
# 
# m_pois$family$family
# 
# # GLM Quasi-Poisson
# m_qpois <- glm(y ~ x, data = df, family = quasipoisson())
# 
# 
# m_qpois$family$family
# 
# # Negative Binomial
# m_nb <- MASS::glm.nb(y ~ x, data = df)
# 
# str(m_nb)
# 
# class(m_nb)
# 
# # Zero-Inflated Poisson
# m_zip <- pscl::zeroinfl(y_zi ~ x | 1, data = df, dist = "poisson")
# 
# m_zip
# class(m_zip)
# 
# m_zip$dist 
# 
# # Zero-Inflated NegBin
# m_zinb <- pscl::zeroinfl(y_zi ~ x | 1, data = df, dist = "negbin")
# ?model.frame
# str(m_zip)
# 
# model.frame(m_zip)