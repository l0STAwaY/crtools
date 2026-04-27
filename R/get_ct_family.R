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