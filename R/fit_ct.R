#' Fit count regression models
#'
#' Fits a variety of count regression models using a unified interface,
#' including Poisson, quasi-Poisson, negative binomial, zero-inflated models,
#' and mixed-effects versions of poisson and negative binomial.
#'
#' @param formula Model formula (e.g., y ~ x1 + x2 or y ~ x1 + (1 | group))
#' @param data Data frame containing variables in the formula
#' @param family Character string specifying model type:
#'   "poisson", "qpois", "nb", "zip", "zinb",
#'   "glmpoisson", or "glmnb"
#'
#' @return A fitted model object with an added field
#'   \code{$ct_family} indicating the chosen model type
#'
#' @examples
#' fit_ct(y ~ x1 + x2, data = df, family = "poisson")
#' fit_ct(y ~ x1 + (1 | group), data = df, family = "glmpoisson")
#'
#' @import MASS pscl glmmTMB
#' @export
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

    
  } else if (family == "glmpoisson") {
    model <- glmmTMB::glmmTMB(
      formula = formula,
      data = data,
      family = poisson()
    )
    
  } else if (family == "glmnb") {
    model <- glmmTMB::glmmTMB(
      formula = formula,
      data = data,
      family = nbinom2())

    
  } else {
    stop("Invalid model type")
  }
  
  model$ct_family = family
  

  
  return(model)
}


model <- fit_ct(y ~ mpg + cyl + hp + (1 | gear),
       data = data,
       family = "glmpoisson")

str(model)
model$ct_family



model1 <- fit_ct(y ~ mpg + cyl,
                data = data,
                family = "poisson")


model <- fit_ct(y ~ mpg + cyl + hp + (1| gear),
                data = data,
                family = "glmnb")

model <- fit_ct(y ~ mpg + cyl + hp + (1+exposure| gear),
                data = data,
                family = "glmnb")

model3 <- lmer(y ~ mpg + cyl + hp + (1 | gear),
               data = data)


model <- fit_ct(y ~ mpg * vs + (1 | gear),
                data = data,
                family = "glmpoisson")

data2 <- read.csv("/Users/apple/Documents/GitHub/crtools/Private_Dataset/McMillanAcheMonkeyTrips.csv")

model <- fit_ct(Age ~ Kills + (1 | PID),
                data = data2,
                family = "glmpoisson")


get_ct_family(model)


model2 <- glmmTMB(
  y ~ mpg + cyl + hp + ( | gear),
  data = data,
  family = poisson()
)

model2 <- glmmTMB(
  y ~ mpg + cyl + hp + (1 | gear),
  data = data,
  family = poisson()
)

get_ct_family(model)
x <-  grepl("Negative Binomial", family(model)$family)
ig. Negative Binomial %in% family(model)["family"]
names(family(model))
attr(model,"family")
