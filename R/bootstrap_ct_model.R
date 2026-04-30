#' Bootstrap Confidence Intervals for Count Models
#'
#' Performs non-parametric bootstrap inference for models fitted using \code{fit_ct}.
#' The function repeatedly resamples the original data with replacement, refits the model,
#' and computes empirical confidence intervals for all coefficients.
#'
#' @param model A fitted model object created by \code{fit_ct}. Must contain a valid
#' count model with a supported family (e.g., "poisson", "zip", "zinb").
#'
#' @param B Integer. Number of bootstrap replications. Default is 100.
#'
#' @return A list with two components:
#' \describe{
#'   \item{raw}{A data frame of bootstrap coefficient estimates for each replication.}
#'   \item{ci}{A data frame of bootstrap confidence intervals (2.5\% and 97.5\%).}
#' }
#'
#' @details
#' The bootstrap procedure:
#' \enumerate{
#'   \item Extracts the model frame from the fitted object.
#'   \item Repeatedly resamples rows with replacement.
#'   \item Refits the model using \code{fit_ct()}.
#'   \item Aggregates coefficient estimates across bootstrap samples.
#' }
#'
#' This function assumes that the model is correctly specified and that
#' \code{fit_ct()} supports the underlying model family.
#'
#' Bootstrap results may be unstable for zero-inflated models or small samples.
#'
#' @import dplyr tibble tidyr
#' @export
#'
bootstrap_ct_model <- function(model, B = 100) {
  # b is the number of simulation for bootstra
  # This has to be a fit_ct fitted model
  # It will break if we call a standard glm and pass it in
  data <- if (!is.null(model$org_data)) model$org_data else model.frame(model)
  # this 
  names(data) <- sub("^offset\\((.*)\\)$", "\\1", names(data))
  formula <- formula(model)
  fam <- get_ct_family(model)
  coefs_all <- tibble::tibble()
  
  
  for (b in 1:B) {
    
    boot_dt <- dplyr::slice_sample(data, n = nrow(data), replace = TRUE)
    
    
    fit_boot <- fit_ct(formula, boot_dt, family = fam)
    

    coef_list <- as.list(coef(fit_boot))
    
    if (fam %in% c("glmnb", "glmpoisson")) {
      coefs <- fixef(fit_boot)$cond
    } else {
      coefs <- coef(fit_boot)
    }
    
    coefs <- unlist(coefs)  # ensure numeric vector
    coefs_all <- dplyr::bind_rows(
      coefs_all,
      tibble::as_tibble(as.list(c(b = b, coefs)))
    )

  }
  
  # ---- CI ----
  ci <- coefs_all %>%
    summarise(
      # across allow you to operate on multiple colmn at once
      # it must numeric and not "b
      across(
        -b,
        list(
          lwr = ~quantile(.x, 0.025, na.rm = TRUE),
          upr = ~quantile(.x, 0.975, na.rm = TRUE)
        )
      )
    ) %>%
    tidyr::pivot_longer(
      everything(),
      names_to = c("term", ".value"),
      names_pattern = "^(.*)_(lwr|upr)$"
    )  %>% rename( `2.5%` = lwr,
                   `97.5%` = upr)
  
  return(list(
    raw = coefs_all,
    ci = ci
  ))
}
