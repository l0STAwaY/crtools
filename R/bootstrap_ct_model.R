bootstrap_ct_model <- function(model, B = 100) {
  
  # This has to be a fit_ct fitted model
  # It will break if we call a standard glm and pass it in
  data <- model.frame(model)
  formula <- formula(model)
  family <- get_ct_family(model)
  
  coefs_all <- tibble()
  
  for (b in 1:B) {
    
    boot_dt <- dplyr::slice_sample(data, n = nrow(data), replace = TRUE)
    
    
    fit_boot <- if (inherits(model, "glm")) {
      glm(formula, data = boot_dt, family = family(model))
      
    } else if (inherits(model, "negbin")) {
      MASS::glm.nb(formula, data = boot_dt)
      
    } else if (inherits(model, "zeroinfl")) {
      pscl::zeroinfl(formula, data = boot_dt, dist = model$dist)
      
    } else {
      stop("Unsupported model type")
    }
    
    
    if (!is.null(fit_boot)) {
      coef_list <- as.list(coef(fit_boot))
      
      coefs_all <- dplyr::bind_rows(
        coefs_all,
        tibble::tibble(
          b = b,
          !!!coef_list
        )
      )
    }
  }
  
  # ---- CI ----
  ci <- coefs_all %>%
    summarise(
      across(
        where(is.numeric) & !dplyr::any_of("b"),
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
    )
  
  return(list(
    raw = coefs_all,
    ci = ci
  ))
}
