bootstrap_ct_model <- function(model, B = 100) {
  # b is the number of simulation for bootstra
  # This has to be a fit_ct fitted model
  # It will break if we call a standard glm and pass it in
  data <- model.frame(model)
  formula <- formula(model)
  family <- get_ct_family(model)
  coefs_all <- tibble::tibble()
  
  
  for (b in 1:B) {
    
    boot_dt <- dplyr::slice_sample(data, n = nrow(data), replace = TRUE)
    
    
    fit_boot <- fit_ct(formula,data,family=family)
    

    coef_list <- as.list(coef(fit_boot))
    
    coefs_all <- dplyr::bind_rows(
      coefs_all,
      tibble::as_tibble(as.list(c(b = b, coef(fit_boot))))
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
    )
  
  return(list(
    raw = coefs_all,
    ci = ci
  ))
}





# 
# res <- bootstrap_ct_model(m_zip, B = 200)
# head(res$raw)
# res$ci
