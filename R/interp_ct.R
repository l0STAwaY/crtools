interpret_effect <- function(beta, term, model) {
  
  link <- model$family$link
  has_offset <- !is.null(model$offset) || "offset" %in% names(model$model)
  
  is_factor <- grepl(" = ", term)
  
  if (link == "log") {
    
    eff <- exp(beta)
    
    if (is_factor) {
      return(paste0(
        term,
        ": multiplicative effect = ",
        round(eff, 3),
        " on expected ",
        ifelse(has_offset, "rate", "count")
      ))
    } else {
      return(paste0(
        term,
        ": 1-unit increase multiplies ",
        ifelse(has_offset, "rate", "expected count"),
        " by ",
        round(eff, 3)
      ))
    }
    
  } else if (link == "identity") {
    
    return(paste0(
      term,
      ": additive effect = ",
      round(beta, 3),
      " on expected count"
    ))
    
  } else {
    
    return(paste0(
      term,
      ": effect = ",
      round(beta, 3),
      " (link: ", link, ")"
    ))
  }
}



interp_ct <- function(model, modelsummary, alpha = 0.05) {
  
  mod.sum <- summary(model)
  mod.table <- modelsummary
  
  # -----------------------------
  # Model-level summary
  # -----------------------------
  
  ll <- tryCatch(as.numeric(logLik(model)), error = function(e) NA)
  
  dev_ratio <- if (!is.null(mod.sum$deviance) && !is.null(mod.sum$df.residual)) {
    mod.sum$deviance / mod.sum$df.residual
  } else {
    NA
  }
  
  header <- c(
    paste0("Model: ", model$ct_family),
    paste0("Log-likelihood: ", round(ll, 4)),
    paste0("Residual deviance: ", round(mod.sum$deviance, 4)),
    paste0("Dispersion ratio: ", round(dev_ratio, 4))
  )
  
  # -----------------------------
  # Coefficients
  # -----------------------------
  
  body <- c()
  
  for (i in seq_len(nrow(mod.table))) {
    
    term <- mod.table$Term[i]
    if (term == "(Intercept)") next
    
    # all the steimates
    beta <- mod.table$Estimate[i]
    pval <- mod.table$`p-value`[i]
    
    sig <- ifelse(pval < alpha, "significant", "not significant")
    
    body <- c(
      body,
      paste0(
        interpret_effect(beta, term, model),
        " (", sig,
        ", p = ",
        ifelse(pval < 0.0001, "< 0.0001", round(pval, 4)),
        ")"
      )
    )
  }
  
  # -----------------------------
  # ZIP / ZINB note
  # -----------------------------
  
  note <- NULL
  
  if (model$ct_family %in% c("zip", "zinb")) {
    note <- paste0(
      "Zero-inflated model: includes (1) count process and (2) structural zero process."
    )
  }
  
  # -----------------------------
  # Output (PACKAGE FRIENDLY)
  # -----------------------------
  
  out <- list(
    header = header,
    effects = body,
    note = note
  )
  
  class(out) <- "count_model_interpretation"
  out
}