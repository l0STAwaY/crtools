#' Select and Compare Count Regression Models
#'
#' Fits and compares multiple count regression models (Poisson, quasi-Poisson,
#' negative binomial, zero-inflated Poisson, and zero-inflated negative binomial),
#' computes diagnostics, bootstrap confidence intervals, and recommends the best model
#' based on BIC. 
#'
#' @param formula A model formula for the count outcome (e.g., \code{y ~ x1 + x2 + offset(log(t))}).
#'
#' @param data A data frame containing the variables used in the formula.
#'
#' @param B Integer. Number of bootstrap replications used in diagnostic functions.
#' Default is 100.
#'
#' @return A list containing:
#' \describe{
#'   \item{models}{List of fitted models (Poisson, qPoisson, NB, ZIP, ZINB).}
#'   \item{diagnostics}{List of diagnostics from \code{diag_ct()} for each model.}
#'   \item{performance}{Model performance metrics (AIC, BIC, R2, etc.).}
#'   \item{recommendation}{Best model selected based on lowest BIC.}
#'   \item{bootstrap_ci}{Combined bootstrap confidence intervals across models.}
#'   \item{model_ci}{Analytical confidence intervals from \code{confint()}.}
#'   \item{bootstrap_plot}{ggplot object visualizing bootstrap confidence intervals.}
#' }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Fits multiple count models using \code{fit_ct()}.
#'   \item Runs diagnostic checks using \code{diag_ct()} including:
#'     model performance, bootstrap inference, and model-based confidence intervals.
#'   \item Selects the best model using BIC.
#'   \item Combines bootstrap confidence interval results across models for comparison.
#'   \item Produces a coefficient plot for the count component of all models.
#' }
#' 
#' 
#' In the case where the formula is specified with mixed effect syntax only the the mixed effectmodels glmnb and glmpoisson will be fitted to compare and select
#'
#'
#' @section Notes:
#' \itemize{
#'   \item ZIP and ZINB models may fail to converge; these are safely handled via \code{safe_fit()}.
#'   \item Model-based confidence intervals depend on \code{confint()} support for each model class. Specifically, Quasai poisson confindecen interval is not computable.
#' }
#'
#' @import dplyr ggplot2 tibble
#'
#' @export
select_ct <- function(formula, data, B = 50) {
  
  
  # safe_fit model here gives the error and fit the other models
  # this is here because some times solution dont converge
  # we sometimes face issue where by default the. zip for example can't converge and need a unique specified start value instead
  safe_fit <- function(fit_expr, model_name) {
    tryCatch(
      fit_expr,
      error = function(e) {
        warning(paste0(model_name, " failed: ", e$message))
        NULL
      }
    )
  }
  

  # detect random effect terms
  # deparse concerse formula ino string and we ase some varible ( | )
  is_mixed <- grepl("\\([^()]*\\|[^()]*\\)", deparse1(formula))
  

  if (is_mixed) {
    
    message("Mixed-effects structure detected -> fitting GLMM count models only")
    
    models <- list(
      glmpoisson = safe_fit(fit_ct(formula, data, family = "glmpoisson"), "glmpoisson"),
      glmnb      = safe_fit(fit_ct(formula, data, family = "glmnb"), "glmnb")
    )
    
  } else {
    
    message("No mixed-effects structure -> fitting full count model set")
    
    models <- list(
      poisson    = safe_fit(fit_ct(formula, data, family = "poisson"), "poisson"),
      qpoisson   = safe_fit(fit_ct(formula, data, family = "qpoisson"), "qpoisson"),
      negbin     = safe_fit(fit_ct(formula, data, family = "negbin"), "negbin"),
      zip        = safe_fit(fit_ct(formula, data, family = "zip"), "zip"),
      zinb       = safe_fit(fit_ct(formula, data, family = "zinb"), "zinb"),
      glmpoisson = safe_fit(fit_ct(formula, data, family = "glmpoisson"), "glmpoisson"),
      glmnb      = safe_fit(fit_ct(formula, data, family = "glmnb"), "glmnb")
    )
  }

  

  
  # all the diagnostic
  # this is a list of list
  diags <- list(
    poisson = if (!is.null(models$poisson)) {
      diag_ct(models$poisson, B = B)
    } else {
      NULL
    },
    
    qpoisson = if (!is.null(models$qpoisson)) {
      diag_ct(models$qpoisson, B = B)
    } else {
      NULL
    },
    
    negbin = if (!is.null(models$negbin)) {
      diag_ct(models$negbin, B = B)
    } else {
      NULL
    },
    
    zip = if (!is.null(models$zip)) {
      diag_ct(models$zip, B = B)
    } else {
      NULL
    },
    
    zinb = if (!is.null(models$zinb)) {
      diag_ct(models$zinb, B = B)
    } else {
      NULL
    },
    
    glmpoisson = if (!is.null(models$glmpoisson)) {
    diag_ct(models$glmpoisson, B = B)
    } else {
      NULL
    },
    
    glmnb = if (!is.null(models$glmnb)) {
      diag_ct(models$glmnb, B = B)
    } else {
       NULL
    }
  )
  
  
  # sapply returns a vector
  # we get all columns that 
  # goes through each element(which is list) of diags if it is no return That element in the returned vector is false
  valid_names <- names(diags)[!sapply(diags, is.null)]

  perf_df <- dplyr::bind_rows(
    lapply(valid_names, function(name) diags[[name]]$performance)
  )

  perf_df$model <- valid_names
  
  
  # poisson alway NA
  perf_df <- perf_df %>%
    dplyr::filter(!(model == "qpoisson"))
  
  


   # best BIC
   best_model <- perf_df %>%
    dplyr::slice_min(BIC, n = 1, with_ties = FALSE) %>%
    dplyr::pull(model)
  
  recommendation <- list(
    best_model = best_model,
    reason = "Lowest BIC among fitted models"
  )
  

  # boot ci
  boot_ci <- dplyr::bind_rows(
    lapply(valid_names, function(name) {
      ci <- diags[[name]]$bootstrap_ci
      ci$model <- name
      return(ci)
    })
  )

  
  
  model_ci <- dplyr::bind_rows(
    lapply(valid_names, function(name) {
      ci <- as_tibble(diags[[name]]$model_ci)
      ci$model <- name
      return(ci)
    })
  )
  
  
  # add     lr_tests = lr_tests, if necessary
  

  boot_ci_ct <- boot_ci[!grepl("^zero_", boot_ci$term), ]
  ct_term_name <- sub("^count_(.*)$", "\\1", boot_ci_ct$term)
    boot_ci_ct <- boot_ci_ct %>%
    dplyr::mutate(
      term = ct_term_name,
      .keep = "all"
    )

  
  boot_plot <- ggplot2::ggplot(
    boot_ci_ct,
    ggplot2::aes(
      x = term,
      y = (`2.5%` + `97.5%`) / 2,
      color = model
    )
  ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.4)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = `2.5%`,
        ymax = `97.5%`
      ),
      position = ggplot2::position_dodge(width = 0.4),
      width = 0.2
    ) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "Bootstrap Confidence Intervals Across Count Models(Count Model Component)",
      x = "Coefficient",
      y = "Coeffcient Estimate"
    )
  
  
  cat("\n================ MODEL DIAGNOSTICS ================\n")
  
  cat("\nFitted models:\n")
  print(names(models)[!sapply(models, is.null)])
  
  cat("\nPerformance summary (top rows):\n")
  print(utils::head(perf_df, 10))
  
  cat("\nBest model recommended by BIC:\n")
  print(best_model)
  
  cat("\nBootstrap CI summary:\n")
  print(utils::head(boot_ci, 10))
  
  
  cat("\nModel-based CI preview (confint):\n")
  print(utils::head(model_ci, 10))
  
  cat("\n====================================================\n\n")
  
  
  # print the bootsrap plot since it is always valid
  print(boot_plot)
  
  # return
  return(list(
    models = models,
    diagnostics = diags,
    performance = perf_df,
    recommendation = recommendation,
    bootstrap_ci = boot_ci,
    model_ci = model_ci,
    bootstrap_plot = boot_plot
  ))
}

