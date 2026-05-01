#' Model Interpretation for Count Regression Models
#'
#' Provides a unified interpretation framework for count regression models
#' fitted using \code{fit_ct}. The function generates model interpretation text,
#' interaction summaries, marginal effects tables, and diagnostic plots.
#'
#' @param model A fitted count regression model from \code{fit_ct}.
#' @param alpha Significance level used for interaction tests and confidence intervals (default = 0.05).
#' @param display Logical. If TRUE, prints tables and plots for model interactions.
#'
#' @details
#' This function summarizes:
#' \itemize{
#'   \item Main model interpretation (log-link coefficient interpretation)
#'   \item Emmeans-based marginal means for interactions
#'   \item Emtrends-based marginal slopes for interactions
#'   \item Pairwise contrasts for both emmeans and emtrends
#'   \item Diagnostic interaction plots including:
#'     \itemize{
#'       \item Emmeans plots
#'       \item Emtrends plots
#'       \item Johnson–Neyman plots (when available)
#'     }
#' }
#' 
#' 
#' 
#'
#' Interaction types supported:
#' \itemize{
#'   \item Continuous × Continuous
#'   \item Continuous × Factor
#'   \item Factor × Factor (limited to emmeans only)
#' }
#'
#' Johnson–Neyman plots are only produced for models and interactions
#' where supported (only glm).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{text}: Full model interpretation summary
#'   \item \code{emmeans_tables}: Marginal means tables
#'   \item \code{contrast_tables}: Pairwise contrasts of marginal means
#'   \item \code{emtrends_tables}: Marginal slope estimates
#'   \item \code{emtrends_contrast_tables}: Pairwise slope contrasts
#'   \item \code{plots}: List of interaction plots including:
#'     \itemize{
#'       \item emmeans plots
#'       \item emtrends plots
#'       \item Johnson–Neyman plots (if available)
#'     }
#' }
#'
#' @import emmeans ggplot2 dplyr lmtest
#' @export
#'
#' @examples
#' interp_ct(model)
#' interp_ct(model, alpha = 0.01, display = FALSE)
interp_ct <- function(model,alpha=0.05,display=TRUE){
  # model family
  fam <- get_ct_family(model)
  data <- if (!is.null(model$org_data)) model$org_data else model.frame(model)
  mod.sum <- summary(model)
  response <- all.vars(formula(model))[1]
  
  
  # likely we would need factorize data that has two level
  
  
  # update refits the model with the formula with nykk
  null_model <- fit_ct(
    as.formula(paste(response, "~ 1")),
    data = data,
    family = fam
  )

  lr <-  lmtest::lrtest(null_model, model)
  
  
  # degree of freedom
  lr_stat <- lr$Chisq[2]
  lr_df   <- lr$Df[2]
  lr_p    <- lr$`Pr(>Chisq)`[2]

  # McFadden R2
  ll_null <- lr$LogLik[1] 
  ll_full <- lr$LogLik[2]
  

  
  if (fam == "qpoisson") {
    message("qpoisson does not have a true liklihood hence McFadden's R2 is not returned")
    r2 <- NA
    mc_class <- "not defined for quasi-poisson"
  } else {
    r2 <- 1 - (ll_full / ll_null)
    
    mc_class <- if (is.na(r2)) {
      "undefined"
    } else if (r2 < 0.1) {
      "weak"
    } else if (r2 < 0.2) {
      "moderate"
    } else if (r2 < 0.4) {
      "strong"
    } else {
      "very strong"
    }
  }
  
  

  
  
  # Dispersion ratio
  pearson.ratio <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
  
  disp_msg <- paste0(
    "\U2022 Pearson dispersion ratio = ", round(pearson.ratio, 4),
    ifelse(pearson.ratio > 1.5,
           ", Disepersion Ratio greater than 1.5 suggesting overdispersion. Use chk_count for furthur model condition details.",
           ", Disepersion Ratio less than 1.5 indicating no strong overdispersion. Use chk_count for furthur model condition details.")
  )
  
  
  # Check for offset varaible
  offset_var <- grep("^offset\\((.*)\\)$", names(data), value = TRUE)
  offset_var <-  sub("^offset\\((.*)\\)$", "\\1", offset_var)
  has_offset<-length(offset_var)>0
  
  # Response variable
  # Note this is different behaviour from names(model$model)[1]
  # names(model$model)[1] will return with log() if transformed where as all.var wont
  
   response_var <- all.vars(formula(model))[1]
  
   # ---------------- offset ----------------
  # what the outcome is to be displayed in interpreation
   # is there an offset or not
  outcome_word <- if (has_offset) {
    paste0("expected rate (", response_var, " per ", offset_var, ")")
  } else {
    paste0("expected count (", response_var, ")")
  }
  
  
  
  # ---------------- link ----------------
  # What is the link function
  link <- if (fam %in% c("poisson", "qpoisson", "negbin")) {
    model$family$link
  } else if (fam %in% c("zip", "zinb")) {
    "log"
  } else if (fam %in% c("glmnb","glmpoisson")){
    family(model)$link
  } else{
    NA
  }
  
  
  
  # ---------------- coefficients ----------------
  # Get the coefficients for interpretation
  # coefs_raw also contain the coef value and is a named vector
  coefs_raw <- coef(model)
  
  
  
  coefs <- if (fam %in% c("zip", "zinb")) {
    # keep ONLY count component
    coefs_raw[grepl("^count_", names(coefs_raw))]
  } else {
    coefs_raw
  }
  
  
  # filter out the intercept and what remains is only the count coeffcients
  if(fam %in% c("glmnb","glmpoisson")){
    coefs <- fixef(model)$cond[names( fixef(model)$cond)!= "(Intercept)"]
  }else if(fam %in% c("zip", "zinb")){
    coefs <- coefs[names(coefs) != "count_(Intercept)"]
  }else{
    coefs <- coefs[names(coefs) != "(Intercept)"]
  }

  # ---------------- main interpretation ----------------
  # implemented log - unit
  # log - log
  # Specify changes only when in equation
  # the interaction here make sense only for loglink
  
  interp <- if (length(coefs) > 0 && link=="log") {
    
    sapply(names(coefs), function(name){
      
      # names for bet
      beta <- coefs[name]
      # log-transformed predictor
      # if this variable is log that is we are able to grep a log out of it
      if (length(grep("log\\(", name)) > 0) {
        
        # log interperation is percentage percentage increase intrepreation
        #extract the name out of the log
        var_name <- sub("^log\\((.*)\\)$", "\\1", name)
        change <- 100 * (exp(beta * log(1.01)) - 1)
        paste0(
          "\U2022 ", var_name,
          " is log-transformed: a 1% increase in ", var_name,
          " is associated with approximately ",
          round(change, 2),
          "% change in the ", outcome_word, "."
        )
        
      } else {
        exp_beta <- exp(beta)
        change <- 100 * (exp_beta - 1)
        paste0(
          "\U2022 A one-unit increase in ", name,
          " changes the ", outcome_word,
          " by ", round(change, 2),
          "%.")
      }
    })
    
  } else ""
  
  interp_text <- paste(interp, collapse = "\n")
  
  

  #-----------------------------Interaction Term Handeling-----------------------------
  
  # This is a list structure becaus we can have multiple interactions
  # All the plot produced by the following process
  
  #
  inter_emmeans_plot_list <- list() # this the emmeans plot
  inter_emmeans_table_list   <- list()
  inter_emmeans_plot_list    <- list()
  inter_emmeans_text_list <- list()
  
  # no plot contrast
  inter_contrast_table_list  <- list()
  inter_contrast_text_list   <- list()
  
  
  inter_emtrends_contrast_table_list <- list()
  inter_emtrends_contrast_text_list  <- list()
  
  # emtrends
  inter_emtrends_plot_list   <- list()
  inter_emtrends_table_list  <- list()
  inter_emtrends_text_list    <- list()
  
  # jnplot
  inter_jn_plot_list <- list()
  
  
  
  
  
  # use term to get term
  interactions <- attr(terms(model), "term.labels")
  interactions <- interactions[grepl(":", interactions)]
  

  
  if (length(interactions) > 0) {
    
    message("This model has interactions so these interpretations are based on marginal effects, which are partial derivatives of the regression equation with respect to each variable. These marginal effects are calculated as the average change across observations. Calculating marginal effects at representative prespecified values is supported in R.This application only supports two way interactions ploting.")

    for (i in interactions) {
      
      
  
        
      vars <- strsplit(i, ":")[[1]]
      

      
      vars <- sub("^log\\((.*)\\)$", "\\1", vars)
      



    
      # this jus labels in order of the vector what are continous and what is a factor
      types <- sapply(vars, function(v) {
        if (is.numeric(model.frame(model)[[v]])) "cont" else "factor"
      })
      
      # --------------------------------------------------------
      # CONTINUOUS × CONTINUOUS use Johnson–Neyman (BOTH DIRECTIONS)
      # --------------------------------------------------------
      if (all(types == "cont") && length(vars) == 2) {
        
        
        # -------------------------------------------------------
        # 1. EMMEANS = RESPONSE SURFACE (cont | cont )
        # -------------------------------------------------------
        
        # plot
        inter_emmeans_plot_list[[paste0("emmeans_", i)]] <- ggemmeansplot(
          model = model,
          pred = vars[1],
          moderator = vars[2]
        )
        
        # emmeans table
        em <- emmeans::emmeans(
          model,
          specs = as.formula(paste("~", vars[1], "|", vars[2])),
          type = "response",
          data = data
        )
        
        em_tab <- as.data.frame(em)
        names(em_tab) <- gsub("\\.scaled", "", names(em_tab))
        
        # store with clear label
        inter_emmeans_table_list[[paste0("emmeans_", i)]] <- em_tab
        
        # text
        inter_emmeans_text_list[[paste0("emmeans_", i)]] <- build_emmeans_text(
          model = model,
          mod.emmeans = em_tab,
          pred = vars[1],
          moderator = vars[2]
        )
        
        #------------------ contrast table-------------
        em_contrast_tab <- build_emmeanscontrasts_tab(
          model = model,
          pred = vars[1],
          moderator = vars[2]
        )
        
        inter_contrast_table_list[[paste0("emmeans_", i)]] <- em_contrast_tab
        
        # contrast text
        inter_contrast_text_list[[paste0("emmeans_", i)]] <- build_emmeanscontrasts_text(
          model = model,
          mod.emmeanscontrast = em_contrast_tab,
          pred = vars[1],
          moderator = vars[2],
          alpha = alpha
        )
        
        
        # -------------------------------------------------------
        # 2. EMTRENDS = MARGINAL SLOPE (cat | cont)
        # -------------------------------------------------------
        
        
        key_trend <- paste0("emtrends_", i)
        
        
        # trend table text and plot
        em_trend_tab <- build_interaction_emtrends_tab(
          model = model,
          pred = vars[1],
          moderator = vars[2]
        )
        inter_emtrends_table_list[[key_trend]] <- em_trend_tab
        
        inter_emtrends_text_list[[key_trend]] <-  build_interaction_emtrends_text(
          model = model,
          mod.emtrends = em_trend_tab,
          pred = vars[1],
          moderator = vars[2],
          alpha = alpha
        )
        
        
        emtrendcontrast_tab <- build_emtrendcontrast_tab(model = model,
                                                         pred = vars[1],
                                                         moderator = vars[2])
        
        inter_emtrends_contrast_table_list[[key_trend]]  <-  emtrendcontrast_tab
        inter_emtrends_contrast_text_list[[key_trend]]   <- build_emtrendcontrast_text( model = model,
          mod.emtrendcontrast =  emtrendcontrast_tab,                                                                          
          pred = vars[1],
          moderator = vars[2],
        alpha = alpha)
        
        
        
        
        # A | B
        res_ab <- jnplot(
          model = model,
          pred = vars[1],
          moderator = vars[2]
        )
        
        if (!is.null(res_ab)) {
          inter_jn_plot_list[[paste0(i, "_", vars[1], "_on_", vars[2])]] <- res_ab
        }
        
        # B | A
        res_ba <- jnplot(
          model = model,
          pred = vars[2],
          moderator = vars[1]
        )
        
        if (!is.null(res_ba)) {
          inter_jn_plot_list[[paste0(i, "_", vars[2], "_on_", vars[1])]] <- res_ba
        }
          
          

   
        
      }
      
      
      # --------------------------------------------------------
      # CONTINUOUS × FACTOR -> emtrends
      # --------------------------------------------------------
      else if (any(types == "cont") && any(types == "factor") && length(vars) == 2) {
        
        
        
       
        cont_var <- vars[types == "cont"]
        fac_var  <- vars[types == "factor"]
        
        # -------------------------------------------------------
        # 1. EMMEANS = RESPONSE SURFACE (cont | cat )
        # -------------------------------------------------------
        
        em <- emmeans::emmeans(
          model,
          specs = as.formula(paste("~", cont_var, "|", fac_var)),
          type = "response",
          data = data
        )
        
        em_tab <- as.data.frame(em)
        names(em_tab) <- gsub("\\.scaled", "", names(em_tab))
        
        key_emmeans <- paste0("emmeans_", cont_var, "_by_", fac_var)
        
        inter_emmeans_table_list[[key_emmeans]] <- em_tab
        
        inter_emmeans_text_list[[key_emmeans]] <- build_emmeans_text(
          model = model,
          mod.emmeans = em_tab,
          pred = cont_var,
          moderator = fac_var
        )
        
        
        # contrast table and text
        em_contrast_tab <- build_emmeanscontrasts_tab(
          model = model,
          pred = cont_var,
          moderator = fac_var
        )
        
        inter_contrast_table_list[[key_emmeans]] <- em_contrast_tab
        
        inter_contrast_text_list[[key_emmeans]] <- build_emmeanscontrasts_text(
          model = model,
          mod.emmeanscontrast = em_contrast_tab,
          pred = cont_var,
          moderator = fac_var,
          alpha = alpha
        )
        
        
        # -------------------------------------------------------
        # 2. EMTRENDS = MARGINAL SLOPE (cat | cont)
        # -------------------------------------------------------
   
        

        
      key_trend <- paste0("emtrends_", cont_var, "_by_", fac_var)
      
        
      # trend table text and plot
      em_trend_tab <- build_interaction_emtrends_tab(
        model = model,
        pred = cont_var,
        moderator = fac_var
      )
      inter_emtrends_table_list[[key_trend]] <- em_trend_tab
      
      inter_emtrends_text_list[[paste0(cont_var, "_by_", fac_var)]] <-  build_interaction_emtrends_text(
        model = model,
        mod.emtrends = em_trend_tab,
        pred = cont_var,
        moderator = fac_var,
        alpha = alpha
      )
      
      # the plot for emmeans is the plot for trend as well
      inter_emtrends_plot_list[[paste0(cont_var, "_by_", fac_var)]] <- ggemmeansplot(
        model = model,
        pred = cont_var,
        moderator = fac_var
      )
      
      
      
      emtrendcontrast_tab <- build_emtrendcontrast_tab(model = model,
                                                       pred = vars[1],
                                                       moderator = vars[2])
      
      inter_emtrends_contrast_table_list[[key_trend]]  <-  emtrendcontrast_tab
      inter_emtrends_contrast_text_list[[key_trend]]   <- build_emtrendcontrast_text( model = model,
                                                                                      mod.emtrendcontrast =  emtrendcontrast_tab,                                                                          
                                                                                      pred = vars[1],
                                                                                      moderator = vars[2], alpha = alpha)



      

      
      }
      
      # --------------------------------------------------------
      # CATEGORICAL X CATEGORICAL emmeans
      # --------------------------------------------------------
      
      if (all(types == "factor") && length(vars) == 2) {
        
        
        
        # -------------------------------------------------------
        # 1. EMMEANS = RESPONSE SURFACE (cat | cat )
        # -------------------------------------------------------
      
        # plot
        inter_emmeans_plot_list[[paste0("emmeans_", i)]] <- ggemmeansplot(
          model = model,
          pred = vars[1],
          moderator = vars[2]
        )
        
        # emmeans table
        em <- emmeans::emmeans(
          model,
          specs = as.formula(paste("~", vars[1], "|", vars[2])),
          type = "response",
          data = data
        )
        
        em_tab <- as.data.frame(em)
        names(em_tab) <- gsub("\\.scaled", "", names(em_tab))
        
        # store with clear label
        inter_emmeans_table_list[[paste0("emmeans_", i)]] <- em_tab
        
        # text
        inter_emmeans_text_list[[paste0("emmeans_", i)]] <- build_emmeans_text(
          model = model,
          mod.emmeans = em_tab,
          pred = vars[1],
          moderator = vars[2]
        )
        
        #------------------ contrast table-------------
        em_contrast_tab <- build_emmeanscontrasts_tab(
          model = model,
          pred = vars[1],
          moderator = vars[2]
        )
        
        inter_contrast_table_list[[paste0("emmeans_", i)]] <- em_contrast_tab
        
        # contrast text
        inter_contrast_text_list[[paste0("emmeans_", i)]] <- build_emmeanscontrasts_text(
          model = model,
          mod.emmeanscontrast = em_contrast_tab,
          pred = vars[1],
          moderator = vars[2],
          alpha = alpha
        )
  
      }
    }
  }


  
  #--------------------------Final Output-----------------------#
  
  if (length(inter_emmeans_text_list) > 0) {
    inter_emmean_text <- paste(unlist(inter_emmeans_text_list), collapse = "\n\n")
  } else {
    inter_emmean_text <- ""
  }
  
  if (length(inter_emtrends_text_list) > 0) {
    inter_emtrends_text <- paste(unlist(inter_emmeans_text_list), collapse = "\n\n")
  } else {
    inter_emtrends_text <- ""
  }
  
  
  if (length(inter_contrast_text_list) > 0) {
    inter_contrast_text <- paste(unlist(inter_contrast_text_list), collapse = "\n\n")
  } else {
    inter_contrast_text <- ""
  }
  
  
  if (length(inter_emtrends_contrast_text_list) > 0) {
    inter_emtrends_contrast_text <- paste(unlist(inter_emtrends_contrast_text_list), collapse = "\n\n")
  } else {
    inter_emtrends_contrast_text <- ""
  }
  
  
  model_result <- summary(model)
  
 cat("----------------------Main Model Results-----------------\n")
 print(model_result)
 cat("\n")

  
  text <- paste0(
    "----------------------Main Model Interpretation-----------------",
    "\n\U2022 The ", fam, " model using a ", link, " link ",
    ifelse(lr_p < alpha,
           "significantly improves fit over the null model ",
           "does not significantly improve fit over the null model "),
    "(LR = ", round(lr_stat,4),
    ", df = ", lr_df,
    ", p-value ", ifelse(lr_p < 0.0001, "< 0.0001", paste0("= ", round(lr_p,4))), ").",
    
    "\n\U2022 McFadden's pseudo R-squared = ", round(r2,4),
    " (", mc_class, " model fit: ",
    "0–0.1 weak, 0.1–0.2 moderate, 0.2–0.4 strong, >0.4 very strong).",
    
    "\n",disp_msg,
    "\n",interp_text,
    "\n\n", "----------------------Emmeans Interactions Interpretation--------------\n",
     "\n",inter_emmean_text, 
    "\n\n----------------------Emmeans Contrast Interpretation-----------------\n",
    "\n", inter_contrast_text,
    "\n\n----------------------Emmeans Trends Interpretation-----------------\n",
    "\n",inter_emtrends_text,
    "\n\n----------------------Emmtrends Contrast Interpretation-----------------\n",
    "\n",inter_emtrends_contrast_text
    
  )
  
  cat(text)
  
  # IF display == True

  if(display==TRUE){
    cat("\n\n----------------------Emmeans Table-----------------\n")
    lapply(inter_emmeans_table_list, print)

    cat("\n\n----------------------Emmeans Contrast Table-----------------\n")
    lapply(inter_contrast_table_list, print)
    cat("\n\n----------------------Emtrends Table--------------------------\n")
    lapply(inter_emtrends_table_list,print)
    cat("\n\n----------------------Emtrends Contrast Table--------------------------\n")
    lapply(inter_emtrends_contrast_table_list, print)
                                      
                                      
    
    #-------------All available plots-----------#
    lapply(inter_emtrends_plot_list, print)
    lapply(inter_emmeans_plot_list, print)
    lapply(inter_jn_plot_list,print)
        

  }
  return(list(
    
    # ---------------- main interpretation ----------------
    text = text,
    
    # ---------------- emmeans ----------------
    emmeans_tables = inter_emmeans_table_list,
    
    # ---------------- emmeans contrasts ----------------
    contrast_tables = inter_contrast_table_list,
    
    # ---------------- emtrends ----------------
    emtrends_tables = inter_emtrends_table_list,
    emtrends_contrast_tables = inter_emtrends_contrast_table_list,
    
    # ---------------- plots ----------------
    plots = list(
      emmeans = inter_emmeans_plot_list,
      emtrends = inter_emtrends_plot_list,
      johnson_neyman = inter_jn_plot_list
    )
  ))
}

  







