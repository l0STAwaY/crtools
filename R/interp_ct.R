interp_ct <- function(model,alpha=0.05){
  # model family
  fam <- get_ct_family(model)
  data <-model.frame(model)
  mod.sum <- summary(model)
  
  
  # likely we would need factorize data that has two level
  
  
  # update refits the model with the formula with nykk
  null_model <- update(model, . ~ 1)
  lr <-  lmtest::lrtest(null_model, model)
  
  # degree of freedom
  lr_stat <- lr$Chisq[2]
  lr_df   <- lr$Df[2]
  lr_p    <- lr$`Pr(>Chisq)`[2]

  # McFadden R2
  ll_null <- lr$LogLik[1] 
  ll_full <- lr$LogLik[2]
  r2 <- 1 - (ll_full / ll_null)
  
  mc_class <- if (r2 < 0.1) {
    "weak"
  } else if (r2 < 0.2) {
    "moderate"
  } else if (r2 < 0.4) {
    "strong"
  } else {
    "very strong"
  }
  
  
  
  # Dispersion ratio
  pearson_ratio <- sum(residuals(model, type = "pearson")^2) / model$df.residual
  
  disp_msg <- paste0(
    "\U2022 Pearson dispersion ratio = ", round(pearson_ratio, 4),
    ifelse(pearson_ratio > 1.5,
           ", Disepersion Ratio greater than 1.5 suggesting overdispersion. Use chk_count for furthur model condition details.",
           ", Disepersion Ratio less than 1.5 indicating no strong overdispersion. Use chk_count for furthur model condition details.")
  )
  
  
  # Check offset varaible
  offset_var <- grep("^offset\\((.*)\\)$", names(data), value = TRUE)
  offset_var <-  sub("^offset\\((.*)\\)$", "\\1", offset_var)
  has_offset<-length(offset_var)>0
  
  # response variable
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
  } else {
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
  coefs <- coefs[names(coefs) != "(Intercept)"]

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
  inter_plot_list <- list()
  
  # All the test produced by the following process
  inter_text_list <- list()


  # use term to get term
  interactions <- attr(terms(model), "term.labels")

  interactions <- interactions[grepl(":", interactions)]
  

  
  if (length(interactions) > 0) {
    
    message("This model has interactions so these interpretations are based on marginal effects, which are partial 
          derivatives of the regression equation with respect to each variable. These marginal effects are calculated as 
          the average change across observations. Calculating marginal effects at representative prespecified values is 
          supported in R.This application only supports two way interactions ploting.")

    for (i in interactions) {
      
   
 
      
      # variable for two way interaction
      vars <- strsplit(i, ":")[[1]]

      
      
      # this jus labels in order of the vector what are continous and what is a factor
      types <- sapply(vars, function(v) {
        if (is.numeric(model.frame(model)[[v]])) "cont" else "factor"
      })
      
      # ALWAYS ADD PLOT (one direction is enough)
      inter_plot_list[[i]] <- ggemmeansplot(
        model = model,
        pred = vars[1],
        moderator = vars[2]
      )
      
      
      
      # --------------------------------------------------------
      # CONTINUOUS × CONTINUOUS use Johnson–Neyman (BOTH DIRECTIONS)
      # --------------------------------------------------------
      if (all(types == "cont") && length(vars) == 2) {
        
        # A | B
          res_ab <- jnplot(
            model = model,
            pred = vars[1],
            moderator = vars[2],
            alpha = alpha
          )
          
          if (!is.null(res_ab)) {
            inter_plot_list[[paste0(i, "_", vars[1], "_on_", vars[2])]] <- res_ab
          }
          
          # B | A
          res_ba <- jnplot(
            model = model,
            pred = vars[2],
            moderator = vars[1],
            alpha = alpha
          )
          
          if (!is.null(res_ba)) {
            inter_plot_list[[paste0(i, "_", vars[2], "_on_", vars[1])]] <- res_ba
          }
        
          interp_text <- paste0(
            interp_text,
            "\n\U2022  Johnson–Neyman analysis was conducted in both directions (",
            vars[1], " <->", vars[2],
            "), showing how each variable’s effect on the outcome varies across the other.",
            "\n\nNOTE: The Johnson–Neyman plot is evaluated on the log(count) scale, not the response scale."
          )
      }
      
      
      # --------------------------------------------------------
      # CONTINUOUS × FACTOR -> emtrends
      # --------------------------------------------------------
      else if (any(types == "cont") && any(types == "factor") && length(vars) == 2) {
        
        cont_var <- vars[types == "cont"]
        fac_var  <- vars[types == "factor"]
        
        em <- emmeans::emtrends(
          model,
          specs = as.formula(paste("~", fac_var)),
          var = cont_var
        )
        
        # this turns the data into response scale
        em_df <- as.data.frame(em)
        
        # safe exponentiation (only meaningful for log-link models)
        em_df$rr <- exp(em_df$trend)
        em_df$rr_LCL <- exp(em_df$asymp.LCL)
        em_df$rr_UCL <- exp(em_df$asymp.UCL)
        
       
      }
      
      # --------------------------------------------------------
      # CATEGORICAL X CATEGORICAL emmeans
      # --------------------------------------------------------
      
      if (all(types == "factor") && length(vars) == 2) {
       
        em <- emmeans::emmeans(
          model,
          specs = as.formula(paste("~", vars[1], "|", vars[2])),
          type = "response"
        )
        
        inter_text_list[[i]] <- build_emmeans_text(
          model = model,
          mod.emmeans = as.data.frame(em),
          pred = vars[1],
          moderator = vars[2],
          transform_type = "none"
        )
        
        
      }
    }
  }


  
  #--------------------------Final Output-----------------------#
  
  if (length(inter_text_list) > 0) {
    interaction_text <- paste(unlist(inter_text_list), collapse = "\n\n")
  } else {
    interaction_text <- ""
  }
  
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
    "\n\n", "----------------------Interactions Interpretation-----------------",
    interaction_text 
  )
  
  cat(text)
  

  
}




response_var <- all.vars(formula(model))[1]
names(model$model)[1]


data <- read.csv("../Private_Dataset/McMillanAcheMonkeyTrips.csv")
data$x2 <- rnorm(nrow(data) ,mean=0,sd=1)



model <- fit_ct(
  Kills ~  log(Age)*x2 ,
  data = data,
  family = "poisson"
)

interp_ct(model)

coefs_raw <- coef(model)

interactions <- names(coefs_raw)[grepl(":", coefs_raw)]

beta <- coefs[name]
model.frame(model)[[moderator]]

  interactions <- coefs_raw[grepl(":", coefs_raw)]

coef(model)


