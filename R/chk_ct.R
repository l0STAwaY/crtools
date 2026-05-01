library(car)
library(glue)
library(DHARMa)
library(patchwork)
#' Check Diagnostics for Count Regression Models
#'
#' Performs a comprehensive diagnostic check for count regression models,
#' including Poisson, quasi-Poisson, negative binomial, and zero-inflated models and mixed effect models
#'
#' This function evaluates:
#' \itemize{
#'   \item Event count adequacy per predictor
#'   \item Multicollinearity using VIF (Variance Inflation Factor)
#'   \item Zero-inflation (when applicable)
#'   \item Dispersion ratio diagnostics
#'   \item Residual diagnostics (Pearson, randomized quantile residuals)
#' }
#'
#' @details
#' Zero-inflation tests are performed using simulation-based methods (DHARMa).
#' However, these tests may fail for some model classes (e.g., \code{zeroinfl}
#' from the \code{pscl} package) due to missing \code{simulate()} methods or
#' incompatible model structures. In such cases, the test is automatically skipped
#'
#' Users are encouraged to use \code{glmmTMB} for zero-inflated models when
#' simulation-based diagnostics are required, as it provides full support for
#' DHARMa.
#'
#' @param model A fitted count regression model that fit_ct() support
#'
#' @return nothing, the model only prints the diagnostics for count regression Regression and suggest
#'
#' @note
#' VIF testing is simulation-based and may occasionally fail or
#' produce unstable results depending on the model structure
#'
#' Therefore, failure of posisson testing does not necessarily indicate
#' an issue with the data or model fit.
#'
#' @import car
#' @import glue
#' @import DHARMa
#' @import patchwork
#'
#' @examples
#' \dontrun{
#' model <- glm(count ~ x1 + x2, family = poisson, data = df)
#' chk_ct(model)
#' }
chk_ct <- function(model){
   
   # model family

  
   fam <- get_ct_family(model)
   

  
   if(fam %in% c("poisson","qpoisson","negbin","glmnb","glmpoisson")){
    
     # Condition 3 Enough count 
     
     cat("#--------------------Count Per Predictor Interpretation----------------------#\n")
     y_ct <- sum(model.response(model.frame(model)))
     Threshold <- 10 * length(coef(model))
     if(y_ct>=Threshold){
       cat("\nTotal event counts is at least 10-20 events predictor variable\n")
     }else{
       cat("\nTotal event counts is not at least 10-20 events predictor variable\n")
     }
     
     
     
     # Condition 2, little to no multi-colinearity
     cat("#---------------------------VIF Model and Interpretation---------------------#\n")
     if(length(coef(model))<=2){
       message("\nno vif reported: model contains fewer than 2 terms" )
     }
     else{
       performance::check_collinearity(model)
       cat("\n")
       cat(vif_report(model))
     }

     # Test Zero Inflation
     if (fam %in% c("qpoisson")){
       message("testZeroInflation does not support qpoisson family, no zero inflation test reported")
     } else if(fam %in% c("zip","zinb")){
         message("testZeroInflation does any Zero inflation model family, no zero inflation test reported")
     }
     else{ 
       message("Zero Inflation Simulation Plot is Plotted" )
       testZeroInflation(model)
       zi_test <- testZeroInflation(model)
       pval <- zi_test$p.value
      
       if (pval < 0.05) {
         cat("\nZero-inflation test is significant (p < ", 0.05,
             "). Evidence suggests excess zeros are present; consider ZIP or ZINB models.\n", sep = "")
         
       } else {
         cat("\nZero-inflation test is not significant (p >= ", 0.05,
             "). No strong evidence of excess zeros; standard count models may be adequate.\n", sep = "")
       }
     }
     
   }
     
   else if(fam %in% c("zip","zinb")){
     
    
     # Condition 1, little to no Multi-Colinearity
     
     cat("#------------------------------VIF Model and Interpretation-------------------------#\n")
     if(length(model$coefficients$count)<=2){
       message("no vif reported model contains fewer than 2 terms" )
    
     }else{
       
       vif_res <- tryCatch(
         {
           performance::check_collinearity(model)
         },
         error = function(e) {
           message("VIF computation failed for ZIP/ZINB model: ", e$message)
         },
         warning = function(w) {
           message("VIF warning for ZIP/ZINB model: ", w$message)
           invokeRestart("muffleWarning")
         }
       )
       
       if (!is.null(vif_res)) {
         print(vif_res)
         cat(vif_report(model))
       }
       
    
      }

     
     # Condition 2 Enough count for predictor
     y_ct <- sum(model.response(model.frame(model)))
     Threshold_count <- 10 * length(model$coefficients$count)
      
     cat("#----------------------------Count and Zero Per Predictor Interpretation----------------#\n")

     if(y_ct>=Threshold_count){
       
       cat(glue("Total event counts {y_ct} is at least 10 events predictor variable"))
     }
     else{
       cat(" Total event counts is not at least 10 events predictor variable")
     }
     
     
     # Condition 3 Enough 0
     zero_ct <- sum(model.response(model.frame(model))==0)
     non_zero_ct <- sum(model.response(model.frame(model))!=0)
     Threshold_zero <- 10 * length(model$coefficients$zero)

     if(zero_ct>=Threshold_zero){
       message(glue(" Total zero event counts {zero_ct} is at least 10 events predictor variable"))
       
     }
     if(non_zero_ct>=Threshold_zero){
       message(glue(" Total nonzero event counts {non_zero_ct} is at least 10 events predictor variable"))
       
     }
     if(zero_ct<Threshold_zero){
       warning(glue("Total zero event counts {zero_ct} is not at least 10 events predictor variable"))
       
     }
     if(non_zero_ct<Threshold_zero){
       warning(glue("Total nonzero event counts {non_zero_ct} is not at least 10 events predictor variable"))
     }
   } 
   
   
   # Dispersion 
   
   cat("\n#-----------------------------Dispersion Ratio Interpretation--------------------#\n")
   pearson.ratio <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
   
   rec <- if (pearson.ratio > 1.5) {
     "Overdispersion (dispersion ratio > 1.5) -> consider Quasi-Poisson or Negative Binomial"
   } else if (pearson.ratio < 0.7) {
     "Underdispersion (dispersion ratio < 0.7) -> consider Poisson may be appropriate, but check model fit"
   } else {
     "Dispersion is acceptable (0.7 <= dispersion ratio <= 1.5) -> Poisson model is reasonable"
   }
   
   label_text <- paste0(
     "Dispersion ratio (Pearson chi-square / df) = ",
     round(pearson.ratio, 3),
     ". ",
     rec
   )
   cat(label_text)
   
    # randomize quantile residual plot
    message("random quantile residual is plotted\n")
    message("person residual is plotted\n")
    print(plt_rdr(model))
    # pearson residual plot
   
    print(plt_pearson(model))
    
  if(fam %in% c("glmnb","glmpoisson")){
    message("random effect plot is plotted\n")
    is_mixed <- grepl("\\([^()]*\\|[^()]*\\)", deparse1(formula(model)))
    
    if(is_mixed==FALSE){
      
      warning("No random effects were detected in the model formula. Please specify a random effects term (e.g., (1 | group)) or use a model without random effects.")
    }else{
      print(plt_ref(model))
    }
    

  }
    
    

}



plt_rdr <- function(model){
  # from poisson
  counts <- model.response(model.frame(model))
  lambdas <- fitted(model)
  # rqr <- statmod::qres.pois(crime_model) does the same job
  rqr <- rep(NA, length(lambdas))
  ui_collection <- rep(NA, length(lambdas))
  for(i in 1:length(lambdas)){
    ai <- ppois(counts[i]-1, lambda=lambdas[i])
    bi <- ppois(counts[i], lambda=lambdas[i])
    # pick a random point between that jump
    # ai, bi and ui close to 0.5
    # similarly ui should also follow a uniform after being dealt with
    
    # runif(1) * (bi - ai) is what we generated
    # collection of ui from different lamdas are approximatly uniform [0,1]
    # differnet poisson distribution are being mapped to the same uniform [0,1]
    # ai and bi are probablity
    # ai + ai_bi random uniform sample is still between 0 and 1
    # we would want more ai bi at 0.5
    
    ui <- ai + runif(1) * (bi - ai)
    # get rid of very extremem probabilities? why?
    ui <- max(min(ui, 1-10^(-6)), 10^(-6))
    
    ui_collection[i] <- ui
    rqr[i] <- qnorm(ui)
  }
  
  
  pearson.ratio <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
  p1 <- ggplot(data=tibble(lambda=lambdas,
                           e=rqr)) + 
    geom_hline(yintercept=0, linetype="dotted")+
    geom_point(aes(x=lambda, y=e)) +
    theme_bw()+
    xlab(bquote(lambda))+
    ylab("Randomized Quantile Residuals")
  p2 <- ggplot(data=tibble(e=rqr)) +
    stat_qq(aes(sample=e)) +
    stat_qq_line(aes(sample=e)) +
    theme_bw() +
    ggtitle(paste("Dispersion Ratio =", round(pearson.ratio, 4))) +
    xlab("Theoritical") + 
    ylab("Observation")
  
  p1+p2
  
}



plt_pearson <- function(model){
  
  ggdat <- tibble(r2= resid(model, type = "pearson")^2,
                  lambdas = fitted(model))
  p1 <- ggplot(ggdat) + 
    geom_point(aes(x=lambdas, y=r2)) +
    geom_hline(yintercept = 1, linetype="dotted", color="red") +
    geom_smooth(aes(x=lambdas, y=r2)) +
    theme_bw() + 
    xlab(bquote(lambda)) +
    ylab(bquote(r^2))
  p1
}



plt_ref <- function(model){
  
  

dat <- model.frame(model)
dat <- dat |> mutate(y = model.response(dat))
dat <- dat |>
   mutate(y.hat = model.matrix(model) %*% fixef(model)$cond) |>
   mutate(r.marginal =  - y.hat)


re <- ranef(model)
  
if (is.null(re) || length(re) == 0 || is.null(re$cond)) {
  stop("plt_ref(): no random effects found in this model")
}



subject <- names(ranef(model)$cond)

# random terms
re_mat <- ranef(model)$cond[[subject]]
re_term <- setdiff(colnames(re_mat), "(Intercept)")
Sigma.gamma <- as.matrix(VarCorr(model)$cond[[subject]]) # estimated covariance 
subjects <- unique(dat[[subject]])
  
margres.dist <- tibble(subject = subjects,
                       n = NA,
                       MD = NA)

for(i in 1:length(subjects)) {
   # Take marginal residual vector for ith subject
  curr.dat <- dat |>
     filter(.data[[subject]] == subjects[[i]])
  curr.margres <- curr.dat$r.marginal
  curr.n <- nrow(curr.dat)
  
   # Compute Sigma_epsilon
   var.r <- sigma(model)^2 # estimated conditional error variance
   if(length(re_mat) ==1){
    Zi <-rep(1, nrow(curr.dat))
   } else if(length(re_mat)>1){
     Zi <- cbind(1,  as.matrix(curr.dat[, re_term, drop = FALSE])) # 1 | x for random effects design
   }

  Sigma.epsilon.i <- Zi %*% Sigma.gamma %*% t(Zi) + diag(var.r, nrow(curr.dat))
   # Compute Distance
   margres.dist$MD[i] = t(curr.margres) %*% solve(Sigma.epsilon.i) %*% curr.margres
   margres.dist$n[i] = curr.n
}


degree_of_freedom <- unique(margres.dist$n)
if(length(degree_of_freedom) > 1){
  message(
    "Degrees of freedom vary across subject ",
    "Mahalanobis distances do not follow a single chi-square distribution. ",
    "A direct chi-square QQ plot would be invalid; transformed (uniform/normal) QQ plots are used instead."
  )
  margres.dist <- margres.dist |>
    rowwise() |>
    mutate(prob = pchisq(q = MD, df = n, lower.tail = F)) |>
    mutate(z = qnorm(1 - prob))
  
  p1 <- ggplot(margres.dist) +
    stat_qq(aes(sample = prob), distribution = qunif) +
    stat_qq_line(aes(sample = prob)) +
    theme_bw()+
    labs(
      title = "QQ Plot (Uniform[0,1])",
      x = "Theoretical Quantiles (Uniform[0,1])",
      y = "Observed Quantiles"
    )
  
  p2<- ggplot(margres.dist) +
    stat_qq(aes(sample = z)) +
    stat_qq_line(aes(sample = z)) +
    theme_bw()  +
    labs(
      title = "QQ Plot (Standard Normal)",
      x = "Theoretical Quantiles (Normal)",
      y = "Observed Quantiles"
    )
  
  p1 | p2
} else{
  df <- degree_of_freedom[1]
  
  p1 <- ggplot(data = tibble(MD = margres.dist$MD)) +
     stat_qq(aes(sample = MD),
               distribution = qchisq,
               dparams=list(df=df))+
     stat_qq_line(aes(sample = MD),
                    distribution = qchisq,
                    dparams=list(df=df)) +
    theme_bw()+
    ylab("Observed Quantile") +
    xlab("Theoretical Quantile")
}
}


vif_report <- function(model) {
  
  vif_df <- suppressMessages(
    suppressWarnings(
      performance::check_collinearity(model)
    )
  )  

  is_interaction <- grepl(":", vif_df$Term)
  
  interaction_terms <- vif_df$Term[is_interaction]
  
  interaction_vars <- unique(unlist(strsplit(interaction_terms, ":")))
  
  # keep ONLY variables NOT involved in ANY interaction
  vif_main <- vif_df[!vif_df$Term %in% c(interaction_vars,interaction_terms), ]
  
  
  VIF <- vif_main$VIF
  names(VIF) <- vif_main$Term
  
  vartext2 <- ""
  
  if (any(VIF > 5)) {
    
    vartext2 <- "\n-- We calculated the Variance Inflation Factor (VIF) for MAIN EFFECTS ONLY(exluding interaction and lower term). --\n"
    
    viftext <- paste0("VIF(", names(VIF), ")=", round(VIF, 2))
    
    high <- VIF > 5
    
    if (sum(high) == 1) {
      
      vartext2 <- paste0(
        vartext2,
        viftext[high],
        " has high VIF, indicating potential multicollinearity among main effects."
      )
      
    } else {
      
      viftext_hi <- viftext[high]
      
      viftext_hi <- paste(
        paste(viftext_hi[-length(viftext_hi)], collapse = ", "),
        "and",
        viftext_hi[length(viftext_hi)]
      )
      
      vartext2 <- paste0(
        vartext2,
        viftext_hi,
        " have high VIF, indicating potential multicollinearity among main effects."
      )
    }
    
  } else {
    
    vartext2 <- paste0(
      vartext2,
      "No VIF exceeded 5, suggesting no serious multicollinearity among non-interaction predictors."
    )
  }

  cat(vartext2)
}