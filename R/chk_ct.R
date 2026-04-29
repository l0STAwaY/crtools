library(car)
library(glue)
library(DHARMa)
library(patchwork)
chk_ct <- function(model){
   
   # model family
   fam <- get_ct_family(model)
   

  
   if(fam %in% c("poisson","qpoisson","negbin","glmnb","glmpoisson")){
    
     # Condition 3 Enough count 
     
     cat("#-------------Count Per Predictor Interpretation-------------#")
     y_ct <- sum(model.response(model.frame(model)))
     Threshold <- 10 * length(coef(model))
     if(y_ct>=Threshold){
       message(" Total event counts is at least 10-20 events predictor variable")
     }else{
       warning(" Total event counts is not at least 10-20 events predictor variable")
     }
     
     
     
     # Condition 2, little to no multi-colinearity
     cat("#-------------VIF Model and Interpretation-------------#")
     if(length(coef(model))<=2){
       message("no vif reported: model contains fewer than 2 terms" )
     }
     else{
       print(vif(model))
       cat(vif_report(model))
     }
     # Test Zero Inflation
     testZeroInflation(model)
     }
     
   else if(fam %in% c("zip","zinb")){
     
    
     # Condition 1, little to no Multi-Colinearity
     
     cat("#-------------VIF Model and Interpretation-------------#")
     if(length(model$coefficients$count)<=2){
       message("no vif reported model contains fewer than 2 terms" )
    
     }
     else{
       if(fam=="zip"){
         print(vif(model))
         cat(vif_report(model))
         # car::vif(fit_ct(model$formula,model$model,family= "poisson"))
       }
       else if((fam=="zinb")){?
         print(vif(model))
         cat(vif_report(model))
         # car::vif(fit_ct(model$formula,model$model,family= "zinb"))
       }
     }
     
     # Condition 2 Enough count for predictor
     y_ct <- sum(model.response(model.frame(model)))
     Threshold_count <- 10 * length(model$coefficients$count)
      
     cat("#-------------Count and Zero Per Predictor Interpretation-------------#")

     if(y_ct>=Threshold_count){
       
       message(glue("Total event counts {y_ct} is at least 10 events predictor variable"))
     }
     else{
       warning(" Total event counts is not at least 10 events predictor variable")
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
   
   cat("#-------------Dispersion Ratio Interpretation-------------#")
   pearson.ratio <- sum(residuals(model, type = "pearson")^2) / df.residual(model)
   
   rec <- if (pearson.ratio > 1.5) {
     "Overdispersion -> consider Quasai Poisson or NegBin"
   } else if (pearson.ratio < 0.7) {
     "Underdispersion -> consider Poisson"
   } else {
     "Dispersion OK -> Poisson reasonable"
   }
   
   label_text <- paste0(
     "Recommendation: ", rec
   )
   
    # randomize quantile residual plot
    message("random quantile residual is plotted\n")
    print(plt_rdr(model))
    # pearson residual plot
    message("random quantile residual is plotted\n")
    print(plt_pearson(model))
    
  if(fam %in% c("glmnb","glmpoisson")){
    message("random effect plot is plotted\n")
    print(plt_ref(model))
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
  
  VIF <- vif(model)
  vartext2 <- ""
  
  if (any(VIF > 5)) {
    
    vartext2 <- "We calculated the Variance Inflation Factor (VIF) to assess collinearity of the model -- "
    viftext <- paste("VIF(", names(VIF), ")=", round(VIF, 2), sep = "")
    
    if (sum(VIF > 5) == 1) {
      vartext2<-paste(vartext2, viftext, " has high VIF, indicating that collinearity may be an issue. You might consider standardizing your data or using a regularization method like ridge regression or LASSO.")
    } else {
      
      viftext <- paste(paste(viftext[-length(viftext)], collapse = ", ")," and ",viftext[length(viftext)],sep = "")
      vartext2<-paste(vartext2, viftext, " have high VIF, indicating that collinearity may be an issue. You might consider standardizing your data or using a regularization method like ridge regression or LASSO.")
    }
    
  } else {
    vartext2<-paste(vartext2, "We calculated the Variance Inflation Factor (VIF) to assess collinearity of the model. None of the calculated VIFs were larger than five, indicating that collinearity may not be an issue.", sep="")
  }
  
  return(vartext2)
}




model2 <- glmmTMB(
  y ~ mpg + cyl + offset(log(exposure)) + (1 | gear),
  data = data,
  family = poisson()
)


# check this







# 
# data(mpg, package = "ggplot2")
# 
# 
# 
# colnames(fit_ct(cyl ~ displ, mpg, family  = "poisson")$model)[1]
# mpg
# 
# 
# # 
# 
# model
# coef(fit_ct(cyl ~ displ, mpg, family  = "poisson"))
# str(fit_ct(cyl ~ displ, mpg, family  = "poisson"))
# Threshold <- 10
# 
# str(fit_ct(cyl ~ displ, mpg, family  = "poisson"))
#      y_ct <- mean(model$model[1])
# chk_count(fit_ct(cyl ~ displ, mpg, family  = "poisson"))
# 
# 
# 
# y_ct <- mean(fit_ct(cyl ~ displ, mpg, family  = "poisson")$model[[1]])
# 
# plt_rdr(fit_ct(cyl ~ displ, mpg, family  = "poisson"))
# # Poisson
# t <-  fit_ct(cyl ~ displ, mpg, family  = "poisson")$family$family == "poisson"
# 
# 
# # Quasai Poisson
# t <-  fit_ct(cyl ~ displ, mpg, family  = "qpoisson")$family
# 
# summary(fit_ct(cyl ~ displ, mpg, family  = "qpoisson"))
# # Negative Binomial
# t <- fit_ct(cyl ~ displ, mpg, family  = "negbin")$family$family
# 
# 
# # Zero Inflation Poisson
# mpg_zip <- mpg 
# zero_idx <- sample(1:nrow(mpg_zip), size = 2)  # inject 5 zeros to the data
# mpg_zip$cyl[zero_idx] <- 0
# 
# mpg_zip
# chk_count(fit_ct(cyl ~ displ + hwy , mpg_zip, family  = "poisson"))
# 
# # Zero Inflation Negative Binomial
# 
# fit_ct(cyl ~ displ, mpg_zip, family  = "zinb")$terms[1]
# 
# fit_ct(cyl ~ displ, mpg_zip, family  = "zinb")$ct_family
# 
# coeff(fit_ct(cyl ~ displ, mpg_zip, family  = "zinb"))
# $coefficients$count)
# [1]
# car::vif(fit_ct(cyl ~ displ + hwy, mpg_zip, family  = "poisson"))
# 
# temp <- fit_ct(cyl ~ displ + hwy, mpg_zip, family  = "zinb")
# str(temp)
# car::vif(fit_ct(temp$formula,temp$model,family= "poisson"))
# 
# class(temp$formula)
# chk_count(fit_ct(cyl ~ displ, mpg_zip, family  = "zinb"))
# str(fit_ct(cyl ~ displ, mpg_zip, family  = "zinb"))
# 
# 
# plt_pearsonfit_ct(cyl ~ displ, mpg_zip, family  = "zinb")$df.residual
