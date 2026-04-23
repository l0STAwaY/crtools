library(car)
library(glue)
library(DHARMa)
plt_rdr <- function(model){
  # from poisson
  counts <- model$y
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
  
  
  pearson.ratio <- sum(residuals(model, type = "pearson")^2) / model$df.residual
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



chk_count <- function(model){
  
   model_type <- model$family
   chk_table <- tibble(
     Enough_Response_Count = NULL,
     Enough_Zero_Count = NULL,
     Enough_NonZero_Count= NULL,
     MultiColiner_Issue = NULL,
     Dispersion_Ratio = NULL
   )
   
   
   if(model$ct_family %in% c("poisson","qpoisson","negbin")){
     
     # Condition 2, little to no multi-colinearity
     
     if(length(coef(model))<=2){
       message("no vif reported: model contains fewer than 2 terms" )
     }
     else{
       vif(model)
     }



     # Condition 3 Enough count 
     mod_dta <- model$data
     y_ct <- mean(model$model[[1]])
     Threshold <- 10 * length(coef(model))
     if(y_ct>=Threshold){
       message(" Total event counts is at least 10-20 events predictor variable")
       
     }
     else{
       warning(" Total event counts is not at least 10-20 events predictor variable")
     }
     
     
     # Test Zero Inflation
     testZeroInflation(model)
     }
     
   
   else if(model$ct_family %in% c("zip","zinb")){
     
    
     # Condition 1, little to no Multi-Colinearity
     
     if(length(model$coefficients$count)<=2){
       message("no vif reported model contains fewer than 2 terms" )
    
     }
     else{
       if(model$ct_family=="zip"){
         car::vif(fit_ct(model$formula,model$model,family= "poisson"))
       }
       else if((model$ct_family=="zinb")){
         car::vif(fit_ct(model$formula,model$model,family= "zinb"))
       }
     }


     
     # Condition 2 Enough count for predictor
     mod_dta <- model$data
     y_ct <- mean(model$model[[1]])
     Threshold_count <- 10 * length(model$coefficients$count)
 
    

     if(y_ct>=Threshold_count){
       message(glue("Total event counts {y_ct} is at least 10 events predictor variable"))
     }
     else{
       warning(" Total event counts is not at least 10 events predictor variable")
     }
     

     
     
     # Condition 3 Enough 0
     mod_dta <- model$data
     zero_ct <- sum(model$model[[1]]==0)
     non_zero_ct <- sum(model$model[[1]]!=0)
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
   
   # randomize quantile residual plot
    print(plt_rdr(model))
    
    # pearson residual plot
    print(plt_pearson(model))

}







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
