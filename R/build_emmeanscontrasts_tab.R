library(magrittr)
#' Build Emmeans Contrasts
#' Computes pairwise contrasts of estimated marginal means (EMMs)
#' from a fitted regression model with interactions.
#'
#' @param model Fitted model object (must support emmeans).
#' @param pred Name of predictor variable (character string).
#' @param moderator Name of moderator variable (character string).
#'
#' @return Data frame of pairwise contrasts including estimates,
#' standard errors, test statistics, p-values, and confidence intervals.
#'
#' @details
#' Handles both continuous and factor variables. Continuous variables
#' are evaluated at approximately ±1 standard deviation from the mean.
#' Results are reported on the response scale.
#'
#' @examples
#' model <- fit_ct(y ~ hp * mpg, data = mtcars, family = "negbin")
#' build_emmeanscontrasts(model, pred = "hp", moderator = "mpg")
build_emmeanscontrasts_tab <- function(model, pred, moderator){
  data <- model.frame(model)
  # ---------------- variable types ----------------
  pred_type <- if (is.numeric(data[[pred]])) "cont" else "factor"
  mod_type  <- if (is.numeric(data[[moderator]])) "cont" else "factor"
  
  fam <- get_ct_family(model)
  
  message("Contrasts computed on RESPONSE scale (type = response)")
  
 
  if (pred_type == "cont" && mod_type == "cont") { #Both Continuous
      m.mod<-mean(unlist(model$model[moderator]), na.rm=T)
      s.mod<-sd(unlist(model$model[moderator]), na.rm=T)
      m.var<-mean(unlist(model$model[pred]), na.rm=T)
      s.var<-sd(unlist(model$model[pred]), na.rm=T)
      modvarat<-list(c(round(m.mod-s.mod,2), round(m.mod+s.mod,2)),
                     c(round(m.var-s.var,2), round(m.var+s.var,2)))
      names(modvarat)<-c(moderator, pred)
      
      mod.emmeans<-emmeans(object = model, spec=c(pred, moderator), var=pred, at=modvarat, type="response")
      mod.emmeans <- add_grouping(mod.emmeans, "pred", pred , c(paste("(Low ", pred,")", sep=""), paste("(High ", pred,")", sep="")))
      mod.emmeans <- add_grouping(mod.emmeans, "moderator", moderator , c(paste("(Low ", moderator,")", sep=""), paste("(High ", moderator,")", sep="")))
      
      mod.emmeanscontrast <-data.frame(pairs(emmeans(mod.emmeans, c("pred","moderator"), type="response")))
      mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
      mod.emmeanscontrast$lower.CL<-mod.emmeanscontrastci$asymp.LCL
      mod.emmeanscontrast$upper.CL<-mod.emmeanscontrastci$asymp.UCL
      
      mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
   
      "####################################"
      "# Clean Up Labels for Printing"
      "####################################"
      mod.emmeanscontrast <- mod.emmeanscontrast %>%
        dplyr::select(-any_of("null"))%>%
        set_rownames(NULL) %>%
        set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))

  }
  
  
  else if(pred_type == "factor" && mod_type == "factor"){ #Both Factors
      "####################################"
      "# Calculate Marginal Effects"
      "####################################"
      mod.emmeans<-emmeans(model, specs = c(pred,moderator), var=moderator, type="response")
      mod.emmeanscontrast <-data.frame(pairs(mod.emmeans))
      mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
      mod.emmeanscontrast$lower.CL<-mod.emmeanscontrastci$asymp.LCL
      mod.emmeanscontrast$upper.CL<-mod.emmeanscontrastci$asymp.UCL
      
      mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
      
      "####################################"
      "# Clean Up Labels for Printing"
      "####################################"
      mod.emmeanscontrast <- mod.emmeanscontrast %>%
        dplyr::select(-any_of("null"))%>%
        set_rownames(NULL) %>%
        set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))
 
  }else{ 
    if(mod_type=="factor"){
  
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        m<-mean(unlist(model$model[pred]), na.rm=T)
        s<-sd(unlist(model$model[pred]), na.rm=T)
        modvarat<-list(c(round(m-s,2), round(m+s,2)))
        names(modvarat)<-c(pred)
        
        mod.emmeans <- emmeans(model, specs = c(moderator,pred), var=moderator, at=modvarat, type="response")
        mod.emmeans <- add_grouping(mod.emmeans, "pred", pred ,  c(paste("(Low ",pred,")", sep=""), paste("(High ",pred,")", sep="")))
        mod.emmeanscontrast <-data.frame(pairs(mod.emmeans))
        mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
        mod.emmeanscontrast$lower.CL<-mod.emmeanscontrastci$asymp.LCL
        mod.emmeanscontrast$upper.CL<-mod.emmeanscontrastci$asymp.UCL
        
        mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeanscontrast <- mod.emmeanscontrast %>%
          dplyr::select(-any_of("null"))%>%
          set_rownames(NULL) %>%
          set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))

    }else{
        "####################################"
        "# Calculate Marginal Effects"
        "####################################"
        m<-mean(unlist(model$model[moderator]), na.rm=T)
        s<-sd(unlist(model$model[moderator]), na.rm=T)
        
        modvarat<-list(c(round(m-s,2), round(m+s,2)))
        names(modvarat)<-c(moderator)
        
        mod.emmeans <- emmeans(model, specs = c(moderator, pred), var=moderator, at=modvarat, type="response")
        mod.emmeans <- add_grouping(mod.emmeans, "moderator", moderator ,  c(paste("(Low ",moderator,")", sep=""), paste("(High ",moderator,")")))
        mod.emmeanscontrast <-data.frame(pairs(mod.emmeans))
        
        mod.emmeanscontrastci<-data.frame(confint(pairs(mod.emmeans)))
        mod.emmeanscontrast$lower.CL<-mod.emmeanscontrastci$asymp.LCL
        mod.emmeanscontrast$upper.CL<-mod.emmeanscontrastci$asymp.UCL
        
        mod.emmeanscontrast$contrast <- gsub("\\.scaled", "", mod.emmeanscontrast$contrast)
        
        "####################################"
        "# Clean Up Labels for Printing"
        "####################################"
        mod.emmeanscontrast <- mod.emmeanscontrast %>%
          dplyr::select(-any_of("null"))%>%
          set_rownames(NULL) %>%
          set_colnames(c("Contrast", "Estimate", "SE", "df", "z ratio", "p-value", "Lower CI", "Upper CI"))
    }
  }
  }  
  
  
  

  
  
 