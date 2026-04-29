#' Build Emtrends Contrast Table
#'
#' Computes pairwise contrasts of estimated marginal trends (slopes) from a fitted model.
#' This is used for interaction effects where the effect of a continuous predictor varies
#' across levels of a moderator.
#'
#' @param model Fitted regression model supported by emmeans.
#' @param pred Continuous predictor variable name (character string).
#' @param moderator Grouping variable (factor or continuous) used as moderator.
#'
#' @return A data frame containing pairwise slope contrasts, including estimates,
#' standard errors, test statistics, and adjusted p.values.
#'
#' @details
#' Uses `emmeans::emtrends()` to estimate slopes and `pairs()` to compute
#' differences between slopes across groups. Results are rounded for readability
#' and p.values below 0.0001 are displayed as "<0.0001".
#'
#' @examples
#' build_trends_tab(model, pred = "hp", moderator = "vs")
build_emtrendcontrast_tab <- function(model, pred, moderator) {
  
  data <- model.frame(model)
  # ---------------- variable types ----------------
  pred_type <- if (is.numeric(data[[pred]])) "cont" else "factor"
  mod_type  <- if (is.numeric(data[[moderator]])) "cont" else "factor"
  
  if(pred_type=="cont" && mod_type =="cont"){ #Both Continuous
    "####################################"
    "# Calculate Marginal Effects"
    "####################################"
    m<-mean(data[[moderator]]), na.rm=T)
    s<-sd(data[[moderator]]), na.rm=T)
    modvarat<-list(c(round(m-s,2), round(m+s,2)))
    names(modvarat)<-c(moderator)
    
    mod.emtrends<-emtrends(object = model, spec=moderator, var=pred, at=modvarat)

    mod.emtrendcontrast <- data.frame(pairs(mod.emtrends))
    mod.emmeanscontrastci <-data.frame(confint(pairs(mod.emtrends)))
    mod.emtrendcontrast$lower.CL<-mod.emmeanscontrastci$asymp.LCL
    mod.emtrendcontrast$upper.CL<-mod.emmeanscontrastci$asymp.UCL
    
    "####################################"
    "# Clean Up Labels for Printing"
    "####################################"
    mod.emtrendcontrast %>%
      dplyr::mutate_if(is.numeric, round, 4) %>%
      dplyr::rename(
        `z ratio` = z.ratio,
        `p-value` = p.value,
        `Lower CI` = lower.CL,
        `Upper CI` = upper.CL,
        Estimate = estimate,
        Contrast = contrast
      )
    
    
  }else if(pred_type=="factor" && mod_type =="factor"){ #Both Factors
    # HIDE emtrends_tab, emtrends_int
  }else{
    if(mod_type =="factor"){
      
      "####################################"
      "# Calculate Marginal Effects"
      "####################################"
      
       mod.emtrends<-emtrends(model, spec = moderator, var = pred)
       mod.emtrendcontrast <- as.data.frame(pairs(mod.emtrends))
       mod.emmeanscontrastci <-data.frame(confint(pairs(mod.emtrends)))
       mod.emtrendcontrast$lower.CL<-mod.emmeanscontrastci$asymp.LCL
       mod.emtrendcontrast$upper.CL<-mod.emmeanscontrastci$asymp.UCL
       
      "####################################"
      "# Clean Up Labels for Printing"
      "####################################"
      
      mod.emtrendcontrast %>%
        dplyr::mutate_if(is.numeric, round, 4) %>%
        dplyr::rename(
          `z ratio` = z.ratio,
          `p-value` = p.value,
          `Lower CI` = lower.CL,
          `Upper CI` = upper.CL,
           Estimate = estimate,
           Contrast = contrast
        )
                     

    }else{
      # HIDE emtrends_tab, emtrends_int
    }
  }
}


