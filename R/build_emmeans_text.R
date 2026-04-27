build_emmeans_text <- function(model, mod.emmeans, pred, moderator) {
  
  
  if (!is.data.frame(mod.emmeans)) {
    mod.emmeans <- as.data.frame(mod.emmeans)
  }
  
  if (nrow(mod.emmeans) == 0) {
    return("No emmeans results available.")
  }
  
  # ---------------- response varaible ----------------
  data <-model.frame(model)
  pred_type <- if (is.numeric(data[[pred]])) "cont" else "factor"
  mod_type  <- if (is.numeric(data[[moderator]])) "cont" else "factor"
  base_response <-  all.vars(formula(model))[1]
  fam <- get_ct_family(model)
  link <- if (fam %in% c("poisson", "qpoisson", "negbin")) {
    model$family$link
  } else if (fam %in% c("zip", "zinb")) {
    "log"
  } else {
    NA
  }
  
  response <- link
  

  # ---------------- building text ----------------

  message("Emmeans is evaluated on the response scale")


  emmeans.text <- ""
  
  for (i in 1:nrow(mod.emmeans)) {
    
    if("rate" %in% names(mod.emmeans)){
    emmeans.text <- paste(emmeans.text,"\n\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", pred), " = ", as.character(mod.emmeans[i,pred]) ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"rate"],2),
                          " (95% CI: ", round(mod.emmeans[i,"asymp.LCL"],2), ", ", round(mod.emmeans[i,"asymp.UCL"],2), ").",
                          sep="")

    } else{
      
      emmeans.text <- paste(emmeans.text,"\n\U2022 The estimated marginal mean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", pred), " = ", as.character(mod.emmeans[i,pred]) ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"emmean"],2),
                            " (95% CI: ", round(mod.emmeans[i,"asymp.LCL"],2), ", ", round(mod.emmeans[i,"asymp.UCL"],2), ").",
                            sep="")

    }
    
  }
  
  emmeans.text <paste(emmeans.text, "<br/><strong>Note:</strong> This approach contrasts the estimated marginal means with a Tukey adjustment for multiple comparisons. These values are calculated at the 'average' of the other variables in the model.",
         "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")

  
  return(emmeans.text)
}



