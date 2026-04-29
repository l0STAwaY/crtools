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
  response <- all.vars(formula(model))[1]
  

  # ---------------- building text ----------------

  message("Emmeans is evaluated on the response scale")


  emmeans.text <- ""
    if("rate" %in% names(mod.emmeans)){
    if (pred_type == "cont" && mod_type == "cont"){ #Both Continuous
      
      for(i in 1:nrow(mod.emmeans)){
        emmeans.text <- paste(emmeans.text,"\U2022 The emmean of ", sub("\\.scaled$", "", names(model$model)[1]), " for ", sub("\\.scaled$", "", pred), " = ", mod.emmeans[i,pred] ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"rate"],2),
                              " (95% CI: ", round(mod.emmeans[i,"asymp.LCL"],2), ", ", round(mod.emmeans[i,"asymp.UCL"],2), "). \n",
                              sep="")
      }
    }else if(pred_type == "factor" && mod_type == "factor"){ #Both Factors
      for(i in 1:nrow(mod.emmeans)){
        emmeans.text <- paste(emmeans.text,"\U2022 The emmean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", pred), " = ", as.character(mod.emmeans[i,pred]) ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"rate"],2),
                              " (95% CI: ", round(mod.emmeans[i,"asymp.LCL"],2), ", ", round(mod.emmeans[i,"asymp.UCL"],2), "). \n",
                              sep="")
      }
    }else{
      if(mod_type=="factor"){
        for(i in 1:nrow(mod.emmeans)){
          emmeans.text <- paste(emmeans.text, "\U2022 The emmean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", pred), " = ", mod.emmeans[i,pred] ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"rate"],2),
                                " (95% CI: ", round(mod.emmeans[i,"asymp.LCL"],2), ", ", round(mod.emmeans[i,"asymp.UCL"],2), "). \n",
                                sep="")
        }
      }else{
        for(i in 1:nrow(mod.emmeans)){
          emmeans.text <- paste(emmeans.text,"\U2022 The emmean of ", sub("\\.scaled$", "", response), " for ", sub("\\.scaled$", "", pred), " = ", as.character(mod.emmeans[i,pred]) ," and ", sub("\\.scaled$", "", colnames(mod.emmeans[moderator])), " = " , as.character(mod.emmeans[i,moderator]), " is ", round(mod.emmeans[i,"emmean"],2),
                                " (95% CI: ", round(mod.emmeans[i,"asymp.LCL"],2), ", ", round(mod.emmeans[i,"asymp.UCL"],2), "). \n",
                                sep="")
        }
      }
    }
    } 

  
  emmeans.text <- paste(emmeans.text, "\nNote: This approach contrasts the emmeans with a Tukey adjustment for multiple comparisons. These values are calculated at the 'average' of the other variables in the model.",
         "You may want to set the other variables to specific values, which is supported in R but not currently supported in this application.")

  
  return(emmeans.text)
}



