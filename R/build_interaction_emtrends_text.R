#' Build interpretation text for emtrends output
#'
#' Generates a human-readable summary of marginal effects from emtrends,
#' describing how the effect of a predictor varies across levels of a moderator.
#'
#' @param model Fitted regression model object
#' @param mod.emtrends Output from emmeans::emtrends (data.frame or emmGrid)
#' @param pred Name of continuous predictor variable
#' @param moderator Name of grouping/moderating variable
#' @param alpha Significance threshold (default = 0.05)
#'
#' @return Character string with formatted interpretation text
#'
build_interaction_emtrends_text <- function(model, mod.emtrends, pred, moderator,alpha=0.05){
  
  
  if (!is.data.frame(mod.emtrends)) {
    mod.emtrends <- as.data.frame(mod.emtrends)
  }
  
  if (nrow(mod.emtrends) == 0) {
    return("No emmeans results available.")
  }

  # ---------------- response varaible ----------------
  data <-model.frame(model)
  pred_type <- if (is.numeric(data[[pred]])) "cont" else "factor"
  mod_type  <- if (is.numeric(data[[moderator]])) "cont" else "factor"

  emtrends.interp<-""
  for(i in 1:nrow(mod.emtrends)){
    if(class(mod.emtrends[,1])=="factor" | class(mod.emtrends[,1])=="character"){
      emtrends.interp <- paste(emtrends.interp,"\U2022 The effect of ", gsub("\\.scaled", "", (pred)),  " is ", ifelse(mod.emtrends[i,"p-value"]<alpha, "significant ", "not significant "),
                               "when ", gsub("\\.scaled", "", (moderator)), " is ", mod.emtrends[i,1],
                               " (Slope = ", round(mod.emtrends[i,2],4),
                               ", z ratio = ", round(mod.emtrends[i,"z ratio"],4),
                               ", p-value ",  ifelse(mod.emtrends[i,"p-value"]<0.0001,"< 0.0001", paste("= ", round(mod.emtrends[i,"p-value"],4), sep="")), ").\n",
                               sep="")
    }else{
      emtrends.interp <- paste(emtrends.interp,"\U2022 The effect of ", gsub("\\.scaled", "", (pred)),  " is ", ifelse(mod.emtrends[i,"p-value"]<alpha, "significant ", "not significant "),
                               "when ", gsub("\\.scaled", "", (moderator)), " is ", round(mod.emtrends[i,1],4),
                               " (Slope = ", round(mod.emtrends[i,2],4),
                               ", z ratio = ", round(mod.emtrends[i,"z ratio"],4),
                               ", p-value ",  ifelse(mod.emtrends[i,"p-value"]<0.0001,"< 0.0001", paste("= ", round(mod.emtrends[i,"p-value"],4), sep="")), ").\n",
                               sep="")
    }
  }
  return(emtrends.interp)
}

  
  
  