#' Build text interpretation for emtrends contrast results
#'
#' The functon interprets the result of pairs emtrends tables
#' It reports whether effects differ significantly across levels of a moderator,
#' along with estimate, test statistics, and p-values.
#'
#' @param model Fitted model object (e.g., glm, count regression model)
#' @param mod.emtrendcontrast Data frame of emtrends contrast results
#' @param pred Name of predictor variable
#' @param moderator Name of moderator variable
#' @param alpha Significance level (default = 0.05)
#'
#' @return A formatted character string summarizing interaction contrasts
#'
#' @details
#' Each row in the contrast table is converted into a bullet-point sentence
#' describing whether the effect of the predictor differs across levels of
#' the moderator.
#'
#' @examples
#' build_emtrendcontrast_text(model, contrast_table, "drat", "vs")
build_emtrendcontrast_text <- function(model, mod.emtrendcontrast, pred, moderator, alpha = 0.05){
  
  if (!is.data.frame(mod.emtrendcontrast)) {
    mod.emtrendcontrast <- as.data.frame(mod.emtrendcontrast)
  }
  
  if (nrow(mod.emtrendcontrast) == 0) {
    return("No emmeans results available.")
  }
  
  # ---------------- response varaible ----------------
  data <-model.frame(model)
  pred_type <- if (is.numeric(data[[pred]])) "cont" else "factor"
  mod_type  <- if (is.numeric(data[[moderator]])) "cont" else "factor"
  response <- all.vars(formula(model))[1]

  
  emtrends.interp<-""
  for(i in 1:nrow(mod.emtrendcontrast)){
    emtrends.interp <- paste(emtrends.interp,"\U2022 The effect of ", gsub("\\.scaled", "", pred),  " is ", ifelse(mod.emtrendcontrast[i,"p-value"]<alpha, "significantly ", "not significantly "),
                             "different by ", gsub("\\.scaled", "", moderator), 
                             " (Contrast = ", mod.emtrendcontrast[i,"Contrast"],
                             ", Estimate = ", round(mod.emtrendcontrast[i,"Estimate"],4),
                             ", z ratio = ", round(mod.emtrendcontrast[i,"z ratio"],4),
                             ", p-value ",  ifelse(mod.emtrendcontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ", round(mod.emtrendcontrast[i,"p-value"],4), sep="")), ").\n",
                             sep="")
  }
  
  return(emtrends.interp)
}
  
