#' Build Emmeans Contrasts Text
#'
#' Converts estimated marginal mean (EMM) contrasts into readable text
#' for reporting.
#'
#' @param model Fitted model object used to identify response and variable types.
#' @param mod.emmeanscontrast Data frame of contrasts from emmeans (pairs + CI).
#' Must include: Contrast, p-value, z ratio, df, Lower CI, Upper CI.
#' @param pred Predictor variable name (character).
#' @param moderator Moderator variable name (character).
#' @param alpha Significance threshold (default = 0.05).
#'
#' @return Character string with formatted contrast descriptions.
#'
#' @details
#' - Automatically detects factor vs continuous variables
#' - Reports significance, test statistic, p-value, and CI
#' - Returns newline-separated bullet points
#'
#' @examples
#'
#' @export
build_emmeanscontrasts_text <- function(model, mod.emmeanscontrast, pred, moderator, alpha = 0.05){
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
  
  if (pred_type == "cont" && mod_type == "cont"){ #Both Continuous
    text <- ""
    for(i in 1:nrow(mod.emmeanscontrast)){
      adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", mod.emmeanscontrast$Contrast[i]),") is",
                           ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " significant ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"z ratio"],2),
                           ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                           "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                           sep="")
      text <- paste(text, adding.text, sep="\n")
    }
    return(text)
  }else if(pred_type == "factor" && mod_type == "factor"){ #Both Factors
    text <- ""
    for(i in 1:nrow(mod.emmeanscontrast)){
      adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", mod.emmeanscontrast$Contrast[i]),") is",
                           ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " significant ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"z ratio"],2),
                           ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                           "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                           sep="")
      text <- paste(text, adding.text, sep="\n")
    }
    return(text)
  }else{
    if(mod_type =="factor"){
      text <- ""
      for(i in 1:nrow(mod.emmeanscontrast)){
        
        adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", as.character(mod.emmeanscontrast$Contrast[i])),")", " groups are",
                             ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " significant ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"z ratio"],2),
                             ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                             "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                             sep="")
        text <- paste(text, adding.text, sep="\n")
      }
      return(text)
    }else{
      text <- ""
      for(i in 1:nrow(mod.emmeanscontrast)){
        adding.text <- paste("\U2022 The contrast of estimated marginal means of ", sub("\\.scaled$", "", response), " for", " (", gsub("\\.scaled", "", mod.emmeanscontrast$Contrast[i]),") is",
                             ifelse(mod.emmeanscontrast[i,"p-value"]<alpha, " significant ", " not significant "), "(t = ", round(mod.emmeanscontrast[i,"z ratio"],2),
                             ", df = ", round(mod.emmeanscontrast[i,"df"]), ", p-value ", ifelse(mod.emmeanscontrast[i,"p-value"]<0.0001,"< 0.0001", paste("= ",round(mod.emmeanscontrast[i,"p-value"],4), sep="")),
                             "; 95% CI: ", round(mod.emmeanscontrast[i,"Lower CI"],2), ", ", round(mod.emmeanscontrast[i,"Upper CI"],2), ").",
                             sep="")
        text <- paste(text, adding.text, sep="\n")
      }
      return(text)
    }
  }
}
