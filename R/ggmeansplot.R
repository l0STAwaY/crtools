library(ggeffects)
library(emmeans)
library(dplyr)
library(ggplot2)
#' Plot model-based predictions for interactions
#'
#' Uses ggeffects::ggemmeans to plot predicted values from count models.
#' Automatically handles continuous and categorical predictors and moderators.
#' Displays confidence intervals when available.
#'
#' @param model Fitted model (glm, nb, zip, zinb, etc.)
#' @param data Original dataset used in the model
#' @param pred Name of predictor variable (character)
#' @param moderator Name of moderator variable (character)
#' @param se.on Show confidence intervals (TRUE/FALSE)
#' @param y.label Label for y-axis
#'
#' @return ggplot object
#' 
#' Variable types are automatically inferred: where numeric are treated as continous and factor are categorical
#'
#' @section Supported interaction types:
#' \itemize{
#'   \item Continuous × Continuous
#'   \item Continuous × Factor
#'   \item Factor × Factor
#' }
#' 
#' 
#' @examples
#' \dontrun{
#' m1 <- glm(y ~ hp * vs, data = mtcars, family = poisson)
#' ggemmeansplot(m1, mtcars, pred = "hp", moderator = "vs")
#'
#' m2 <- glm(y ~ hp * mpg, data = mtcars, family = poisson)
#' ggemmeansplot(m2, mtcars, pred = "hp", moderator = "mpg")
#' }
#' 
#' @import ggeffects emmeans dplyr ggplot2
#' @export
ggemmeansplot <- function(model,pred,moderator,se.on=TRUE, y.label = "Prediction"){
  
  # Believe that ggeffects auomatically on the response scales so major changes
  data <-model.frame(model)
  pred_type <- if (is.numeric(data[[pred]])) "cont" else "factor"
  mod_type  <- if (is.numeric(data[[moderator]])) "cont" else "factor"

  
  if (pred_type == "cont" && mod_type == "cont"){
    
    "####################################"
    "# Effects of Interest"
    "####################################"
    
    m.mod <- mean(data[[moderator]], na.rm = TRUE)
    s.mod <- sd(data[[moderator]], na.rm = TRUE)
    meffectsfor <- c(pred, paste(moderator, "[",round(m.mod-s.mod,2), ",", round(m.mod+s.mod,2),"]", sep=""))
    mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "confidence") # checkbox
    ggdat <- data.frame(mod.emmeans) %>%
      mutate(group = case_when(group == round(m.mod - s.mod, 2) ~ paste("Low (Mean - 1SD = ", round(m.mod - s.mod, 2), ")", sep = ""),
                               TRUE                             ~ paste("High (Mean + 1SD = ", round(m.mod + s.mod, 2), ")", sep = "")))
    
    if (!all(c("conf.low", "conf.high") %in% names(ggdat))) {
      warning("No confidence intervals returned by ggemmeans. Check model type.")
      return(NULL)
    }
    
    if (any(is.na(ggdat$predicted))) {
      warning("Some predicted values are NA. These rows will be removed from the plot so proceed interpretation with caution. This usually happens when the model cannot estimate predictions for certain combinations of variables.")
      ggdat <- ggdat %>% filter(!is.na(predicted))
    }
    
    "####################################"
    "# Plot"
    "####################################"
    p <- ggplot(data = ggdat, aes(x = x, y = predicted, color = group, fill = group)) +
      geom_line() +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                  linetype="dotted",
                  alpha = ifelse(se.on, 0.4, 0)) +
      xlab(pred)+
      ylab(y.label)+
      scale_color_brewer(moderator, palette = "Pastel1")+
      scale_fill_brewer(moderator, palette = "Pastel1")+
      theme_bw()
    
    return(p)
    
  }
  
  else if(pred_type == "factor" && mod_type == "factor"){
    
    mod.emmeans<-ggemmeans(model = model, terms = c(pred, moderator) , interval = "confidence") # checkbox
    ggdat <- data.frame(mod.emmeans)
    

    if (!all(c("conf.low", "conf.high") %in% names(ggdat))) {
      warning("No confidence intervals returned by ggemmeans. Check model type.")
      return(NULL)
    }
    
    if (any(is.na(ggdat$predicted))) {
      warning("Some predicted values are NA. These rows will be removed from the plot so proceed interpretation with caution. This usually happens when the model cannot estimate predictions for certain combinations of variables.")
      ggdat <- ggdat %>% filter(!is.na(predicted))
    }
    
    p <- ggplot(data=ggdat, aes(x=x, y=predicted, color=group))+
      geom_point(position = position_dodge(.25))+
      geom_errorbar(aes(ymin=conf.low, ymax=conf.high), position = position_dodge(.25), width=0.1)+
      xlab(pred)+
      ylab(y.label)+
      scale_color_brewer(moderator, palette = "Pastel1")+
      scale_fill_brewer(moderator, palette = "Pastel1")+
      theme_bw()
    
    return(p)
    
    
    
  }else{#One of Each
    if(mod_type=="factor"){
      
      mod.emmeans<-ggemmeans(model = model, terms = c(pred,moderator) , interval = "confidence") # checkbox
      
      "####################################"
      "# Plot"
      "####################################"
      ggdat <- data.frame(mod.emmeans)
      
      if (any(is.na(ggdat$predicted))) {
        warning("Some predicted values are NA. These rows will be removed from the plot so proceed interpretation with caution. This usually happens when the model cannot estimate predictions for certain combinations of variables.")
        ggdat <- ggdat %>% filter(!is.na(predicted))
      }
      

      if (!all(c("conf.low", "conf.high") %in% names(ggdat))) {
        warning("No confidence intervals returned by ggemmeans. Check model type.")
        return(NULL)
      }
      
      p <- ggplot(data = ggdat, aes(x = x, y = predicted, color = group, fill = group)) +
        geom_line() +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                    linetype="dotted",
                    alpha = ifelse(se.on, 0.4, 0)) +
        xlab(pred)+
        ylab(y.label)+
        scale_color_brewer(moderator, palette = "Pastel1")+
        scale_fill_brewer(moderator, palette = "Pastel1")+
        theme_bw()
      
      return(p)
      
      
    }else{
      m.mod <- mean(data[[moderator]], na.rm = TRUE)
      s.mod <- sd(data[[moderator]], na.rm = TRUE)
      meffectsfor <- c(pred, paste(moderator, "[",round(m.mod-s.mod,2), ",", round(m.mod+s.mod,2),"]", sep=""))
      mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "confidence") # checkbox
      
      
      mod.emmeans <- mod.emmeans %>% 
        mutate(group=ifelse(group == round(m.mod-s.mod,2), paste("Low (Mean - 1SD = ", round(m.mod-s.mod,2), ")", sep=""),
                            paste("High (Mean + 1SD = ", round(m.mod+s.mod,2), ")", sep="")))
      

      
      "####################################"
      "# Plot"
      "####################################"
      ggdat <- data.frame(mod.emmeans)
      

      if (!all(c("conf.low", "conf.high") %in% names(ggdat))) {
        warning("No confidence intervals returned by ggemmeans. Check model type.")
        return(NULL)
      }
      
      if (any(is.na(ggdat$predicted))) {
        warning("Some predicted values are NA. These rows will be removed from the plot so proceed interpretation with caution. This usually happens when the model cannot estimate predictions for certain combinations of variables.")
        ggdat <- ggdat %>% filter(!is.na(predicted))
      }
      
       p <- ggplot(data = ggdat, aes(x = x, y = predicted, color = group, fill = group)) +
        geom_line() +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                    linetype="dotted",
                    alpha = ifelse(se.on, 0.4, 0)) +
        xlab(pred)+
        ylab(y.label)+
        scale_color_brewer(moderator, palette = "Pastel1")+
        scale_fill_brewer(moderator, palette = "Pastel1")+
        theme_bw()
       
       return(p)
      
    }
  }
  
  return(p)
  
}


