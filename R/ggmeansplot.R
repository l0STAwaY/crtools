library(ggeffects)
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
  data <- if (!is.null(model$org_data)) model$org_data else model.frame(model)
  pred_type <- if (is.numeric(data[[pred]])) "cont" else "factor"
  mod_type  <- if (is.numeric(data[[moderator]])) "cont" else "factor"
  
  


  
    if (pred_type == "cont") {
      pred_range <- seq(
        min(data[[pred]], na.rm = TRUE),
        max(data[[pred]], na.rm = TRUE),
        length.out = 25
      )
    }
      
    "####################################"
    "# Effects of Interest"
    "####################################"
    
if (pred_type == "cont" && mod_type == "cont") {
      
    m.mod <- mean(data[[moderator]], na.rm = TRUE)
    s.mod <- sd(data[[moderator]], na.rm = TRUE)
    
    # make one categoricla
    meffectsfor <- c(round(m.mod - s.mod, 2),round(m.mod + s.mod, 2))
    # mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "confidence",data=data) # checkbox
    
    mod.emmeans <- emmeans(
      object = model,
      specs = as.formula(paste0("~ ", pred, " * ", moderator)),
      at = setNames(list(pred_range, meffectsfor), c(pred, moderator)),
      type = "response",
      interval = "confidence",
      data = data
    )
    
    ggdat <- as.data.frame(mod.emmeans) %>%
      rename(
        conf.low = asymp.LCL, 
        conf.high = asymp.UCL
      ) %>%
      mutate(group = .data[[moderator]],x= .data[[pred]]) %>%
      mutate(group = case_when(group == round(m.mod - s.mod, 2) ~ paste("Low (Mean - 1SD = ", round(m.mod - s.mod, 2), ")", sep = ""),
                               TRUE                             ~ paste("High (Mean + 1SD = ", round(m.mod + s.mod, 2), ")", sep = "")))
     
    
    
    if ("response" %in% names(ggdat)) {
      ggdat <- ggdat %>% rename(predicted = response)
    } else if ("rate" %in% names(ggdat)) {
      ggdat <- ggdat %>% rename(predicted = rate)
    } else if ("emmean" %in% names(ggdat)) {
      ggdat <- ggdat %>% rename(predicted = emmean)
    }
    
    
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
    
    # every point for both
    mod.emmeans <- emmeans(
    object = model,
    specs = as.formula(paste0("~ ", pred, " * ", moderator)),
    type = "response",
    interval = "confidence",
    data = data
    )
    ggdat <- as.data.frame(mod.emmeans) %>%
    rename(
       conf.low = asymp.LCL,
       conf.high = asymp.UCL
    ) %>%
      mutate(group = .data[[moderator]],x= .data[[pred]])
    
    
    if ("response" %in% names(ggdat)) {
      ggdat <- ggdat %>% rename(predicted = response)
    } else if ("rate" %in% names(ggdat)) {
      ggdat <- ggdat %>% rename(predicted = rate)
    } else if ("emmean" %in% names(ggdat)) {
      ggdat <- ggdat %>% rename(predicted = emmean)
    }
    

  

  

    if (!all(c("conf.low", "conf.high") %in% names(ggdat))) {
      warning("No confidence intervals returned by ggemmeans. Check model type.")
      return(NULL)
    }
    
    if (any(is.na(ggdat$predicted))) {
      warning(
        "Some predicted values are NA and were removed from the plot. This usually occurs when the model cannot estimate predictions 
        for certain combinations of the predictor and moderator 
        (e.g., values outside the observed range or unsupported factor combinations). Please check your data or evaluation points."
      )      
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
    
    
    
  }else{  #One of Each
    if(mod_type=="factor"){
      
      # at all factor level and for all defined contious
      mod.emmeans <- emmeans(
        object = model,
        specs = as.formula(paste0("~ ", pred, " * ", moderator)),
        at = setNames(list(pred_range), pred),
        type = "response",
        interval = "confidence",
        data = data
      )

      
      # mod.emmeans<- ggemmeans(model = model, terms = c(pred,moderator) , interval = "confidence",data=data) # checkbox
      
      "####################################"
      "# Plot"
      "####################################"
      ggdat <- as.data.frame(mod.emmeans) %>%
        rename(
          conf.low = asymp.LCL, 
          conf.high = asymp.UCL
        ) %>%
        mutate(group = .data[[moderator]], x= .data[[pred]])
      
      
        if ("response" %in% names(ggdat)) {
          ggdat <- ggdat %>% rename(predicted = response)
        } else if ("rate" %in% names(ggdat)) {
          ggdat <- ggdat %>% rename(predicted = rate)
        } else if ("emmean" %in% names(ggdat)) {
          ggdat <- ggdat %>% rename(predicted = emmean)
        }
      
        
      if (any(is.na(ggdat$predicted))) {
        warning(
          "Some predicted values are NA and were removed from the plot. This usually occurs when the model cannot estimate predictions 
        for certain combinations of the predictor and moderator 
        (e.g., values outside the observed range or unsupported factor combinations). Please check your data or evaluation points."
        )         
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
      
      
    }else{ # if moderator is the contnious
      m.mod <- mean(data[[moderator]], na.rm = TRUE)
      s.mod <- sd(data[[moderator]], na.rm = TRUE)
      meffectsfor <- c(round(m.mod - s.mod, 2),round(m.mod + s.mod, 2))

      # mod.emmeans<-ggemmeans(model = model, terms = meffectsfor , interval = "confidence",data=data) # checkbox
      
      mod.emmeans <- emmeans(
        object = model,
        specs = as.formula(paste0("~ ", pred, " * ", moderator)),
        at = setNames(list(meffectsfor), moderator),
        type = "response", 
        data = data
      )
 
      
      


      
      "####################################"
      "# Plot"
      "####################################"
      ggdat <- as.data.frame(mod.emmeans) %>%
        rename(
          conf.low = asymp.LCL, 
          conf.high = asymp.UCL
        ) %>%
        mutate(group = .data[[moderator]],x= .data[[pred]] )%>% 
        mutate(group=ifelse(group == round(m.mod-s.mod,2), paste("Low (Mean - 1SD = ", round(m.mod-s.mod,2), ")", sep=""),
                            paste("High (Mean + 1SD = ", round(m.mod+s.mod,2), ")", sep="")))
      
        if ("response" %in% names(ggdat)) {
          ggdat <- ggdat %>% rename(predicted = response)
        } else if ("rate" %in% names(ggdat)) {
          ggdat <- ggdat %>% rename(predicted = rate)
        } else if ("emmean" %in% names(ggdat)) {
          ggdat <- ggdat %>% rename(predicted = emmean)
        }
      

      if (!all(c("conf.low", "conf.high") %in% names(ggdat))) {
        warning("No confidence intervals returned by ggemmeans. Check model type.")
        return(NULL)
      }
      
      if (any(is.na(ggdat$predicted))) {
        warning(
          "Some predicted values are NA and were removed from the plot. This usually occurs when the model cannot estimate predictions 
        for certain combinations of the predictor and moderator 
        (e.g., values outside the observed range or unsupported factor combinations). Please check your data or evaluation points."
        )          
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


