#' Johnson-Neyman Plot for Interaction Effects
#'
#' Computes and visualizes the Johnson-Neyman significance region
#' for interactions involving a continuous predictor and moderator.
#' The modelis not implemented for ZeroInflation or effectivmodels because both don't work the interaction package
#'
#' @param model Fitted regression model (glm, lm, etc.)
#' @param pred Name of focal predictor (character)
#' @param moderator Name of moderator variable (character)
#' @param control.fdr Logical; adjust for multiple testing (default TRUE)s
#'
#' @return ggplot object
#'
#' @import ggplot2 interactions
#' @export
jnplot <- function(model,pred,moderator,control.fdr = TRUE) {
  
  data <- if (!is.null(model$org_data)) model$org_data else model.frame(model)
  pred_type <- if (is.numeric(data[[pred]])) "cont" else "factor"
  mod_type  <- if (is.numeric(data[[moderator]])) "cont" else "factor"
  fam <- get_ct_family(model)
  
  
  if (!(pred_type == "cont" && mod_type == "cont")) {
    stop("Johnson-Neyman requires BOTH predictor and moderator to be continuous.")
  }
  
  if(get_ct_family(model) %in% c("zinb","zip")){
    warning("Johnson-Neyman is not implemented for ZeroInflation Model Yet.")
    return(NULL)
  }
  
  
  if (fam %in% c("glmpoisson", "glmnb")) {
    warning("Johnson-Neyman is not recommended for glmmTMB models.")
    return(NULL)
  }
  
  
  # Input	Type	Meaning
  # "hp"	string	text label
  # sym("hp")	symbol	variable reference
  
  # very odd behaviour
  # h <- "hp"
  # interactions::johnson_neyman(model = m10,pred = h, modx = "mpg") gives error
  # but passing "hp" is fine
  # Use do.call() to pass variable names as strings to johnson_neyman().
  # This bypasses non-standard evaluation issues and allows programmatic use.
  
  

  jn<-do.call(interactions::johnson_neyman, list(model = model, pred = pred, modx = moderator))
  
 
  
  # If greyscale, this bit is needed
  # p$layers[[9]]  <- geom_vline(xintercept = jnplot$bounds[1], color="black", linetype="dashed")
  # p$layers[[10]] <- geom_vline(xintercept = jnplot$bounds[2], color="black", linetype="dashed")
  
  "####################################"
  "# Plot"
  "####################################"
  p<-jn$plot + 
    xlab(moderator)+
    ylab(paste("Slope of ", pred ,sep=""))+
    ggtitle("Johnson-Neyman Plot")+
    scale_color_brewer(paste("Slope of ",pred,sep=""), palette = "Pastel1")+
    scale_fill_brewer(paste("Slope of ", pred ,sep=""), palette = "Pastel1")+
    theme_bw()
  
  message("The Johnson Neyman Plot is evaluated on the Log(Count_Response) Scale Not Response Scale")
  
       # If greyscale, this bit is needed
        # p$layers[[9]]  <- geom_vline(xintercept = jnplot$bounds[1], color="black", linetype="dashed")
        # p$layers[[10]] <- geom_vline(xintercept = jnplot$bounds[2], color="black", linetype="dashed")
  
  return(p)
}







