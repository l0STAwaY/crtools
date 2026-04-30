build_interaction_emtrends_tab <- function(model, pred, moderator){

  
message("emtrends plot is on the log(response/offset) scale")
  data <- if (!is.null(model$org_data)) model$org_data else model.frame(model)
# ---------------- variable types ----------------
pred_type <- if (is.numeric(data[[pred]])) "cont" else "factor"
mod_type  <- if (is.numeric(data[[moderator]])) "cont" else "factor"
if(pred_type=="cont" && mod_type =="cont"){ #Both Continuous
  "####################################"
  "# Calculate Marginal Effects"
  "####################################"
  m<-mean(data[[moderator]], na.rm=T)
  s<-sd(data[[moderator]], na.rm=T)
  modvarat<-list(c(round(m-s,2), round(m+s,2)))
  names(modvarat)<-c(moderator)
  
  mod.emtrends<-data.frame(emtrends(object = model, spec=moderator, var=pred, at=modvarat,data=data))
  mod.emtrends[,moderator]<- ifelse(mod.emtrends[,moderator]==round(m-s,2),"Low (Mean - 1SD)", "High (Mean + 1SD)")
  
  "####################################"
  "# Clean Up Labels for Printing"
  "####################################"
  colnames(mod.emtrends)[-(1)]<-c(paste("Slope of", pred), "SE", "df", "Lower CI", "Upper CI")
  emtrend.test <-test(emtrends(object = model, spec=moderator, var=pred, at=modvarat,data=data))
  mod.emtrends<-mod.emtrends %>% mutate("p-value"= emtrend.test$p.value,
                                        "z ratio"= emtrend.test$z.ratio) %>%
    relocate(c(`z ratio`, `p-value`), .after=df)

  
}else if(pred_type=="factor" && mod_type =="factor"){ #Both Factors
  # HIDE emtrends_tab, emtrends_int
}else{
  if(mod_type =="factor"){

      "####################################"
      "# Calculate Marginal Effects"
      "####################################"
      mod.emtrends<-data.frame(emtrends(model, specs = moderator, var = pred,data=data))
      
      "####################################"
      "# Clean Up Labels for Printing"
      "####################################"
      colnames(mod.emtrends)[-(1)]<-c(paste("Slope of", pred), "SE", "df", "Lower CI", "Upper CI")
      emtrend.test <-test(emtrends(model, specs = moderator, var = pred,data=data))
      mod.emtrends<-mod.emtrends %>% mutate("p-value"= emtrend.test$p.value,
                                            "z ratio"= emtrend.test$z.ratio)%>%
        relocate(c(`z ratio`, `p-value`), .after=df)
  }else{
    # HIDE emtrends_tab, emtrends_int
  }
}


}

