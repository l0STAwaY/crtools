correlation_summary <- function(data) {
  
  modmatrix <- data %>% select_if(is.numeric)
  
  cormats<-Hmisc::rcorr(as.matrix(modmatrix), type = "pearson")
  cormat<-round(cormats$r,4)
  pmat<-as.matrix(data.frame(stars.pval(cormats$P)))
  
  # correlation matrix
  cormat<-matrix(paste(cormat, pmat, sep=""),nrow(cormats$r),ncol(cormats$r))
  rownames(cormat)<-colnames(cormats$r)
  colnames(cormat)<-rownames(cormats$r)
  return(data.frame(cormat))
  
}