make_ggpairs_plot <- function(data){
  
  upper <- list(continuous = "cor", discrete = "count", combo = "box_no_facet")
  lower <- list(continuous = "points", discrete = "barDiag", combo = "box_no_facet")
  
  p <- GGally::ggpairs(
    data,
    progress = FALSE,
    upper = upper,
    lower = lower
  )
  p +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)
    )
  
  
}
