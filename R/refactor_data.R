refactor_data <- function(data, factors) {
  
  if (length(factors) == 0) return(data)
  
  data %>%
    dplyr::mutate(dplyr::across(all_of(factors), as.factor))
  
  return(data)
}
