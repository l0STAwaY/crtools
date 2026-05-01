library(dplyr)
#' Title
#'
#' @param data A data frame
#' @param na.rm removing NA when calculating summaries takes on (TRUE/FALSE)
#' This function computes a comprehensive set of summary statistics for all numeric
#' variables in a data frame. It is designed for quick exploratory data analysis and
#' provides both central tendency and dispersion measures, as well as missing data
#' diagnostics.
#'
#' The function extracts all numeric columns and returns a tibble containing:
#' \itemize{
#'   \item Number of rows in the dataset
#'   \item Number of missing values per variable
#'   \item Minimum value
#'   \item First quartile (25%)
#'   \item Median
#'   \item Mean
#'   \item Third quartile (75%)
#'   \item Maximum value
#'   \item Variance
#' }
#' 
#' 
#' When ggpair == True it also displays a ggpair plot
#' 
#' @returns A tibble of summary statistics
#' @export
#'
#' @examples
#' data(mpg, package = "ggplot2")
#' summary_overall(mpg)
#' 
#' 
summary_overall <- function(data, na.rm = TRUE,ggpair=TRUE) {
  # return a vector get all numeric data and summarize them
  # these are continous summaries
  # summary table of all the statistics
  # Five Number Summary
  # Missing Values
  # Number of rows
  num_data <- data[sapply(data, is.numeric)]
  if (ncol(num_data) == 0) {
    stop("No numeric columns found")
  }
  
  if(ggpair==TRUE){
    print(make_ggpairs_plot(data))
  }
  
  
  summary_data <- tibble(
    variable = names(num_data),
    rows = nrow(data),
    missing_values = unname(sapply(num_data, function(x) sum(is.na(x)))),
    Min = unname(sapply(num_data, min, na.rm = na.rm)),
    `1st Qu.` = unname(sapply(num_data, function(x) unname(quantile(x, 0.25, na.rm = na.rm)))),
    Median = unname(sapply(num_data, median, na.rm = na.rm)),
    Mean = unname(sapply(num_data, mean, na.rm = na.rm)),
    `3rd Qu.` = unname(sapply(num_data, function(x) unname(quantile(x, 0.75, na.rm = na.rm)))),
    Max = unname(sapply(num_data, max, na.rm = na.rm)),
    variance = unname(sapply(num_data, var, na.rm = na.rm))
  )
  
  return(summary_data)
}
