library(dplyr)

#' Count Response Summaries
#' 
#' 
#' @description 
#' Computes basic summary statistics of the count response varaible (5-number summary, variance, variance/ratio,
#' and zero proportion), optionally grouped by a variable.
#'
#' Note that count data are technically not continuous summaries need to be interpreted carefully Specifically: The mean of count data can be a non-integer,
#' even though actual data values are always integers. For variance in count data it's important to compare it to the mean and consider natural dispersion
#' For median and quantiles it is natural to see repeated or integer jumps in values
#' 
#'
#'
#'
#' @param data A data frame
#' @param response  A string corresponding to the name of the response variable
#' @param group group Optional string corresponding to grouping variable (default NA)
#' @param na.rm removing NA when calculating summaries takes on (TRUE/FALSE)
#'
#' @returns A tibble of summary statistics
#' @import dplyr
#' @export
#'
#' @examples
#' data(mpg, package = "ggplot2")
#' 
#' # Overall summary for the count response
#' summary_ct(mpg, "cyl")
#' 
#' # Grouped summary for the count response
#' summary_ct(mpg, "cyl", group = "class")
#' 
#' 
summary_ct <- function(data, response, group = NA, na.rm = TRUE){
  
  # ---- no grouped----
  if (is.na(group)) {
    
    summary_data <- tibble(
      Min. = min(data[[response]], na.rm = na.rm),
      "1st Qu." = unname(quantile(data[[response]], 0.25, na.rm = na.rm)),
      Median = median(data[[response]], na.rm = na.rm),
      Mean = mean(data[[response]], na.rm = na.rm),
      "3rd Qu." = unname(quantile(data[[response]], 0.75, na.rm = na.rm)),
      Max. = max(data[[response]], na.rm = na.rm),
      zero_prop = mean(data[[response]] == 0, na.rm = na.rm),
      variance = var(data[[response]], na.rm = na.rm),
      Var_to_Mean = mean(data[[response]], na.rm = na.rm) /
        var(data[[response]], na.rm = na.rm) # This is less meaningful Directly unless all x equal but somewhat useful
    )
    
    return(summary_data)
  }
  
  # ---- grouped case ----
  else {
    
    # why we use .data here
    # follow https://dplyr.tidyverse.org/articles/programming.html#:~:text=Data%2D%20and%20env%2Dvariables,are%20created%20manipulating%20existing%20variables
    # Specifically how tidy verse use .data to aboid Ambiguity between env-variables and columns in a data
    summary_data <- data %>%
      group_by(.data[[group]]) %>%
      summarise(
        Min. = min(.data[[response]], na.rm = na.rm),
        `1st Qu.` = unname(quantile(.data[[response]], 0.25, na.rm = na.rm)),
        Median = median(.data[[response]], na.rm = na.rm),
        Mean = mean(.data[[response]], na.rm = na.rm),
        `3rd Qu.` = unname(quantile(.data[[response]], 0.75, na.rm = na.rm)),
        Max. = max(.data[[response]], na.rm = na.rm),
        zero_prop = mean(.data[[response]] == 0, na.rm = na.rm),
        variance = var(.data[[response]], na.rm = na.rm),
        Var_to_Mean = mean(.data[[response]], na.rm = na.rm) /
          var(.data[[response]], na.rm = na.rm),
        .groups = "drop"
      ) %>%
      rename(Group = 1)
    
    return(summary_data)
  }
}

# --------Some Cases When developing----------
#
# data(mpg, package = "ggplot2")
# 
# 
# summary_ct(mpg, "class")
# summary_ct(mpg, "cyl", group = "class")
# 
# 






