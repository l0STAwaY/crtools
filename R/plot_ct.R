library(ggplot2)

#' Count Response Plotting
#'
#' For plots since count is non-continuous the bar plot makes the most sense 
#' However, for Box plot and violin plots For count data (which is discrete), the density estimate can be misleading or look odd, 
#' it will be more natural if we have a greater range of different discrete values and odd if we have  few unique values.
#' For the violin plot especially, the smooth kernel density plot  may suggest that the count is continous when it is not, hence we 
#' added the jitter points for both violin and boxplot
#' 
#' 
#' 
#' @param data A data frame
#' @param response A string corresponding to the name of the response variable
#' @param group Optional string corresponding to grouping variable (default NA)
#' @param type type of plot to select from are "boxplot","violin" and "barplot" 
#'
#' @returns returns a plot, specifically a ggplot2::ggplot object
#' @export
#'
#' @examples
#' 
#' data(mpg,package = "ggplot2")
#' 
#' # Count boxplot
#' p1 <- plot_ct(mpg, "cyl", type = "boxplot")
#' p1
#' 
#' # Count boxplot across group
#' p2 <- plot_ct(mpg, "cyl", group = "class", type = "boxplot")
#' p2
#' 
#' 
#' # Count violin plot
#' p3 <- plot_ct(mpg, "cyl", type = "violin")
#' p3
#' 
#' 
#' # Count violin plot across group
#' p4 <- plot_ct(mpg, "cyl", group = "class", type = "violin")
#' p4
#' 
#' 
#'  # Count bar plot
#' p5 <- plot_ct(mpg, "cyl", type = "barplot")
#' p5
#' 
#' 
#' 
#' # Count bar plot across group
#' p6 <- plot_ct(mpg, "cyl", group = "class", type = "barplot")
#' p6
#' 
#' 
plot_ct <- function(data, response, group = NA, type = "barplot"){

  # ---------------- Optional Plots-----------------
  p <- NULL
  
  if (type != FALSE) {
    
    if (is.na(group)) {
      
      # recommend count 10 
      
      if (type == "boxplot") {
        # add jitter becuase technically 
        p <- ggplot(data, aes(y = .data[[response]])) +
          geom_boxplot() +
          geom_jitter(width = 0.15, alpha = 0.4)
        
      } else if (type == "violin") {
        
        # Setting x = "" or x = 1 forces R to treat the data as a single group. 
        # essentially it set every single observation to the same,
        # vilin accet grou and then the contious variable so we have onegorup. here eseentially
        p <- ggplot(data, aes(x = "", y = .data[[response]])) +
          geom_violin(fill = "skyblue", alpha = 0.6) +
          geom_jitter(width = 0.15, alpha = 0.4)
        
      } else if (type == "barplot") {
        
        # frequency bar plot (RAW COUNTS → geom_bar is correct)
        # This is essentally a count frequency
        p <- ggplot(data, aes(x = .data[[response]])) +
          geom_bar() +
          xlab(response) +
          ylab("Frequency")
      }
      
    } else {
      
      if (type == "boxplot") {
        
        p <- ggplot(data, aes(x = .data[[group]], y = .data[[response]])) +
          geom_boxplot() +
          geom_jitter(width = 0.15, alpha = 0.4)
        
      } else if (type == "violin") {
        
        p <- ggplot(data, aes(x = .data[[group]], y = .data[[response]])) +
          geom_violin(fill = "skyblue", alpha = 0.6) +
          geom_jitter(width = 0.15, alpha = 0.4)
        
      } else if (type == "barplot") {
        
        p <- ggplot(mpg, aes(x = .data[[group]], fill = as.factor(.data[[response]]))) +
          geom_bar(position = "dodge") +
          xlab(group) +
          ylab("Count")
      }
    }
  }
  
  return(p)
  
}


# --------Some Cases When developing----------#
# data(mpg,package = "ggplot2")
# p1 <- plot_ct(mpg, "cyl", type = "boxplot")
# p1
# class(p1)
# p2 <- plot_ct(mpg, "cyl", group = "class", type = "boxplot")
# p2
# 
# p3 <- plot_ct(mpg, "cyl", type = "violin")
# p3
# 
# p4 <- plot_ct(mpg, "cyl", group = "class", type = "violin")
# p4
# 
# p5 <- plot_ct(mpg, "cyl", type = "barplot")
# p5
# 
# p6 <- plot_ct(mpg, "cyl", group = "class", type = "barplot")
# p6


