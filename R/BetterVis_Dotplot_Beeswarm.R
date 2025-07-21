#' Create a Beeswarm Dot Plot
#'
#' This function generates a beeswarm plot using the `ggbeeswarm` package, which is
#' excellent for visualizing the distribution of a numeric variable across different
#' categories. It includes options for adding median bars and statistical comparisons.
#'
#' @name BetterVis_Dotplot_Beeswarm
#'
#' @param data A data frame containing the plotting data.
#' @param x_var A character string for the column name of the categorical x-axis variable.
#' @param y_var A character string for the column name of the numeric y-axis variable.
#' @param color A vector of colors for the points, corresponding to the levels in `x_var`.
#' @param ylim A numeric vector of length 2, setting the y-axis limits. Default is `c(0, 1)`.
#' @param y_break Numeric. The interval for y-axis tick marks. Default is `0.1`.
#' @param point_size Numeric. The size of the points. Default is `2`.
#' @param point_alpha Numeric. The transparency of the points (0-1). Default is `0.6`.
#' @param median_bar Logical. If `TRUE`, adds a median bar with interquartile range to each group. Default is `TRUE`.
#' @param border Logical. If `TRUE`, adds a border around the plot panel. Default is `TRUE`.
#' @param title,x_title,y_title Character strings for plot titles. Default is `NULL`.
#' @param sig_symbol Logical. If `TRUE`, enables statistical comparison annotations. Default is `TRUE`.
#' @param sig_type Character. The type of significance label: `"SYMBOL"` for asterisks or `"NUMBER"` for p-values. Default is `"SYMBOL"`.
#' @param comparison_col A character string for the column name to be used in statistical comparisons (usually the same as `x_var`).
#' @param comparison_group A list of character vectors specifying the groups to compare. Each vector within the list should contain two group names, e.g., `list(c("GroupA", "GroupB"), c("GroupB", "GroupC"))`.
#' @param comparison_method The statistical method for comparison, e.g., `"wilcox.test"` or `"t.test"`. Default is `"wilcox.test"`.
#'
#' @return A `ggplot` object representing the beeswarm plot.
#'
#' @importFrom ggplot2 ggplot aes_string scale_color_manual scale_y_continuous theme_classic labs theme element_text element_rect stat_summary
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom ggpubr stat_compare_means
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("BetterVis_Dotplot_Beeswarm_example", package = "BetterVis")
#'
#' # Example 1: Using the provided dataset with t-test comparisons
#' BetterVis_Dotplot_Beeswarm(
#'   data = BetterVis_Dotplot_Beeswarm_example, x_var = "Type", y_var = "MPG.city",
#'   color = c("#68d0ce", "#f4a4c9", "#4955d0", "#e09a2a", "#de6a73", "firebrick"),
#'   ylim = c(10, 60), y_break = 10,
#'   title = "City MPG by Vehicle Type", x_title = "Type", y_title = "Miles Per Gallon (City)",
#'   sig_symbol = TRUE, sig_type = "SYMBOL",
#'   comparison_col = "Type",
#'   comparison_group = list(c("Compact", "Large"), c("Midsize", "Large"), c("Small", "Van")),
#'   comparison_method = "t.test"
#' )
#'
#' # Example 2: Using the iris dataset with Wilcoxon test and p-values
#' BetterVis_Dotplot_Beeswarm(
#'   data = iris, x_var = "Species", y_var = "Sepal.Width",
#'   color = c("#68d0ce", "#f4a4c9", "#4955d0"),
#'   ylim = c(1.5, 5), y_break = 1,
#'   title = "Sepal Width by Species", x_title = "Species", y_title = "Sepal Width",
#'   sig_symbol = TRUE, sig_type = "NUMBER",
#'   comparison_col = "Species",
#'   comparison_group = list(c("setosa", "versicolor"), c("versicolor", "virginica")),
#'   comparison_method = "wilcox.test"
#' )
#' # You can add more theme customizations like this:
#' # + theme(axis.text.x = element_text(size=18))
BetterVis_Dotplot_Beeswarm <- function(data, x_var, y_var,
                                       color = c("#68d0ce", "#f4a4c9", "#4955d0", "#e09a2a", "#de6a73"),
                                       ylim = c(0, 1),
                                       y_break = 0.1,
                                       point_size = 2,
                                       point_alpha = 0.6,
                                       median_bar = TRUE,
                                       title = NULL,
                                       x_title = NULL,
                                       y_title = NULL,
                                       sig_symbol = TRUE,
                                       comparison_col = NULL,
                                       comparison_group = NULL,
                                       comparison_method = "wilcox.test",
                                       sig_type = "SYMBOL",
                                       border = TRUE) {
  p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_beeswarm(aes_string(color = x_var),
                  size = point_size,
                  priority = "ascending",
                  cex = 1.5,
                  alpha = point_alpha,
                  stroke = 0.8) +
    scale_color_manual(values = color) +
    scale_y_continuous(limits = ylim, breaks = seq(ylim[1], ylim[2], y_break)) +
    theme_classic()

  if (!is.null(title) || !is.null(x_title) || !is.null(y_title)) {
    p <- p + labs(title = title, x = x_title, y = y_title)
  }

  if (median_bar) {
    p <- p +
      stat_summary(fun = median, geom = 'crossbar', width = 0.4, size = 0.2, color = 'black') +
      stat_summary(fun.data = function(x) Hmisc::median_hilow(x, 0.5),
                   geom = 'errorbar', width = 0.2, color = 'black')
  }

  if (sig_symbol && !is.null(comparison_col) && !is.null(comparison_group)) {
    # Ensure comparison_group is a list for ggpubr
    if (!is.list(comparison_group)) {
      if (length(comparison_group) %% 2 != 0)
        stop("If not a list, 'comparison_group' must have an even number of elements.")
      # Automatically convert a flat vector to a list of pairs
      comp_list <- split(comparison_group, ceiling(seq_along(comparison_group) / 2))
    } else {
      comp_list <- comparison_group
    }

    label_type <- ifelse(toupper(sig_type) == "NUMBER", "p.format", "p.signif")

    p <- p + stat_compare_means(comparisons = comp_list,
                                method = comparison_method,
                                label = label_type)
  }

  p <- p +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 16, face = "bold"))

  if (border) {
    p <- p + theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA))
  }

  return(p)
}
