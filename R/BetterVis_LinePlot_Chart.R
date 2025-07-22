#' Create a Summary Line Plot with Confidence Intervals
#'
#' This function generates a line plot that summarizes a continuous variable
#' over another continuous variable (e.g., time) for different groups. It displays
#' mean trends with error bars (standard error) and optional confidence interval ribbons.
#'
#' @name BetterVis_LinePlot_Chart
#'
#' @param df A data frame containing the data for plotting.
#' @param x_var A character string for the column name of the continuous x-axis variable (e.g., "Day").
#' @param y_var A character string for the column name of the continuous y-axis variable to be summarized.
#' @param fill_var A character string for the column name of the categorical variable used for grouping and coloring.
#' @param colors A vector of colors for the different levels of `fill_var`.
#' @param confidence Logical. If `TRUE`, a shaded confidence interval ribbon is added. Default is `TRUE`.
#' @param x_title,y_title Character strings for plot axis titles.
#' @param legend_title Character string for the legend title.
#' @param legend_position Character string specifying the legend position (e.g., "top", "right").
#' @param facet Logical. If `TRUE`, the plot is faceted by `facet_var`. Default is `FALSE`.
#' @param facet_var (Optional) A character string for the column name of the variable to facet by.
#' @param facet_background_color Character string. The background color of the facet labels.
#' @param facet_background_border Logical. If `TRUE`, adds a border to the facet labels.
#' @param size_global_adjust A single numeric value to uniformly scale text elements. Default is `1`.
#'
#' @return A `ggplot` object representing the line plot.
#'
#' @importFrom ggplot2 ggplot aes_string stat_summary ylab xlab coord_cartesian scale_fill_manual scale_color_manual scale_x_continuous scale_y_continuous mean_cl_normal theme_classic theme margin element_rect element_text element_line unit facet_wrap
#' @importFrom stats as.formula
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("BetterVis_LinePlot_Chart_example", package = "BetterVis")
#'
#' # Define a color palette
#' example_colors <- c("#4489C8", "#ED7E7A", "#008F91", "#FFCD44")
#'
#' # Example 1: Basic line plot with confidence intervals
#' BetterVis_LinePlot_Chart(
#'   df = BetterVis_LinePlot_Chart_example,
#'   x_var = "Day",
#'   y_var = "value",
#'   fill_var = "Treatment",
#'   colors = example_colors,
#'   confidence = TRUE,
#'   x_title = "Day", y_title = "Value",
#'   legend_title = "", legend_position = "top"
#' )
#'
#' # Example 2: Faceted line plot
#' BetterVis_LinePlot_Chart(
#'   df = BetterVis_LinePlot_Chart_example,
#'   x_var = "Day",
#'   y_var = "value",
#'   fill_var = "Treatment",
#'   colors = example_colors,
#'   x_title = "Day", y_title = "Value",
#'   facet = TRUE, facet_var = "Group",
#'   facet_background_color = "#E5E5E5"
#' )
BetterVis_LinePlot_Chart <- function(df, x_var, y_var, fill_var, colors,
                                     confidence = TRUE,
                                     x_title = NULL,
                                     y_title = "Weight change (%)",
                                     legend_title = "",
                                     legend_position = "top",
                                     facet_var = NULL,
                                     facet = FALSE,
                                     facet_background_color = "grey",
                                     facet_background_border = FALSE,
                                     size_global_adjust = 1) {

  p <- ggplot(df, aes_string(x = x_var, y = y_var, color = fill_var, group = fill_var))

  if (confidence) {
    p <- p + stat_summary(geom = "ribbon",
                          fun.data = "mean_cl_normal",
                          aes_string(fill = fill_var),
                          alpha = 0.2,
                          color = NA) # Set color to NA to avoid ribbon border
  }

  p <- p +
    stat_summary(geom = "line", fun = "mean", linewidth = 1.5) +
    stat_summary(geom = "errorbar",
                 fun.data = "mean_se",
                 width = 1.2, linewidth = 0.8, color = "black") +
    stat_summary(geom = "point", fun = "mean", aes_string(fill = fill_var),
                 size = 4, shape = 21, stroke = 1.2, color = 'black') +
    ylab(y_title) +
    xlab(x_title) +
    coord_cartesian(clip = 'off', expand = FALSE) +
    scale_fill_manual(name = legend_title, values = colors) +
    scale_color_manual(name = legend_title, values = colors) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 40), breaks = c(0, 7, 14, 21, 28, 35)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 60), breaks = seq(0, 60, by = 20)) +
    theme_classic(base_size = 18 * size_global_adjust) +
    theme(legend.position = legend_position,
          legend.margin = margin(0, 0, 0, 0),
          legend.key.width = unit(0.6, "cm"),
          legend.key.height = unit(0.1, "cm"),
          legend.key = element_rect(colour = "black"),
          axis.text = element_text(size = 18 * size_global_adjust, face = "plain", color = "black"),
          axis.title = element_text(size = 18 * size_global_adjust, face = "plain", hjust = 0.525),
          axis.line = element_line(linewidth = 1.2),
          axis.ticks = element_line(linewidth = 0.8, color = 'black'),
          strip.background = element_rect(fill = facet_background_color,
                                          color = if (facet_background_border) "black" else NA),
          strip.text = element_text(color = "black", face = "bold", size = 16 * size_global_adjust))

  if (facet && !is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }

  return(p)
}
