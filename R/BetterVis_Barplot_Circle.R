#' Circular Barplot Visualization Function
#'
#' Creates circular bar plots with extensive customization options. The function allows users to adjust the appearance of bars, points, and labels, as well as control legend display and aesthetics.
#'
#' @name BetterVis_Barplot_Circle
#' @description Generates a circular bar plot to visualize data distributions, offering customization of color, size, shape, and text for enhanced visual clarity and insight.
#'
#' @param data A data frame containing the variables for plotting.
#' @param x_var A string specifying the variable for the x-axis, which should be the grouping factor.
#' @param y_var A string specifying the numeric variable for the y-axis.
#' @param fill_var A string specifying the variable used for fill colors, which should be a factor variable.
#' @param label_var An optional string for the column that contains labels to annotate each bar.
#' @param barplot_color A vector specifying the colors corresponding to each level of the fill_var factor.
#' @param y_continuous_limit A numeric vector defining the limits for the y-axis scale, e.g., \code{c(-5, 6)}.
#' @param point_shape Numeric, defines the shape of the points on top of bars.
#' @param point_alpha Numeric, sets the transparency level for the points.
#' @param text_adjust Numeric, adjusts the distance of the labels from the bars.
#' @param text_size Numeric, defines the size of the bar labels.
#' @param text_alpha Numeric, sets the transparency level for the bar labels.
#' @param text_bold Logical, specifies whether bar labels should be bold (default: \code{TRUE}).
#' @param legend_bold Logical, specifies if the legend text should be bold (default: \code{TRUE}).
#' @param legend_size Numeric, specifies the size of the legend text.
#' @param legend_title_size Numeric, specifies the font size of the legend title.
#' @param legend_title_bold Logical, specifies if the legend title should be bold (default: \code{TRUE}).
#' @param size_legend Logical, determines if the legend for size should be shown (default: \code{FALSE}).
#' @param fill_legend Logical, determines if the legend for fill should be shown (default: \code{TRUE}).
#'
#' @return A \code{ggplot} object representing the circular bar plot visualization.
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 geom_point scale_fill_manual labs theme element_blank element_text margin guides
#' @importFrom dplyr mutate
#' @importFrom ggtext element_markdown
#' @examples

#' library(ggplot2)
#' library(dplyr)
#' library(ggtext)
#'
#'
#' ##   Data Input
#' data("BetterVis_Barplot_Circle_example", package ="BetterVis")
#' barplot_color = c("#ABDDDE", "#FAD510", "#C6CDF7")
#'
#' BetterVis_Barplot_Circle(data = BetterVis_Barplot_Circle_example,x_var = "id",y_var="med_LFC",
#' fill_var = "type",label_var = "geneSymbol",barplot_color = barplot_color,y_continuous_limit = c(-3, 3))
#'
#'
#' ##  Advanced Example
#' BetterVis_Barplot_Circle(data = BetterVis_Barplot_Circle_example, x_var = "id", y_var = "med_LFC", fill_var = "type",label_var = "geneSymbol",
#' barplot_color = barplot_color,
#' y_continuous_limit = c(-6, 3),
#' point_shape = 19,point_alpha = 0.7,
#' text_adjust = 0.5, text_size = 4,text_alpha = 0.9, text_bold = TRUE,
#' size_legend = FALSE, fill_legend = TRUE,
#' legend_bold = TRUE,legend_size = 10,legend_title_size = 12,legend_title_bold = TRUE)
#' #+theme(legend.position = "bottom") + guides(size = guide_legend(title = "Size Legend"))
#' @export
#'
BetterVis_Barplot_Circle <- function(data, x_var, y_var, fill_var, label_var = NULL, barplot_color,
                                     y_continuous_limit = c(-5, 6), point_shape = 19,
                                     point_alpha = 0.7, text_adjust = 0.4, text_size = 3,
                                     text_alpha = 1, text_bold = TRUE, legend_bold = TRUE,
                                     legend_size = 8, legend_title_size = 10, legend_title_bold = TRUE,
                                     size_legend = FALSE, fill_legend = TRUE) {


  if (!is.null(label_var)) {
    adjust_label_angles <- function(df, id_column) {
      nBar <- nrow(df)
      angle <- 90 - 360 * (df[[id_column]] - 0.5) / nBar
      df$hjust <- as.numeric(angle < -90)
      df$angle <- (angle + 180) * (angle < -90) + angle * (angle >= -90)
      return(df)
    }
    label_data <- adjust_label_angles(data, x_var)
  }

  p <- ggplot(data) +
    geom_bar(aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]]),
             stat = "identity", position = "dodge",
             show.legend = fill_legend, alpha = 0.9, linewidth = 0.5) +
    coord_polar() +
    scale_y_continuous(limits = y_continuous_limit, expand = c(0, 0)) +
    geom_point(aes(x = .data[[x_var]], y = .data[[y_var]], size = .data[[y_var]]),
               shape = point_shape, alpha = point_alpha, show.legend = size_legend) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual(labels = paste("<span style='color:", barplot_color,
                                     "'>", unique(data[[fill_var]]),
                                     "</span>"), values = barplot_color,
                      name = "comparison")

  if (!is.null(label_var)) {
    p <- p +
      geom_text(data = label_data,
                aes(x = .data[[x_var]], y = .data[[y_var]] + text_adjust,
                    label = .data[[label_var]], hjust = hjust),
                fontface = ifelse(text_bold, "bold", "plain"), alpha = text_alpha, size = text_size,
                angle = label_data$angle, inherit.aes = FALSE)
  }

  p <- p +
    labs(x = NULL, y = NULL) +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.text = element_blank(),
      legend.text = element_markdown(size = legend_size, face = ifelse(legend_bold, "bold", "plain")),
      legend.title = element_text(size = legend_title_size, face = ifelse(legend_title_bold, "bold", "plain"),
                                  vjust = 0.5, hjust = 0.5),
      legend.position = c(0.5, 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )

  if (!size_legend) {
    p <- p + guides(size = "none")
  }
  if (!fill_legend) {
    p <- p + guides(fill = "none")
  }

  return(p)
}
