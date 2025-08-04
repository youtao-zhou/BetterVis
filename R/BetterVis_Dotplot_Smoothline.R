#' Create a Scatter Plot with Smoothed Line and Correlation
#'
#' This function generates a highly customized scatter plot using `ggplot2`. It
#' includes options for adding a smoothed regression line, displaying correlation
#' test results, and utilizes themes and palettes from `ggprism` and `paletteer`.
#'
#' @name BetterVis_Dotplot_Smoothline
#'
#' @param data A data frame containing the plotting data.
#' @param x_var,y_var Character strings specifying the column names for the continuous x and y-axis variables.
#' @param classified_var A character string for the column name of the categorical variable used for point fill color.
#' @param color (Optional) A vector of colors for the different levels of `classified_var`. If `NULL`, a default `paletteer` color scale is used.
#' @param size_var (Optional) A character string for a numeric column to be mapped to point size.
#' @param point_size Numeric. The base or maximum size of the points. Default is `15`.
#' @param smoothline Logical. If `TRUE`, adds a smoothed conditional mean line (`loess`). Default is `TRUE`.
#' @param smoothline_width Numeric. The line thickness of the smoothed line. Default is `1`.
#' @param smoothline_color Character string. The color of the smoothed line. Default is `"#CE743C"`.
#' @param cor_test Logical. If `TRUE`, displays correlation statistics on the plot. Default is `TRUE`.
#' @param cor_method Character string. The correlation method to use ("pearson", "spearman", or "kendall"). Default is `"spearman"`.
#' @param cor_color Character string. The color for the correlation text and label box. Default is `"black"`.
#' @param cor_position A numeric vector of length 2, `c(x, y)`, specifying the coordinates for the correlation label.
#' @param title,x_title,y_title Character strings for plot titles.
#' @param legend Logical. If `TRUE`, the legend is displayed. Default is `TRUE`.
#' @param legend_title Character string for the legend title.
#' @param legend_position Character string specifying the legend position (e.g., "right", "bottom"). Default is `"right"`.
#' @param global_size_adjust A single numeric value to uniformly scale all text elements. Default is `1`.
#'
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes_string geom_point scale_size_continuous geom_smooth ggtitle labs theme element_text unit element_line margin scale_fill_manual
#' @importFrom ggpubr stat_cor
#' @importFrom ggprism theme_prism
#' @importFrom paletteer scale_fill_paletteer_d
#'
#' @export
#'
#' @examples
#' if (requireNamespace("ggprism", quietly = TRUE) &&
#'     requireNamespace("paletteer", quietly = TRUE)) {
#'   # Load example data
#'   data("BetterVis_Dotplot_Smoothline_example", package = "BetterVis")
#'
#'   data("BetterVis_Dotplot_Smoothline_example2", package = "BetterVis")
#'
#'
#'
#'
#'   # Example 1: Using a manual color palette and sizing points by a variable
#'   BetterVis_Dotplot_Smoothline(
#'     data = BetterVis_Dotplot_Smoothline_example,
#'     x_var = "Sepal.Width", y_var = "Sepal.Length", classified_var = "Species",
#'     size_var = "Petal.Length",
#'     color = c("#FBA72A", "#78B7C5", "#7294D4"),
#'     cor_position = c(3.7, 3.5)
#'    )
#'
#'   BetterVis_Dotplot_Smoothline(data =  BetterVis_Dotplot_Smoothline_example2, x_var = "Blood_Pressure", y_var = "Cholesterol_Level", classified_var = "Patient_Group",
#'      size_var = "Blood_Pressure", color = c("#FBA72A", "#78B7C5", "#7294D4"), cor_position = c(90,300))
#'
#'
#'
#'
#'
#'
#'   # Example 2: More customization and using default paletteer colors
#' BetterVis_Dotplot_Smoothline(data = BetterVis_Dotplot_Smoothline_example, x_var = "Sepal.Width", y_var = "Sepal.Length", classified_var = "Species",
#'                              size_var = "Sepal.Length", color = c("#BEBADA", "#80B1D3", "#FDB462"),  point_size = 15,
#'                              smoothline = TRUE, smoothline_width = 2, smoothline_color = "#FB8072",
#'                              cor_test = TRUE, cor_method = "spearman", cor_color = "black", cor_position = c(2, 4),
#'                              title = "Title", x_title = "Sepal.Width ", y_title = "Sepal.Length",
#'                              legend = TRUE, legend_position = "right",
#'                              global_size_adjust = 1)
#' ##+facet_wrap(vars(Species),drop = TRUE)
#' BetterVis_Dotplot_Smoothline(data =  BetterVis_Dotplot_Smoothline_example2, x_var = "Blood_Pressure", y_var = "Cholesterol_Level", classified_var = "Patient_Group",
#'                              size_var = "Blood_Pressure", color = c("#BEBADA", "#80B1D3", "#FDB462"),  point_size = 15,
#'                              smoothline = TRUE, smoothline_width = 2, smoothline_color = "#FB8072",
#'                              cor_test = TRUE, cor_method = "spearman", cor_color = "black", cor_position = c(90,300),
#'                              title = "Title", x_title = "Blood_Pressure ", y_title = "Cholesterol_Level",
#'                              legend = TRUE, legend_position = "right",
#'                              global_size_adjust = 1)
#' ##+facet_wrap(vars(Species),drop = TRUE)
#'
#' }
BetterVis_Dotplot_Smoothline <- function(data, x_var, y_var, classified_var, color = NULL,
                                         size_var = NULL, point_size = 15, smoothline = TRUE,
                                         smoothline_width = 1, smoothline_color = "#CE743C",
                                         cor_test = TRUE, cor_method = "spearman",
                                         cor_color = "black", cor_position = c(2, 2),
                                         title = NULL, x_title = NULL, y_title = NULL,
                                         legend = TRUE,
                                         legend_position = "right", global_size_adjust = 1) {

  if (is.null(color)) {
    color <- scale_fill_paletteer_d("colorblindr::OkabeIto")
  } else {
    color <- scale_fill_manual(values = color)
  }

  p <- ggplot(data, aes_string(x = x_var, y = y_var))

  if (is.null(size_var)) {
    p <- p + geom_point(aes_string(fill = classified_var), pch = 21, size = point_size)
  } else {
    p <- p + geom_point(aes_string(size = size_var, fill = classified_var), pch = 21) +
      scale_size_continuous(range = c(1, point_size))
  }

  if (smoothline) {
    p <- p + geom_smooth(method = "loess", se = FALSE, size = smoothline_width, color = smoothline_color)
  }

  if (cor_test) {
    p <- p + ggpubr::stat_cor(method = cor_method,
                              aes(label = paste(..rr.label.., ..p.label.., sep = "~")),
                              color = cor_color, geom = "label",
                              label.x = cor_position[1], label.y = cor_position[2],
                              size = 4 * global_size_adjust)
  }

  if (!is.null(title)) { p <- p + ggtitle(title) }
  if (!is.null(x_title)) { p <- p + labs(x = x_title) }
  if (!is.null(y_title)) { p <- p + labs(y = y_title) }

  if (!legend) {
    p <- p + theme(legend.position = "none")
  }

  p <- p + color +
    theme_prism(border = TRUE, base_size = 5) +
    theme(
      strip.text.x = element_text(size = 12 * global_size_adjust),
      title = element_text(size = 12 * global_size_adjust),
      legend.box.spacing = unit(1, "cm"),
      legend.text = element_text(size = 12 * global_size_adjust),
      legend.title = element_text(size = 12 * global_size_adjust),
      axis.text.y = element_text(size = 12 * global_size_adjust, angle = 0, vjust = 0.2),
      axis.text.x = element_text(size = 12 * global_size_adjust, angle = 0),
      panel.grid = element_line(color = "gray", size = 0.15, linetype = 2),
      panel.spacing = unit(1, "lines"),
      plot.caption = element_text(size = 12 * global_size_adjust),
      legend.position = legend_position,
      plot.margin = margin(t = 20, b = 20, l = 20, r = 20)
    )

  return(p)
}
# This handles the "no visible binding for global variable" NOTE from R CMD check
# for variables created inside ggpubr::stat_cor.
utils::globalVariables(c("..rr.label..", "..p.label.."))
