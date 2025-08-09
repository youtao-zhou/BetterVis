#' Create a Heatmap for Bi-Continuous Data
#'
#' This function generates a heatmap (tile plot) from data in a long format,
#' where two variables map to the x and y axes, and a third continuous variable
#' maps to the fill color.
#'
#' @name BetterVis_Heatmap_Bicontinuous
#'
#' @param data A data frame containing the data for plotting.
#' @param x_var,y_var Character strings for the column names of the x and y-axis variables.
#' @param fill_var Character string for the column name of the continuous variable for the tile fill color.
#' @param vline_position,hline_position Numeric. The position for vertical and horizontal reference lines.
#' @param vline_color,hline_color Character strings. The color of the vertical and horizontal lines.
#' @param hline_linetype Character string. The line type for the horizontal line (e.g., "dashed", "solid").
#' @param heatmap_color A vector of colors to be used for the color gradient. Defaults to a `MetBrewer` palette.
#' @param legend_break Numeric. The interval for the legend's color scale breaks. Default is `1`.
#' @param legend_title Character string for the legend title. Supports markdown via `ggtext`.
#' @param legend_position Character string. The position of the legend (e.g., "bottom", "right").
#' @param legend_title_position Character string. The position of the legend title relative to the legend.
#'
#' @return A `ggplot` object representing the heatmap.
#'
#' @importFrom ggplot2 ggplot aes_string geom_tile geom_vline geom_hline labs scale_fill_gradientn guides guide_colorsteps theme_test theme element_text unit
#' @importFrom dplyr %>%
#' @importFrom MetBrewer met.brewer
#' @importFrom ggtext element_markdown
#'
#' @export
#'
#' @examples
#' library(BetterVis)
#' library(MetBrewer)
#'
#' ## Data Input
#' data("BetterVis_Heatmap_Bicontinuous_example",package="BetterVis")
#' data("BetterVis_Heatmap_Bicontinuous_example2",package="BetterVis")
#'
#' ## Simple Example
#' BetterVis_Heatmap_Bicontinuous(data = BetterVis_Heatmap_Bicontinuous_example, x_var = "ID", y_var = "name", fill_var = "value",legend_title = "Change")
#'
#'
#' ## Advanced Example
#' BetterVis_Heatmap_Bicontinuous(BetterVis_Heatmap_Bicontinuous_example, x_var ="ID", y_var ="name", fill_var ="value",
#' vline_position = 0.25, hline_position = 0,
#' vline_color = "black", hline_color = "black", hline_linetype = "dashed",
#' heatmap_color =  rev(met.brewer("Hiroshige")),
#' legend_break = 1, legend_title = "Change",legend_position = "bottom",legend_title_position = "bottom") +
#' theme(axis.text = element_text(size = 12))
#'
#'
#' ## We have generated a new dataset to verify its validity
#' BetterVis_Heatmap_Bicontinuous(BetterVis_Heatmap_Bicontinuous_example2, x_var ="ID", y_var ="name", fill_var ="value",
#' vline_position = 15, hline_position = 300,
#' vline_color = "black", hline_color = "black", hline_linetype = "dashed",
#' heatmap_color =  rev(met.brewer("Hiroshige")),
#' legend_break = 1, legend_title = "Change",legend_position = "bottom",legend_title_position = "bottom") +
#' theme(axis.text = element_text(size = 12))
#'
#'
BetterVis_Heatmap_Bicontinuous <- function(data, x_var, y_var, fill_var,
                                           vline_position = 0.25, hline_position = 0,
                                           vline_color = "black", hline_color = "black", hline_linetype = "dashed",
                                           heatmap_color = rev(MetBrewer::met.brewer("Hiroshige")),
                                           legend_break = 1, legend_title = NULL, legend_position = "bottom", legend_title_position = "bottom") {

  min_value <- floor(min(data[[fill_var]], na.rm = TRUE))
  max_value <- ceiling(max(data[[fill_var]], na.rm = TRUE))

  breaks_value <- seq(min_value, max_value, by = legend_break)

  p <- data %>%
    ggplot(aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_tile() +
    geom_vline(xintercept = vline_position, color = vline_color) +
    geom_hline(yintercept = hline_position, color = hline_color, linetype = hline_linetype) +
    labs(x = NULL, y = NULL) +
    scale_fill_gradientn(
      colors = heatmap_color,
      limits = c(min(breaks_value), max(breaks_value)),
      breaks = breaks_value,
      na.value = NA
    ) +
    guides(
      fill = guide_colorsteps(
        show.limits = TRUE,
        title = legend_title,
        title.position = legend_title_position,
        breaks = breaks_value,
        barwidth = 28,
        barheight = 1
      )
    ) +
    theme_test() +
    theme(
      legend.position = legend_position,
      legend.direction = "horizontal",
      axis.text = element_text(color = "black"),
      axis.ticks.length.x = unit(-0.2, "cm"),
      axis.ticks.length.y = unit(-0.2, "cm"),
      legend.text = element_text(color = "black"),
      legend.title = element_markdown(hjust = 0.5, color = "black", face = "bold", size = 9)
    )

  return(p)
}
