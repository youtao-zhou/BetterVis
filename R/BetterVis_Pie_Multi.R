#' Create a Scatter Plot with Pie Chart Glyphs
#'
#' This function generates a scatter plot where each point is rendered as a single
#' or a concentric pie chart. It uses the `jjPlot` package for the pie chart
#' geometry and `ggnewscale` to handle multiple fill scales.
#'
#' @name BetterVis_Pie_Multi
#'
#' @param data_in A data frame for the inner pie chart.
#' @param data_out (Optional) A data frame for the outer pie chart.
#' @param facet_var A character string for the column name used to group pie charts. In the `data_in` and `data_out` data frames, each unique value in this column corresponds to one pie glyph on the plot.
#' @param x_var,y_var Character strings for the column names that define the x and y coordinates of each pie chart glyph.
#' @param fill_var1,pie_var1 Character strings for the column names in `data_in` that define the inner pie's slice colors and proportions, respectively.
#' @param fill_var2,pie_var2 (Optional) Character strings for the column names in `data_out` that define the outer pie's slice colors and proportions.
#' @param in_color,out_color (Optional) Vectors of colors for the inner and outer pies.
#' @param outer_pie_width,inner_pie_width Numeric. The radius/width of the outer and inner pie charts.
#' @param circle_radius Numeric. The radius of the central circle.
#' @param draw_outer_pie Logical. If `TRUE`, the outer pie chart is drawn. Requires `data_out`.
#'
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes_string geom_text labs scale_fill_manual theme element_blank element_text unit margin
#' @importFrom jjPlot geom_jjPointPie
#' @importFrom ggnewscale new_scale_fill
#' @importFrom dplyr %>% distinct
#' @importFrom rlang syms !!!
#'
#' @export
#'
#' @examples
#' if (requireNamespace("jjPlot", quietly = TRUE) && requireNamespace("ggnewscale", quietly = TRUE)) {
#' library(tidyverse)
#' library(ggtext)
#' library(dplyr)
#' library(ggplot2)
#' library(scales)
#' library(MetBrewer)
#'
#'   # Load example data
#'   data("BetterVis_Pie_Multi_example1", package = "BetterVis")
#'   data("BetterVis_Pie_Multi_example2", package = "BetterVis")
#'
#'   # Define color palettes
#'   color1 <- c("#FED439B2", "#709AE1B2", "#8A9197B2", "#D2AF81B2",
#'               "#FD7446B2", "#D5E4A2B2", "#197EC0B2", "#46732EB2",
#'               "#71D0F5B2", "#C6CDF7", "#91331FB2", "#1A9993B2",
#'               "#FD8CC1B2", "#FAD510", "#0A9F9D", "#3B9AB2",
#'               "#FF0000", "#969BC7", "#B695BC", "#6F9954")
#'   color2 <- c("#FBA72A", "#969BC7"); names(color2) <- c("Cured", "Improved")
#'
#'   # Generate the plot
#'   BetterVis_Pie_Multi(
#'     data_in = BetterVis_Pie_Multi_example1,
#'     data_out = BetterVis_Pie_Multi_example2,
#'     facet_var = "Pie_Label",
#'     x_var = "Treatment",
#'     y_var = "Patient_Stage",
#'     fill_var1 = "Microbe_Phylum",
#'     pie_var1 = "Abundance",
#'     fill_var2 = "Outcome",
#'     pie_var2 = "Proportion",
#'     in_color = color1,
#'     out_color = color2,
#'     draw_outer_pie = TRUE
#'   )
#' }
BetterVis_Pie_Multi <- function(data_in, data_out = NULL, facet_var, x_var, y_var,
                                fill_var1, pie_var1,
                                fill_var2 = NULL, pie_var2 = NULL,
                                in_color = NULL, out_color = NULL,
                                outer_pie_width = 3, inner_pie_width = 2.5,
                                circle_radius = 0.1, draw_outer_pie = TRUE) {

  required_cols_in <- c(facet_var, x_var, y_var, fill_var1, pie_var1)
  if (!all(required_cols_in %in% names(data_in))) stop("data_in missing required columns.")
  if (draw_outer_pie) {
    if (is.null(data_out)) stop("data_out must be provided when draw_outer_pie is TRUE.")
    required_cols_out <- c(facet_var, x_var, y_var, fill_var2, pie_var2)
    if (!all(required_cols_out %in% names(data_out))) stop("data_out missing required columns.")
  }
  if (is.null(out_color) && draw_outer_pie) {
    out_color <- scales::hue_pal()(length(unique(data_out[[fill_var2]])))
    names(out_color) <- unique(data_out[[fill_var2]])
  }
  if (is.null(in_color)) {
    in_color <- scales::hue_pal()(length(unique(data_in[[fill_var1]])))
    names(in_color) <- unique(data_in[[fill_var1]])
  }

  plot <- ggplot()
  if (draw_outer_pie) {
    plot <- plot +
      geom_jjPointPie(data = data_out,
                      aes(x = .data[[x_var]], y = .data[[y_var]],
                          pievar = .data[[pie_var2]], fill = .data[[fill_var2]],
                          group = interaction(.data[[x_var]], .data[[y_var]]),
                          width = outer_pie_width),
                      add.circle = TRUE, color = "white", circle.rev = TRUE,
                      circle.radius = circle_radius, circle.fill = "white") +
      labs(fill = fill_var2) +
      scale_fill_manual(values = out_color) +
      new_scale_fill()
  }
  plot <- plot +
    geom_jjPointPie(data = data_in,
                    aes(x = .data[[x_var]], y = .data[[y_var]],
                        pievar = .data[[pie_var1]], fill = .data[[fill_var1]],
                        group = interaction(.data[[x_var]], .data[[y_var]]),
                        width = inner_pie_width),
                    color = NA, add.circle = TRUE, circle.rev = TRUE,
                    circle.radius = circle_radius, circle.fill = "white") +
    geom_text(data = data_in %>% distinct(!!!syms(c(facet_var, x_var, y_var))),
              aes(x = .data[[x_var]], y = .data[[y_var]], label = .data[[facet_var]]),
              size = 3.5, color = "black", vjust = 0.5, hjust = 0.5) +
    labs(fill = fill_var1) +
    scale_fill_manual(values = in_color) +
    theme(
      panel.background = element_blank(),
      axis.text = element_text(size=15),
      axis.title = element_text(size=15),
      legend.background = element_blank(),
      legend.key = element_blank()
    )
  return(plot)

}
