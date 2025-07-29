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
#'   data("BetterVis_Pie_Multi_example_in", package = "BetterVis")
#'   data("BetterVis_Pie_Multi_example_out", package = "BetterVis")
#'
#'   # Define color palettes
#'   color1 <- c("#FED439B2", "#709AE1B2", "#8A9197B2", "#D2AF81B2",
#'               "#FD7446B2", "#D5E4A2B2", "#197EC0B2", "#46732EB2",
#'               "#71D0F5B2", "#C6CDF7", "#91331FB2", "#1A9993B2",
#'               "#FD8CC1B2", "#FAD510", "#0A9F9D", "#3B9AB2",
#'               "#FF0000", "#969BC7", "#B695BC", "#6F9954")
#'   color2 <- c("#FBA72A", "#969BC7")
#'
#'   # Generate the plot
#'   BetterVis_Pie_Multi(
#'     data_in = BetterVis_Pie_Multi_example_in,
#'     data_out = BetterVis_Pie_Multi_example_out,
#'     facet_var = "Habitat",
#'     x_var = "type",
#'     y_var = "Group",
#'     fill_var1 = "Phylum",
#'     pie_var1 = "Proportion",
#'     fill_var2 = "name",
#'     pie_var2 = "value",
#'     in_color = color1,
#'     out_color = color2,
#'     draw_outer_pie = TRUE
#'   )
#' }
BetterVis_Pie_Multi <- function(data_in, data_out = NULL, facet_var, x_var, y_var,
                                fill_var1, pie_var1,
                                fill_var2 = NULL,
                                pie_var2 = NULL,
                                in_color = NULL,
                                out_color = NULL,
                                outer_pie_width = 3,
                                inner_pie_width = 2.5,
                                circle_radius = 0.1,
                                draw_outer_pie = TRUE) {

  required_cols_in <- c(facet_var, x_var, y_var, fill_var1, pie_var1)
  if (!all(required_cols_in %in% names(data_in))) {
    stop(paste0("Error: data_in is missing required columns for the inner pie. Missing: ",
                paste(setdiff(required_cols_in, names(data_in)), collapse = ", ")))
  }

  if (draw_outer_pie) {
    if (is.null(data_out)) {
      stop("Error: `data_out` must be provided when `draw_outer_pie` is TRUE.")
    }
    if (is.null(fill_var2) || is.null(pie_var2)) {
      stop("Error: `fill_var2` and `pie_var2` must be specified when `draw_outer_pie` is TRUE.")
    }
    required_cols_out <- c(facet_var, x_var, y_var, fill_var2, pie_var2)
    if (!all(required_cols_out %in% names(data_out))) {
      stop(paste0("Error: data_out is missing required columns for the outer pie. Missing: ",
                  paste(setdiff(required_cols_out, names(data_out)), collapse = ", ")))
    }
  }

  if (is.null(out_color) && draw_outer_pie) {
    out_color <- c("#FBA72A", "#969BC7")
    names(out_color) <- unique(data_out[[fill_var2]])
  }

  if (is.null(in_color)) {
    in_color <- c("#FED439B2", "#709AE1B2", "#8A9197B2", "#D2AF81B2",
                  "#FD7446B2", "#D5E4A2B2", "#197EC0B2", "#46732EB2",
                  "#71D0F5B2", "#C6CDF7", "#91331FB2", "#1A9993B2",
                  "#FD8CC1B2", "#FAD510", "#0A9F9D", "#3B9AB2",
                  "#FF0000", "#969BC7", "#B695BC", "#6F9954")
    unique_fill1_values <- unique(data_in[[fill_var1]])
    names(in_color) <- unique_fill1_values[1:min(length(unique_fill1_values), length(in_color))]
  }

  plot <- ggplot()

  if (draw_outer_pie) {
    plot <- plot +
      geom_jjPointPie(data = data_out,
                      aes_string(x = x_var, y = y_var,
                                 pievar = pie_var2,
                                 fill = fill_var2,
                                 group = facet_var,
                                 width = outer_pie_width),
                      add.circle = TRUE,
                      color = "white",
                      circle.rev = TRUE,
                      circle.radius = circle_radius,
                      circle.fill = "white") +
      labs(fill = fill_var2) +
      scale_fill_manual(values = out_color) +
      new_scale_fill()
  }

  plot <- plot +
    geom_jjPointPie(data = data_in,
                    aes_string(x = x_var, y = y_var,
                               pievar = pie_var1,
                               fill = fill_var1,
                               group = facet_var,
                               width = inner_pie_width),
                    color = NA,
                    add.circle = TRUE,
                    circle.rev = TRUE,
                    circle.radius = circle_radius,
                    circle.fill = "white") +
    geom_text(data = data_in %>% distinct(!!!syms(c(facet_var, x_var, y_var))),
              aes_string(x = x_var, y = y_var, label = facet_var),
              size = 3.5, color = "black",
              vjust = 0.5, hjust = 0.5) +
    labs(fill = fill_var1) +
    scale_fill_manual(values = in_color) +
    theme(panel.background = element_blank(),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.height = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.text = element_text(color = "black", size = 10, face = "bold",
                                     margin = margin(0, 0, 0, 0.2, unit = "cm")),
          legend.title = element_text(color = "black", size = 10, face = "bold"))

  return(plot)
}
