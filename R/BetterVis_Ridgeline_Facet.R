#' Create a Faceted Ridgeline Plot with Optional Annotations
#'
#' This function generates a faceted ridgeline plot using the `ggridges` package.
#' It allows for visualizing distributions across different categories, further
#' subdivided into facets, and supports an optional annotation sidebar.
#'
#' @name BetterVis_Ridgeline_Facet
#'
#' @param data A data frame containing the plotting data.
#' @param x_var A character string specifying the column name for the numeric variable (x-axis).
#' @param y_var A character string specifying the column name for the categorical variable (y-axis).
#' @param facet_var (Optional) A character string for the column name used to create plot facets.
#' @param nrow (Optional) The number of rows for the facet layout.
#' @param ncol (Optional) The number of columns for the facet layout.
#' @param color A vector of colors for the ridgeline fills, with a length equal to the number of levels in `y_var`.
#' @param annotation A logical value. If `TRUE`, an annotation plot is added. Default is `TRUE`.
#' @param annotation_var (Optional) A character string for the column name in `data` used for the annotation plot's fill colors.
#' @param color_annotation_var A vector of colors for the annotation plot.
#' @param all_text_adjust A single numeric value to uniformly scale text elements. Default is `1`.
#' @param combined_width A numeric vector of length 2 specifying the relative widths of the ridgeline plot and the annotation plot. Default is `c(1.5, 0.8)`.
#'
#' @return A `ggplot` or `cowplot` object.
#'
#' @importFrom ggplot2 ggplot aes scale_fill_manual scale_y_discrete labs theme element_text element_blank margin coord_cartesian geom_point scale_x_discrete theme_void facet_wrap vars
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom cowplot plot_grid
#' @importFrom rlang sym
#'
#' @export
#'
#' @examples
#' # Load example data from the package
#' data("BetterVis_Ridgeline_Facet_example", package = "BetterVis")
#'
#' # Define a color palette for the ridgelines
#' sampleColor <- c('#F6EB5E','#CD8280','#F0F0F0', '#54D0B4',
#'                  '#D2D1D1','#78C3ED', '#E69F00', '#CBDFF3',
#'                  '#3BFFB8', '#ECAD67','#B3B2B2',
#'                  '#959595', '#E98FBD','#2672B2')
#'
#' # Example 1: Faceting by a single variable
#' BetterVis_Ridgeline_Facet(
#'   data = BetterVis_Ridgeline_Facet_example,
#'   x_var = "CytoTRACEScore",
#'   y_var = "patient",
#'   facet_var = "smoking1",
#'   annotation_var = "mutationGroup",
#'   color = sampleColor
#' )
#'
#' # Example 2: Faceting with specified rows and columns
#' BetterVis_Ridgeline_Facet(
#'   data = BetterVis_Ridgeline_Facet_example,
#'   x_var = "CytoTRACEScore",
#'   y_var = "patient",
#'   facet_var = "Group",
#'   nrow = 2,
#'   ncol = 3,
#'   color = sampleColor,
#'   annotation = TRUE,
#'   annotation_var = "mutationGroup",
#'   color_annotation_var = c("#68d0ce", "#f4a4c9", "#4955d0", "#e09a2a", "#de6a73"),
#'   all_text_adjust = 1.1,
#'   combined_width = c(1.5, 0.8)
#' )
BetterVis_Ridgeline_Facet <- function(data, x_var, y_var,
                                      facet_var = NULL, nrow = NULL, ncol = NULL,
                                      annotation_var = NULL,
                                      color,
                                      annotation = TRUE,
                                      color_annotation_var = c("#68d0ce", "#f4a4c9", "#4955d0", "#e09a2a", "#de6a73"),
                                      all_text_adjust = 1,
                                      combined_width = c(1.5, 0.8)) {

  x_sym <- sym(x_var)
  y_sym <- sym(y_var)
  RidgePlot <- ggplot(data, aes(x = !!x_sym, y = !!y_sym)) +
    geom_density_ridges(aes(fill = !!y_sym), alpha = 0.8, show.legend = FALSE,
                        quantile_lines = TRUE, quantiles = 2, color = "white") +
    scale_fill_manual(values = color) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = '', y = '') +
    theme_ridges(grid = FALSE) +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 14 * all_text_adjust),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(10, 10, 10, 10)) +
    coord_cartesian(clip = "off")

  if (!is.null(facet_var)) {
    facet_sym <- sym(facet_var)
    RidgePlot <- RidgePlot +
      facet_wrap(vars(!!facet_sym), ncol = ncol, nrow = nrow) +
      theme(strip.text = element_text(size = 14 * all_text_adjust))
  }

  if (annotation && !is.null(annotation_var)) {
    annotation_sym <- sym(annotation_var)
    annotation_plot <- ggplot(data, aes(x = 1, y = !!y_sym)) +
      geom_point(aes(fill = !!annotation_sym), size = 4, shape = 21, stroke = 1.1) +
      scale_x_discrete(breaks = c(1)) +
      scale_fill_manual(name = "", values = color_annotation_var) +
      theme_void() +
      theme(legend.position = "right",
            plot.margin = margin(10, 0, 10, 10),
            legend.box.margin = margin(l = 0, r = 50, t = 0, b = 0),
            legend.text = element_text(size = 11 * all_text_adjust)) +
      coord_cartesian(clip = "off")

    combined_plots <- plot_grid(RidgePlot, annotation_plot, ncol = 2,
                                rel_widths = combined_width, align = "h", axis = "tb")
  } else {
    combined_plots <- RidgePlot
  }

  return(combined_plots)
}
