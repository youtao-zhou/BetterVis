#' Create a Ridgeline Plot with Optional Annotations
#'
#' This function generates a ridgeline plot using the `ggridges` package to visualize
#' the distribution of a numeric variable for different categories. It also supports
#' adding a separate annotation plot and custom arrows.
#'
#' @name BetterVis_Ridgeline_Single
#'
#' @param data A data frame containing the plotting data.
#' @param x_var A character string specifying the column name for the numeric variable (x-axis).
#' @param y_var A character string specifying the column name for the categorical variable (y-axis).
#' @param color A vector of colors for the ridgeline fills, with a length equal to the number of levels in `y_var`.
#' @param annotation A logical value. If `TRUE`, an annotation plot is added. Default is `TRUE`.
#' @param annotation_var (Optional) A character string for the column name in `data` used for the annotation plot's fill colors.
#' @param color_annotation_var A vector of colors for the annotation plot.
#' @param arrow A logical value. If `TRUE`, arrows and text can be added to the plot. Default is `TRUE`.
#' @param arrow_x1 (Optional) A numeric vector of length 2 for the starting x-coordinates of the two arrows.
#' @param arrow_x2 (Optional) A numeric vector of length 2 for the ending x-coordinates of the two arrows.
#' @param arrow_y1 (Optional) A numeric vector of length 2 for the starting y-coordinates of the two arrows.
#' @param arrow_y2 (Optional) A numeric vector of length 2 for the ending y-coordinates of the two arrows.
#' @param arrow_width Numeric. The line width of the arrows. Default is `0.6`.
#' @param arrow_text_left (Optional) Character string. Text to display near the left arrow.
#' @param arrow_text_middle (Optional) Character string. Text to display between the arrows.
#' @param arrow_text_right (Optional) Character string. Text to display near the right arrow.
#' @param all_text_adjust A single numeric value to uniformly scale text elements. Default is `1`.
#' @param combined_width A numeric vector of length 2 specifying the relative widths of the ridgeline plot and the annotation plot. Default is `c(1.5, 0.8)`.
#'
#' @return A `ggplot` or `cowplot` object.
#'
#' @importFrom ggplot2 ggplot aes scale_fill_manual scale_y_discrete labs theme element_text element_blank margin coord_cartesian annotate geom_point scale_x_discrete theme_void arrow
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom cowplot plot_grid
#' @importFrom rlang sym
#' @importFrom grid unit
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("BetterVis_Ridgeline_Single_example", package = "BetterVis")
#'
#' # Define a color palette for the ridgelines
#' sampleColor <- c('#F6EB5E','#CD8280','#F0F0F0', '#54D0B4',
#'                  '#D2D1D1','#78C3ED', '#E69F00', '#CBDFF3',
#'                  '#3BFFB8', '#ECAD67','#B3B2B2',
#'                  '#959595', '#E98FBD','#2672B2')
#'
#' # Example 1: Ridgeline plot with annotation
#' BetterVis_Ridgeline_Single(
#'   data = BetterVis_Ridgeline_Single_example,
#'   x_var = "CytoTRACEScore",
#'   y_var = "patient",
#'   color = sampleColor,
#'   annotation = TRUE,
#'   annotation_var = "mutationGroup",
#'   color_annotation_var = c("#68d0ce", "#f4a4c9", "#e09a2a", "#de6a73")
#' )
#'
#' # Example 2: Full plot with annotations and arrows
#' BetterVis_Ridgeline_Single(
#'   data = BetterVis_Ridgeline_Single_example,
#'   x_var = "CytoTRACEScore",
#'   y_var = "patient",
#'   color = sampleColor,
#'   annotation = TRUE,
#'   annotation_var = "mutationGroup",
#'   color_annotation_var = c("#68d0ce", "#f4a4c9", "#e09a2a", "#de6a73"),
#'   arrow = TRUE,
#'   arrow_x1 = c(0.88, 0.12),
#'   arrow_x2 = c(1.12, -0.12),
#'   arrow_y1 = c(0.5, 0.5),
#'   arrow_y2 = c(0.5, 0.5),
#'   arrow_text_left = "Left",
#'   arrow_text_middle = "Middle",
#'   arrow_text_right = "Right",
#'   all_text_adjust = 1,
#'   combined_width = c(1.5, 0.8)
#' )

BetterVis_Ridgeline_Single <- function(data, x_var, y_var,
                                       annotation_var = NULL,
                                       color,
                                       annotation = TRUE,
                                       color_annotation_var = c("#68d0ce", "#f4a4c9", "#4955d0", "#e09a2a", "#de6a73"),
                                       arrow = TRUE,
                                       arrow_x1 = NULL,
                                       arrow_x2 = NULL,
                                       arrow_y1 = NULL,
                                       arrow_y2 = NULL,
                                       arrow_width = 0.6,
                                       arrow_text_left = NULL,
                                       arrow_text_middle = NULL,
                                       arrow_text_right = NULL,
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

  if (arrow) {
    arrow_annotations <- list()
    if (!is.null(arrow_x1) && !is.null(arrow_x2) && !is.null(arrow_y1) && !is.null(arrow_y2)) {
      arrow_annotations[[length(arrow_annotations) + 1]] <-
        annotate("segment",
                 x = arrow_x1[1], xend = arrow_x2[1],
                 y = arrow_y1[1], yend = arrow_y2[1],
                 color = "black", linewidth = arrow_width,
                 arrow = arrow(type = "open", length = unit(0.1, "inches")))
      arrow_annotations[[length(arrow_annotations) + 1]] <-
        annotate("segment",
                 x = arrow_x1[2], xend = arrow_x2[2],
                 y = arrow_y1[2], yend = arrow_y2[2],
                 color = "black", linewidth = arrow_width,
                 arrow = arrow(type = "open", length = unit(0.1, "inches")))
    }
    if (!is.null(arrow_text_right) && !is.null(arrow_x2) && !is.null(arrow_y1)) {
      arrow_annotations[[length(arrow_annotations) + 1]] <-
        annotate("text",
                 x = arrow_x2[1], y = arrow_y1[1] - 0.5,
                 label = arrow_text_right, size = 4.2 * all_text_adjust, hjust = 0)
    }
    if (!is.null(arrow_text_left) && !is.null(arrow_x2) && !is.null(arrow_y1)) {
      arrow_annotations[[length(arrow_annotations) + 1]] <-
        annotate("text",
                 x = arrow_x2[2], y = arrow_y1[2] - 0.5,
                 label = arrow_text_left, size = 4.2 * all_text_adjust, hjust = 1)
    }
    if (!is.null(arrow_text_middle) && !is.null(arrow_x1) && !is.null(arrow_y1)) {
      arrow_annotations[[length(arrow_annotations) + 1]] <-
        annotate("text",
                 x = mean(c(arrow_x1[1], arrow_x1[2])), y = arrow_y1[1] - 0.3,
                 label = arrow_text_middle, size = 4.2 * all_text_adjust, hjust = 0.5)
    }

    if (length(arrow_annotations) > 0) {
      RidgePlot <- RidgePlot + arrow_annotations
    }
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

    combined_plots <- plot_grid(RidgePlot, annotation_plot, ncol = 2, rel_widths = combined_width, align = "h", axis = "tb")
  } else {
    combined_plots <- RidgePlot
  }

  return(combined_plots)
}
