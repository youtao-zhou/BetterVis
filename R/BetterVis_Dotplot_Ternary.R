#' Create a Ternary Plot
#'
#' This function generates a ternary plot using the `ggtern` package to visualize
#' the relationship between three compositional variables. An optional fourth variable
#' can be mapped to point size and color.
#'
#' @name BetterVis_Dotplot_Ternary
#'
#' @param data A data frame containing the plotting data.
#' @param x_var,y_var,z_var Character strings for the column names of the three compositional variables.
#' @param size_var (Optional) A character string for a numeric column to be mapped to point size and color gradient.
#' @param color A vector of colors for the color gradient (if `size_var` is used) or a single color for all points (if `size_var` is `NULL`). Defaults to a `MetBrewer` palette.
#' @param point_size Numeric. The base size of the points. If `size_var` is used, this acts as a scaling factor. Default is `3`.
#' @param point_alpha Numeric. The transparency of the points (0-1). Default is `0.6`.
#' @param title,subtitle,x_title,y_title,z_title Character strings for plot titles and axis labels.
#' @param legend Logical. If `TRUE`, the legend is displayed. Default is `TRUE`.
#' @param border_size Numeric. The thickness of the ternary plot axis lines. Default is `1.2`.
#' @param border_color A vector of three colors for the top, left, and right axis lines, respectively.
#' @param all_text_adjust A single numeric value to uniformly scale all text elements. Default is `1`.
#'
#' @return A `ggtern` object.
#'
#' @importFrom ggtern ggtern theme_bw theme_custom theme_legend_position
#' @importFrom ggplot2 aes_string geom_point scale_size_continuous scale_color_gradientn labs guides guide_legend guide_colorbar theme element_text element_line element_rect
#' @importFrom MetBrewer met.brewer
#' @importFrom grid arrow
#'
#' @export
#'
#' @examples
#' if (requireNamespace("ggtern", quietly = TRUE) && requireNamespace("MetBrewer", quietly = TRUE)) {
#' library(ggplot2)
#'   # Load example data
#'   data("BetterVis_Dotplot_Ternary_example", package = "BetterVis")
#'
#'   # Example 1: Basic plot with size and color mapped to a variable
#'   BetterVis_Dotplot_Ternary(
#'     data = BetterVis_Dotplot_Ternary_example,
#'     x_var = "x", y_var = "y", z_var = "z", size_var = "size",
#'     all_text_adjust = 1.5
#'   )
#'
#'   # Example 2: More customized plot with titles and different border colors
#'   BetterVis_Dotplot_Ternary(
#'     data = BetterVis_Dotplot_Ternary_example,
#'     x_var = "x", y_var = "y", z_var = "z", size_var = "size",
#'     color = MetBrewer::met.brewer("Isfahan1"),
#'     point_size = 4,
#'     point_alpha = 0.7,
#'     border_size = 2,
#'     border_color = c("#68d0ce", "#e09a2a", "#de6a73"),
#'     legend = TRUE,
#'     all_text_adjust = 1.3,
#'     title = "Ternary Plot Example",
#'     subtitle = "Composition of Three Variables",
#'     x_title = "Component X", y_title = "Component Y", z_title = "Component Z"
#'   )
#'   # You can add other ggplot2/ggtern layers:
#'   # + theme(plot.title = element_text(size=20))
#' }
BetterVis_Dotplot_Ternary <- function(data, x_var, y_var, z_var, size_var,
                                      color = MetBrewer::met.brewer("Hiroshige"),
                                      point_size = 3, point_alpha = 0.6,
                                      title = "", subtitle = "",
                                      x_title = "", y_title = "", z_title = "",
                                      legend = TRUE,
                                      border_size = 1.2,
                                      border_color = c("#68d0ce", "#e09a2a", "#de6a73"),
                                      all_text_adjust = 1) {

  if (!is.null(size_var)) {
    scale_factor <- point_size / 3
    p1 <- ggtern::ggtern(data = data,
                         mapping = aes_string(x = x_var, y = y_var, z = z_var,
                                              size = size_var, color = size_var)) +
      geom_point(alpha = point_alpha) +
      scale_size_continuous(range = c(3 * scale_factor, 10 * scale_factor)) +
      scale_color_gradientn(colors = color)
  } else {
    p1 <- ggtern::ggtern(data = data,
                         mapping = aes_string(x = x_var, y = y_var, z = z_var)) +
      geom_point(alpha = point_alpha, size = point_size, color = color[1])
  }

  p1 <- p1 +
    theme_bw(base_size = 40 * all_text_adjust) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title,
      z = z_title
    ) +
    guides(size = guide_legend(), color = guide_colorbar())

  # Helper function for a custom theme defined within the main function scope
  theme_tropical <- function(base_size = 15 * all_text_adjust, base_family = "") {
    col.T <- border_color[1]
    col.L <- border_color[2]
    col.R <- border_color[3]
    col.text <- "black"
    col.bg.strip <- "gray90"
    col.bg <- "white"
    arrow_obj <- grid::arrow()
    theme_custom(base_size = base_size, base_family = base_family,
                 tern.plot.background = NULL, tern.panel.background = col.bg,
                 col.T = col.T, col.L = col.L, col.R = col.R, col.grid.minor = col.bg.strip) +
      theme(text = element_text(color = col.text, size = 16 * all_text_adjust),
            strip.background = element_rect(color = col.text, fill = col.bg.strip),
            strip.text = element_text(color = col.text, size = 16 * all_text_adjust),
            tern.axis.line.T = element_line(color = col.T, linewidth = border_size),
            tern.axis.line.L = element_line(color = col.L, linewidth = border_size),
            tern.axis.line.R = element_line(color = col.R, linewidth = border_size),
            tern.axis.arrow.show = TRUE,
            tern.axis.arrow = element_line(arrow = arrow_obj)
      )
  }

  p2 <- p1 + theme_tropical() +
    theme_legend_position(x = "topright") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16 * all_text_adjust, vjust = -3, color = "black"),
      plot.subtitle = element_text(hjust = 0.5, size = 15 * all_text_adjust, vjust = -3),
      tern.axis.title = element_text(size = 16 * all_text_adjust),
      legend.position = "right"
    )

  if (!legend) {
    p2 <- p2 + theme(legend.position = "none")
  }

  return(p2)
}
