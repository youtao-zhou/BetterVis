#' Create a Classic Heatmap
#'
#' This function generates a classic heatmap from a data frame or matrix using
#' `ggplot2`. It reshapes wide data into a long format and provides extensive
#' options for aesthetic customization.
#'
#' @name BetterVis_Heatmap_Common
#'
#' @param data A data frame or matrix for plotting. Rows and columns will become the heatmap axes.
#' @param trans Logical. If `TRUE`, the input data is transposed before plotting. Default is `FALSE`.
#' @param heatmap_color A vector of colors for the color gradient. Defaults to a `MetBrewer` palette.
#' @param legend_position Character string. The position of the legend (e.g., "top", "right"). Default is `"top"`.
#' @param legend_title Character string for the legend title.
#' @param legend_break Numeric. The interval for the legend's color scale breaks. Default is `1`.
#' @param legend_text_size Numeric. The font size for the legend text.
#' @param legend_text_bold Logical. If `TRUE`, legend text is bold.
#' @param x_text_size,y_text_size Numeric. Font size for x and y axis text.
#' @param x_text_bold,y_text_bold Logical. If `TRUE`, axis text is bold.
#' @param x_text_angle Numeric. The angle of x-axis text.
#'
#' @return A `ggplot` object representing the heatmap.
#'
#' @importFrom ggplot2 ggplot aes geom_tile labs scale_y_discrete scale_x_discrete coord_cartesian scale_fill_gradientn theme_test theme element_text element_blank margin unit guides guide_colorsteps
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom tibble rownames_to_column
#' @importFrom magrittr set_colnames
#' @importFrom MetBrewer met.brewer
#'
#' @export
#'
#' @examples
#' if (requireNamespace("MetBrewer", quietly = TRUE)) {
#'   # Load example data
#'   data("BetterVis_Heatmap_Common_example", package = "BetterVis")
#'
#'   # Example 1: Basic usage
#'   BetterVis_Heatmap_Common(BetterVis_Heatmap_Common_example, trans = FALSE)
#'
#'   # Example 2: More customized plot
#'   BetterVis_Heatmap_Common(
#'     data = BetterVis_Heatmap_Common_example,
#'     trans = FALSE,
#'     heatmap_color = met.brewer("Isfahan1"),
#'     legend_break = 2,
#'     legend_position = "bottom",
#'     legend_title = "Value",
#'     legend_text_size = 8,
#'     legend_text_bold = TRUE,
#'     x_text_size = 10,
#'     x_text_bold = TRUE,
#'     x_text_angle = 45,
#'     y_text_size = 10,
#'     y_text_bold = FALSE
#'   )
#'   # You can add other ggplot2 layers:
#'   # + guides(fill = "none")
#'   # + theme(axis.text.x = element_text(size = 15))
#' }
BetterVis_Heatmap_Common <- function(data, heatmap_color = MetBrewer::met.brewer("Hiroshige"),
                                     legend_position = "top", legend_title = NULL, legend_break = 1,
                                     legend_text_size = 10, legend_text_bold = TRUE,
                                     x_text_size = 12, x_text_bold = TRUE, x_text_angle = 90,
                                     y_text_size = 12, y_text_bold = TRUE, trans = FALSE) {

  if (trans) {
    data <- as.data.frame(t(data))
  }

  min_value <- floor(min(data, na.rm = TRUE))
  max_value <- ceiling(max(data, na.rm = TRUE))

  breaks_value <- seq(min_value, max_value, by = legend_break)

  data_long <- data %>%
    tibble::rownames_to_column("ID") %>%
    tidyr::pivot_longer(-1) %>%
    magrittr::set_colnames(c("ID", "name", "value"))

  p <- data_long %>%
    ggplot(aes(.data$ID, .data$name, fill = .data$value)) +
    geom_tile() +
    labs(x = NULL, y = NULL) +
    scale_y_discrete(expand = c(0, 0), position = "right") +
    scale_x_discrete(expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    scale_fill_gradientn(
      colors = heatmap_color,
      guide = "colorbar" ,
      limits = c(min(breaks_value), max(breaks_value)),
      breaks = breaks_value
    ) +
    theme_test() +
    theme(
      axis.text.x = element_text(
        color = "black",
        size = x_text_size,
        angle = x_text_angle,
        vjust = 0.5,
        hjust = 1, # Use hjust=1 for angled text
        face = ifelse(x_text_bold, "bold", "plain")
      ),
      axis.text.y = element_text(
        color = "black",
        size = y_text_size,
        angle = 0,
        vjust = 0.5,
        face = ifelse(y_text_bold, "bold", "plain")
      ),
      axis.ticks = element_blank(),
      legend.position = legend_position,
      legend.background = element_blank(),
      legend.text = element_text(
        size = legend_text_size,
        color = "black",
        face = ifelse(legend_text_bold, "bold", "plain")
      ),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
      legend.key.width = unit(3.3, "cm")
    ) +
    guides(
      fill = guide_colorsteps(
        show.limits = TRUE,
        title = legend_title,
        barwidth = 28,
        barheight = 1
      )
    )

  return(p)
}
