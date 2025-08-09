#' Create a Circular Heatmap
#'
#' This function generates a circular heatmap (radial tile plot) using `ggplot2`
#' and `geomtextpath`. It is designed to visualize tabular data in a polar
#' coordinate system with curved text labels.
#'
#' @name BetterVis_Heatmap_Circle
#'
#' @param data A data frame containing the plotting data in a long format.
#' @param value_var A character string for the column name of the numeric variable for the tile fill color.
#' @param classified_text_var A character string for the column name of the categorical variable for the outer ring labels.
#' @param classified_value_var A character string for the column name of the categorical variable for the inner rings (y-axis).
#' @param heatmap_color A vector of colors for the color gradient. Defaults to a `MetBrewer` palette.
#' @param text_size Numeric. The size for the curved text labels. Default is `4`.
#' @param barwidth,barheight Numeric. The width and height of the color bar legend.
#' @param legend_break Numeric. The interval for the legend's color scale breaks. Default is `1`.
#' @param legend_text_size Numeric. The font size for the legend text. Default is `12`.
#' @param classified_value_var_label_size Numeric. The font size for the radial axis labels. Default is `10`.
#'
#' @return A `ggplot` object representing the circular heatmap.
#'
#' @importFrom ggplot2 ggplot aes sym geom_tile scale_fill_gradientn scale_x_discrete scale_y_discrete geom_rect guides guide_colorbar theme element_blank element_text margin unit alpha coord_radial
#' @importFrom geomtextpath geom_textpath
#' @importFrom dplyr %>% filter
#' @importFrom MetBrewer met.brewer
#'
#' @export
#'
#' @examples
#'  library(ggplot2)
#'  library(MetBrewer)
#'  library(geomtextpath)
#'  library(dplyr)
#' ## Data Input
#' data("BetterVis_Heatmap_Circle_example",package="BetterVis")
#' BetterVis_Heatmap_Circle(data = BetterVis_Heatmap_Circle_example, value_var = "value", classified_text_var = "id", classified_value_var = "name")
#' BetterVis_Heatmap_Circle(data = BetterVis_Heatmap_Circle_example, value_var = "value", classified_text_var = "id", classified_value_var = "name",
#' heatmap_color = alpha(met.brewer("Hiroshige"), 1),
#' text_size =4,barwidth = 9, barheight = 0.5,legend_break = 1,legend_text_size=12,
#' classified_value_var_label_size = 10)
#'
#' ## We have modified the iris dataset to create these boxplots
#' iris$Group <- rep(paste0("Group", 1:10), times = 15)
#' iris$Group <-as.factor(iris$Group)
#' iris$Group2 <- rep(rep(paste0("Color", 1:5), each = 10),3)
#' iris$Group2 <- sample(iris$Group2)
#' iris$Group2 <-as.factor(iris$Group2)
#' BetterVis_Heatmap_Circle(data = iris, value_var = "Sepal.Width", classified_text_var = "Group", classified_value_var = "Group2")
BetterVis_Heatmap_Circle <- function(data, value_var, classified_text_var, classified_value_var,
                                     heatmap_color=alpha(met.brewer("Hiroshige"), 1),
                                     text_size = 4, barwidth = 9, barheight = 0.5,
                                     legend_break=1,legend_text_size=12,classified_value_var_label_size= 10) {

  factor_levels_count <- length(unique(data[[classified_value_var]]))

  min_value <- round(min(data[[value_var]], na.rm = TRUE))
  max_value <- round(max(data[[value_var]], na.rm = TRUE))

  breaks_value <- seq(min_value, max_value, by = legend_break)

  p <- ggplot(data, aes(x = !!sym(classified_text_var), y = !!sym(classified_value_var), fill = !!sym(value_var))) +
    geom_tile(color = "white", linewidth = 1) +
    scale_fill_gradientn(colors = heatmap_color, na.value = NA,breaks = breaks_value) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    coord_radial(start = 0.02, end = pi * 2, inner.radius = 0.6) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = factor_levels_count + 2, ymax = factor_levels_count + 2),
              linewidth = 7, color = "#85D4E3", fill = NA) +
    guides(fill = guide_colorbar(position = "inside",
                                 barwidth = unit(barwidth, "cm"),
                                 barheight = unit(barheight, "cm"),
    )) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text.r = element_text(colour = "black", size = classified_value_var_label_size, vjust = 0.5,
                                     margin = margin(r = -0.5, unit = "cm")),
          legend.title = element_blank(),
          legend.direction = "horizontal",
          legend.text = element_text(size=legend_text_size))

  for (i in seq_along(unique(data[[classified_value_var]]))) {
    p <- p + geom_textpath(data = data %>% filter(!!sym(classified_value_var) == unique(data[[classified_value_var]])[i]),
                           aes(label = round(!!sym(value_var), digits = 2)),
                           y = i,
                           size = text_size)
  }

  p <- p + geom_textpath(aes(label = !!sym(classified_text_var), y = factor_levels_count + 1),
                         size = text_size,
                         parse = TRUE)

  return(p)
}
