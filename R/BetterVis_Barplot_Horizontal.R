#' Horizontal Barplot Visualization Function
#'
#' This function generates horizontal bar plots with extensive customization options for controlling bar widths, label positions, color gradients, and legends.
#'
#' @name BetterVis_Barplot_Horizontal
#' @description Creates horizontal bar plots that allow for rich customization of visual elements, such as label positioning, color gradients, and legend design, enabling detailed and informative visual representations of data.
#'
#' @param data A data frame containing the variables for plotting.
#' @param x_var A string specifying the variable for the x-axis, which should be a factor.
#' @param y_var A string specifying the numeric variable for the y-axis.
#' @param fill_var A string specifying the variable used for fill colors.
#' @param label_y_var A string representing the column for y-axis labels.
#' @param label_other_var A string representing the column for additional labels within bars.
#' @param bar_width Numeric, specifies the width of the bars (default: \code{0.5}).
#' @param base_size Numeric, controls the scaling of the entire plot (default: \code{35}).
#' @param label_y_size Numeric, sets the size of the y-axis labels (default: \code{4.5}).
#' @param label_y_hjust Numeric, horizontal adjustment for y-axis labels (default: \code{0}).
#' @param label_y_vjust Numeric, vertical adjustment for y-axis labels (default: \code{0}).
#' @param label_other_size Numeric, size of additional labels (default: \code{4}).
#' @param label_other_hjust Numeric, horizontal adjustment for additional labels (default: \code{0}).
#' @param label_other_vjust Numeric, vertical adjustment for additional labels (default: \code{2.5}).
#' @param label_y_start Numeric, starting position for y-axis labels (default: \code{0.5}).
#' @param label_other_start Numeric, starting position for additional labels (default: \code{0.5}).
#' @param fill_gradient A list with "low", "mid", "high", "midpoint" for fill color gradient.
#' @param color_gradient A list with "low", "mid", "high", "midpoint" for text color gradient.
#' @param fill_legend Logical, determines if the legend for fill_var should be shown (default: \code{TRUE}).
#' @param color_legend Logical, determines if the legend for label_other_var should be shown (default: \code{FALSE}).
#' @param fill_legend_title The title for the fill legend.
#' @param color_legend_title The title for the color legend.
#' @param legend_title_size Numeric, size of the legend titles (default: \code{15}).
#' @param legend_text_size Numeric, size of the legend text (default: \code{15}).
#' @param xlab_text The label for the x-axis.
#' @param ylab_text The label for the y-axis.
#' @param plot_title The title for the plot.
#' @param y_label_out Logical, if \code{TRUE}, y-axis labels are placed outside (default: \code{FALSE}).
#' @param label_other_display Logical, if \code{TRUE}, additional labels are displayed inside bars (default: \code{TRUE}).
#'
#' @return A \code{ggplot} object presenting the horizontal barplot visualization.
#' @importFrom ggplot2 ggplot aes_string geom_bar coord_flip theme_test theme element_text scale_fill_gradient2 scale_color_gradient2 xlab ylab labs guides
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' ##  示例数据1
#' data("BetterVis_Barplot_Horizontal_example", package = "BetterVis")
#' df <- BetterVis_Barplot_Horizontal_example
#' df$LogP <- -log10(as.numeric(df$Bonferroni))
#' df<-arrange(df,Bonferroni)
#' df$Description <- sub(".*:", "", df$Description)
#' df$Description <- factor(df$Description, levels = df$Description[order(df$LogP)])
#'
#'
#' BetterVis_Barplot_Horizontal(
#'   data = df,x_var = "Description",y_var = "Count",fill_var = "LogP",
#'   label_y_var = "Description",label_other_var = "Genes",
#'   bar_width = 0.4,base_size = 30,
#'   label_y_start = 0.5,label_y_size = 5,label_y_hjust = 0,label_y_vjust = 1,
#'   label_other_start = 0.5,label_other_size = 4.5,label_other_hjust = 0,label_other_vjust = 2.4,
#'   fill_gradient = list(low = "#486b98", mid = "#f5f2b1", high = "#b93735", midpoint = 3),
#'   color_gradient = list(low = "#486b98", mid = "#f5f2b1", high = "#b93735", midpoint = 3),
#'   fill_legend = TRUE, fill_legend_title="logp",color_legend = FALSE, color_legend_title = "col_lengend",
#'   legend_title_size = 16, legend_text_size = 16,
#'   xlab_text = " Names",ylab_text = "Gene Count", plot_title = "KEGG Enrichment Analysis",
#'   y_label_out = TRUE,label_other_display = TRUE
#' )
#' ##+theme(axis.title.x = element_blank())
#' @export
#'
BetterVis_Barplot_Horizontal <- function(data, x_var, y_var, fill_var, label_y_var, label_other_var,
                                         bar_width = 0.5, base_size = 35,
                                         label_y_size = 4.5, label_y_hjust = 0, label_y_vjust = 0,
                                         label_other_size = 4, label_other_hjust = 0, label_other_vjust = 2.5,
                                         label_y_start = 0.5, label_other_start = 0.5,
                                         fill_gradient = list(low = "#486b98", mid = "f5f2b1", high = "#b93735", midpoint = 3),
                                         color_gradient = list(low = "#486b98", mid = "#f5f2b1", high = "#b93735", midpoint = 3),
                                         fill_legend = TRUE, color_legend = FALSE,
                                         fill_legend_title, color_legend_title, legend_title_size = 15, legend_text_size = 15,
                                         xlab_text = "", ylab_text = "", plot_title = "",
                                         y_label_out = FALSE,label_other_display=TRUE) {
  library(ggplot2)

  p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_bar(stat = "identity", position = "dodge", width = bar_width) +
    coord_flip() +
    theme_test(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, vjust = -0.2),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text = element_text(size = 15, face = "bold"),
      legend.title = element_text(size = legend_title_size, face = "bold"),
      legend.text = element_text(size = legend_text_size)
    ) +
    scale_fill_gradient2(low = fill_gradient$low, mid = fill_gradient$mid, high = fill_gradient$high, midpoint = as.numeric(fill_gradient$midpoint)) +
    scale_color_gradient2(low = color_gradient$low, mid = color_gradient$mid, high = color_gradient$high, midpoint = as.numeric(color_gradient$midpoint)) +
    xlab(xlab_text) +
    ylab(ylab_text) +
    labs(title = plot_title) +
    guides(
      fill = if (fill_legend) guide_colorbar(title = fill_legend_title) else "none",
      color = if (color_legend) guide_colorbar(title = color_legend_title) else "none"
    )

  if (!y_label_out) {
    p <- p + theme(axis.text.y = element_blank()) +
      geom_text(aes_string(x = x_var, y = label_y_start, label = label_y_var), show.legend = fill_legend,
                size = label_y_size, hjust = label_y_hjust, vjust = label_y_vjust)
  }
  if(label_other_display){
    p <- p + geom_text(aes_string(x = x_var, y = label_other_start, label = label_other_var, color = fill_var), show.legend = color_legend,
                       size = label_other_size, hjust = label_other_hjust, vjust = label_other_vjust)
  }

  return(p)
}

