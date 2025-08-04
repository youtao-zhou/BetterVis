#' BetterVis Boxplot MixViolin Function
#'
#' Creates an enhanced mixed visualization combining violin plots, boxplots, and scatter points with extensive customization options.
#' The function allows users to apply different themes, customize axes, add statistical comparisons, and define aesthetic properties.
#'
#' @name BetterVis_Boxplot_MixViolin
#' @description Generates a violin-boxplot hybrid visualization with extensive customization options, statistical comparisons, and color controls.
#'
#' @param data A data frame containing the variables for plotting.
#' @param x_var A string specifying the variable for the x-axis (should be a factor variable).
#' @param y_var A string specifying the numeric variable for the y-axis.
#' @param fill_color A vector of colors corresponding to each level of the x-axis variable.
#' @param y_max_adjust Numeric. Adjusts the upper limit of the y-axis (default: \code{1}).
#' @param y_min_adjust Numeric. Adjusts the lower limit of the y-axis (default: \code{1}).
#' @param title Character. The main title of the plot (default: \code{""}).
#' @param title_size Numeric. Defines the font size of the title (default: \code{16}).
#' @param title_bold Logical. Defines whether the title should be bold (default: \code{FALSE}).
#' @param title_hjust Numeric. Adjusts the horizontal alignment of the title (default: \code{0.5}).
#' @param title_vjust Numeric. Adjusts the vertical alignment of the title (default: \code{1}).
#' @param xlab Character. The label for the x-axis.
#' @param ylab Character. The label for the y-axis.
#' @param border Logical. Whether to display a border around the plot (default: \code{TRUE}).
#' @param panel_background_color Character. Color of the panel background (default: \code{"#f3f6f6"}).
#' @param axis_x_title A \code{ggplot2::element_text} object specifying x-axis title styling.
#' @param axis_y_title A \code{ggplot2::element_text} object specifying y-axis title styling.
#' @param legend_show Logical. Whether to show the legend (default: \code{TRUE}).
#' @param legend_title Character. Title for the legend (default: \code{NULL}).
#' @param legend_title_size Numeric. Size of the legend title text (default: \code{15}).
#' @param legend_title_bold Logical. Whether the legend title should be bold (default: \code{FALSE}).
#' @param legend_text_size Numeric. Size of the legend text (default: \code{12}).
#' @param legend_text_bold Logical. Whether the legend text should be bold (default: \code{FALSE}).
#' @param stat_compare_means Logical. Whether to add statistical comparisons (default: \code{TRUE}).
#' @param compare_symbol Character. Whether to display comparison results as symbols ("SYMBOL") or numeric p-values ("NUMBER") (default: \code{"SYMBOL"}).
#' @param ... Additional arguments passed to \code{ggplot2} layers.
#'
#' @return A \code{ggplot} object representing the customized violin-boxplot visualization.
#'
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot geom_point
#' @importFrom ggplot2 scale_y_continuous scale_fill_manual scale_color_manual
#' @importFrom ggplot2 labs theme_minimal theme element_text element_line element_rect
#' @importFrom dplyr filter summarise group_by
#' @importFrom ggpubr stat_compare_means compare_means
#'
#' @examples
#' # Load necessary libraries
#' library(ggpubr)
#' library(ggplot2)
#' library(RColorBrewer)
#' library(dplyr)
#' library(rstatix)
#' library(BetterVis)
#'
#' data("BetterVis_Boxplot_MixViolin_example", package = "BetterVis")
#' iris <- BetterVis_Boxplot_MixViolin_example
#'
#' colors <- c("#e97257", "#72c5d9", "#58ae9a", "#646e9a", "#efa78e",
#'             "#f3a9c4", "#ffbf69", "#b4d33e", "#ed7d31", "#6b83b3",
#'             "#b7d3ff", "#ff72cc", "#d4a4ff")
#'
#' # Example 1: Simple visualization
#' BetterVis_Boxplot_MixViolin(data = iris, x_var = "Species", y_var = "Sepal.Length", fill_color = colors,
#' box_size = 0.5, y_max_adjust = 2, y_min_adjust = 1)
#'
#' # Example 2: Customized visualization with titles and axes
#' BetterVis_Boxplot_MixViolin(
#'   data = iris, x_var = "Species", y_var = "Sepal.Length", fill_color = colors,
#'   title = "",title_size = 25,title_bold = FALSE, title_hjust = 0.5,title_vjust = 0,
#'   xlab = "Species", ylab = "Sepal Length",
#'   axis_x_title = element_text(size = 22, color = "#b35959", face = "bold"),
#'   axis_y_title = element_text(size = 22, color = "#b35959", face = "bold"),
#'   legend_show = TRUE, legend_title = "Species", legend_title_size = 15,
#'   stat_compare_means = TRUE, compare_symbol = "SYMBOL"
#' )
#'
#'
#'BetterVis_Boxplot_MixViolin(
#'   data = iris,x_var = "Group2",y_var = "Sepal.Length",fill_color = colors,
#'   y_max_adjust = 2,y_min_adjust = 1,box_size = 0.5,
#'   title = "You can write title here",title_size = 25,title_bold = FALSE, title_hjust = 0.5,title_vjust = 0,
#'   xlab = "Group2",ylab = "Sepal.Length",
#'   border = TRUE,panel_background_color = "#f3f6f6",
#'   axis_x_title =element_text(size = 22,color = "#b35959",face = "bold"),
#'   axis_y_title =element_text(size = 22,color = "#b35959",face = "bold"),
#'   legend_show = TRUE,legend_title = "Group2",legend_title_size = 15,legend_title_bold = FALSE,
#'   legend_text_size=12,legend_text_bold = FALSE,stat_compare_means = TRUE,compare_symbol = "SYMBOL")
#'
#'
#'
#'
#'
#' @export

BetterVis_Boxplot_MixViolin <- function(data, x_var, y_var, fill_color,  box_size=0.5,title = "", title_size = 16, title_bold = FALSE,
                                        title_hjust = 0.5, title_vjust = 1, axis_x_title = element_text(size = 22, color = "#b35959", face = "bold"),
                                        axis_y_title = element_text(size = 22, color = "#b35959", face = "bold"),
                                        xlab = NULL, ylab = NULL, border = TRUE, panel_background_color = "#f3f6f6",
                                        legend_show = TRUE, legend_title = NULL, legend_title_size = 15, legend_title_bold = FALSE,
                                        legend_text_size = 12, legend_text_bold = FALSE, stat_compare_means = TRUE, compare_symbol = "SYMBOL",
                                        y_max_adjust = 1, y_min_adjust = 1) {

  y_max <- max(data[[y_var]], na.rm = TRUE) + y_max_adjust
  y_min <- min(data[[y_var]], na.rm = TRUE) - y_min_adjust

  group_stats <- data %>%
    group_by(.data[[x_var]]) %>%
    summarise(
      n = n(),
      var = var(.data[[y_var]], na.rm = TRUE)
    )

  valid_groups <- group_stats %>%
    filter(n > 1 & var > 0)

  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_violin(aes(fill = .data[[x_var]]), color = NA,
                alpha = 0.6, width = 0.7,
                trim = TRUE, scale = "width") +
    geom_point(aes(color = .data[[x_var]], fill = .data[[x_var]]),
               show.legend = legend_show,
               position = position_jitter(seed = 123456, width = 0.2),
               shape = 21, size = 4) +
    geom_boxplot(aes(fill = .data[[x_var]]),
                 width = 0.7, size = box_size,
                 alpha = 0.6, outlier.shape = NA) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(y_min, y_max),
                       breaks = seq(floor(y_min), ceiling(y_max), 1)) +
    scale_fill_manual(values = fill_color) +
    scale_color_manual(values = fill_color) +
    labs(title = title,
         x = xlab,
         y = ylab) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = title_size, face = ifelse(title_bold, "bold", "plain"), hjust = title_hjust),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      axis.title.x = axis_x_title,
      axis.title.y = axis_y_title,
      axis.line = element_line(size = 1),
      panel.border = if (border) element_rect(fill = NA, color = "black", size = 2, linetype = "solid") else element_blank(),
      panel.background = element_rect(fill = panel_background_color),
      legend.title = element_text(size = legend_title_size, face = ifelse(legend_title_bold, "bold", "plain")),
      legend.text = element_text(size = legend_text_size, face = ifelse(legend_text_bold, "bold", "plain"))
    ) +
    guides(fill = ifelse(legend_show, "legend", "none"),
           color = ifelse(legend_show, "legend", "none"))

  if (stat_compare_means && nrow(valid_groups) >= 2) {
    results <- compare_means(as.formula(paste(y_var, "~", x_var)), data = data, method = "t.test")
    significant_results <- results %>%
      filter(p < 0.05)

    if (nrow(significant_results) > 0) {
      significant_comparisons <- list()
      for (i in 1:nrow(significant_results)) {
        significant_comparisons[[i]] <- c(as.character(significant_results$group1[i]),
                                          as.character(significant_results$group2[i]))
      }

      if (compare_symbol == "SYMBOL") {
        p <- p + stat_compare_means(comparisons = significant_comparisons,
                                    method = "t.test",
                                    label = "p.signif",
                                    size = 5,
                                    color = "black")
      } else if (compare_symbol == "NUMBER") {
        p <- p + stat_compare_means(comparisons = significant_comparisons,
                                    method = "t.test",
                                    label = "p.format",
                                    size = 5,
                                    color = "black")
      }
    }
  }

  if (!is.null(legend_title)) {
    p <- p + labs(fill = legend_title, color = legend_title)
  }
  return(p)
}
