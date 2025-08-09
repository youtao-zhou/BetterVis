#' Create a Scatter Plot with Marginal Density Bars
#'
#' This function generates a scatter plot using `ggplot2` and can overlay it
#' with marginal density/histogram plots via `ggExtra::ggMarginal`. It also
#' calculates and displays group medians with error bars (representing standard deviation).
#'
#' @name BetterVis_Dotplot_Mixbar
#'
#' @param df A data frame containing the data for plotting.
#' @param x_var A character string specifying the column name for the continuous x-axis variable.
#' @param y_var A character string specifying the column name for the continuous y-axis variable.
#' @param classified_var A character string specifying the column name for the categorical variable used for coloring points.
#' @param size_var (Optional) A character string for a numeric column to be mapped to point size.
#' @param color A vector of colors for the different levels of `classified_var`.
#' @param point_shape Numeric. The shape of the points. Default is `21`.
#' @param point_size Numeric. The base size of the points. Default is `3`.
#' @param error_bar Logical. If `TRUE`, median points with SD error bars are displayed for each group. Default is `TRUE`.
#' @param errorbar_width Numeric. The width of the error bars. Default is `0.5`.
#' @param errorbar_size Numeric. The line thickness of the error bars. Default is `0.8`.
#' @param legend Logical. If `TRUE`, the color legend is displayed. Default is `FALSE`.
#' @param title,x_title,y_title Character strings for plot titles. Default is `NULL`.
#' @param legend_title Character string for the legend title. Default is `NULL`.
#' @param theme_custom (Optional) A `theme()` object to apply custom theme settings.
#' @param guides_custom (Optional) A `guides()` object to apply custom guide settings.
#' @param Mixbar Logical. If `TRUE`, adds marginal density plots to the scatter plot. Default is `TRUE`.
#'
#' @return A `ggExtraPlot` object if `Mixbar = TRUE`, otherwise a `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar scale_color_manual scale_fill_manual guides theme_classic theme element_text ggtitle xlab ylab labs theme
#' @importFrom dplyr %>% group_by summarize across full_join
#' @importFrom rlang ensym sym
#' @importFrom ggExtra ggMarginal
#' @importFrom stats median sd
#' @importFrom tidyselect where
#'
#' @export
#'
#' @examples
#' # Load example data from the package
#' data("BetterVis_Dotplot_Mixbar_example", package = "BetterVis")
#' library(ggplot2)
#' # Example 1: Basic plot with marginal density bars
#' BetterVis_Dotplot_Mixbar(df = BetterVis_Dotplot_Mixbar_example, x_var = "Tumor_Size_mm", y_var = "Biomarker_Level", classified_var = "Cancer_Type", size_var = "Age",
#'                          color =  c("#0D6B9C", "#953594", "#38A498") ,Mixbar = TRUE,error_bar = TRUE,
#'                          x_title = "Tumor Size mm", y_title = "Biomarker Level")
#' BetterVis_Dotplot_Mixbar(df = BetterVis_Dotplot_Mixbar_example, x_var = "Tumor_Size_mm", y_var = "Biomarker_Level",
#'                          classified_var = "Cancer_Type", size_var = "Age",color =  c("#0D6B9C", "#953594", "#38A498"),
#'                          point_shape = 16, point_size = 4,
#'                          error_bar = TRUE ,errorbar_width = 0.6, errorbar_size = 1,
#'                          legend = FALSE, title = "", x_title = "Tumor Size mm",
#'                          y_title = "Biomarker Level" , legend_title = "Species",
#'                          theme_custom =theme(axis.title.x = element_text(size = 15)),
#'                          guides_custom=guides(color="none",fill="none"),Mixbar = TRUE)
#'
#'
#'
BetterVis_Dotplot_Mixbar <- function(df, x_var, y_var, classified_var, size_var = NULL,
                                     color = c("#FBA72A", "#78B7C5", "#7294D4"),
                                     point_shape = 21, point_size = 3, errorbar_width = 0.5,
                                     errorbar_size = 0.8, legend = FALSE, title = NULL,
                                     x_title = NULL, y_title = NULL, legend_title = NULL,
                                     theme_custom = NULL, guides_custom = NULL, error_bar = TRUE,
                                     Mixbar = TRUE) {

  classified_var_sym <- ensym(classified_var)
  x_var_sym <- ensym(x_var)
  y_var_sym <- ensym(y_var)

  p <- ggplot(df, aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(classified_var), fill = !!sym(classified_var))) +
    geom_point(shape = point_shape, size = point_size, alpha = 0.3)

  if (!is.null(size_var)) {
    p <- p + geom_point(aes(size = !!sym(size_var)), shape = point_shape, alpha = 0.3)
  }

  df_summary_stats <- df %>%
    group_by(!!classified_var_sym) %>%
    summarize(across(where(is.numeric),
                     list(median = ~median(.x, na.rm = TRUE),
                          sd = ~sd(.x, na.rm = TRUE))),
              .groups = 'drop')

  x_var_median <- paste0(x_var, "_median")
  x_var_sd <- paste0(x_var, "_sd")
  y_var_median <- paste0(y_var, "_median")
  y_var_sd <- paste0(y_var, "_sd")

  if (error_bar) {
    p <- p +
      geom_errorbar(data = df_summary_stats,
                    aes(x = !!sym(x_var_median),
                        ymin = !!sym(y_var_median) - !!sym(y_var_sd),
                        ymax = !!sym(y_var_median) + !!sym(y_var_sd),
                        color = !!sym(classified_var)),
                    show.legend = legend,
                    inherit.aes = FALSE,
                    width = errorbar_width, size = errorbar_size) +
      geom_errorbar(data = df_summary_stats,
                    aes(y = !!sym(y_var_median),
                        xmin = !!sym(x_var_median) - !!sym(x_var_sd),
                        xmax = !!sym(x_var_median) + !!sym(x_var_sd),
                        color = !!sym(classified_var)),
                    show.legend = legend,
                    inherit.aes = FALSE,
                    width = errorbar_width, size = errorbar_size)
  }

  p <- p +
    geom_point(data = df_summary_stats,
               aes(x = !!sym(x_var_median), y = !!sym(y_var_median)),
               size = 6, pch = 21, fill = "white") +
    scale_color_manual(values = color[1:length(unique(df[[classified_var]]))]) +
    scale_fill_manual(values = color[1:length(unique(df[[classified_var]]))]) +
    guides(colour = if (legend) "legend" else "none", size = "none", fill = "none") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 15, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(size = 14, face = "bold"),
          legend.text = element_text(size = 14, face = "bold"))

  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

  p <- p + xlab(x_title) + ylab(y_title)

  if (!is.null(legend_title)) {
    p <- p + labs(color = legend_title)
  }

  if (!is.null(theme_custom)) {
    p <- p + theme_custom
  }

  if (!is.null(guides_custom)) {
    p <- p + guides_custom
  }

  if (Mixbar) {
    p <- ggMarginal(p, type = "densigram", groupColour = TRUE, groupFill = TRUE, alpha = 0.5)
  }

  return(p)
}
