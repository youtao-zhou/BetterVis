#' Create a Multi-Faceted Scatter Plot Matrix
#'
#' This function generates a matrix of scatter plots with marginal density plots
#' for all pairwise combinations of numeric variables in a dataframe. The points are
#' colored by a specified categorical variable.
#'
#' @name BetterVis_Dotplot_Multifacet
#'
#' @param df A data frame containing the data for plotting. The function will use all numeric columns.
#' @param classified_var A character string specifying the column name for the categorical variable used for coloring points.
#' @param color A vector of colors for the different levels of `classified_var`.
#' @param ncol Numeric. The number of columns in the final plot grid. Default is `3`.
#' @param subset_display (Optional) A numeric vector of indices to select a subset of pairwise plots to display.
#' @param strokeSize Numeric. The size of the point borders. Default is `0.2`.
#' @param pointSize Numeric. The size of the points. Default is `3`.
#' @param strokeColor Character string. The color of the point borders. Default is `'gray30'`.
#' @param alpha Numeric. The transparency of the points (0-1). Default is `0.6`.
#' @param legend_title,legend_title_size,legend_text_size Parameters for the legend appearance.
#' @param x_text_angle,x_text_size,y_text_size,x_title_size,y_title_size Parameters for axis text and title appearance.
#' @param theme_custom (Optional) A `theme()` object to apply custom theme settings to each subplot.
#' @param guides_custom (Optional) A `guides()` object to apply custom guide settings to each subplot.
#'
#' @return A `cowplot` object (grob) representing the final plot grid.
#'
#' @importFrom ggplot2 ggplot aes xlab ylab geom_point scale_x_continuous scale_y_continuous theme element_blank element_line element_text scale_fill_manual geom_density coord_flip
#' @importFrom cowplot axis_canvas insert_xaxis_grob insert_yaxis_grob ggdraw get_legend plot_grid
#' @importFrom grid unit
#' @importFrom scales pretty_breaks
#' @importFrom utils combn
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ggpubr)
#' library(cowplot)
#' library(grid)
#' library(gridExtra)
#' library(MASS)
#'
#' # Example 1: Using the iris dataset (as `BetterVis_Dotplot_Multifacet_example`)
#' data("BetterVis_Dotplot_Multifacet_example", package = "BetterVis")
#' BetterVis_Dotplot_Multifacet(
#'   df = BetterVis_Dotplot_Multifacet_example,
#'   classified_var = "Species",
#'   color = c("#BEBADA", "#80B1D3", "#FDB462","#FB8072"),
#'   ncol = 3
#' )
#'
#' # Example 2: Using the Cars93 dataset from the MASS package
#' if (requireNamespace("MASS", quietly = TRUE) && requireNamespace("dplyr", quietly = TRUE)) {
#'   Cars93 <- MASS::Cars93 %>% dplyr::select(Type, Min.Price:MPG.highway)
#'   BetterVis_Dotplot_Multifacet(
#'     df = Cars93,
#'     classified_var = "Type",
#'     color = c("#BEBADA", "#80B1D3", "#FDB462", "#FB8072", "#8DD3C7", "#BC80BD"),
#'     ncol = 5,
#'     x_text_angle = 45
#'    )
#' }
#'
#' # Example 3: Subsetting the plots and adding custom themes
#' BetterVis_Dotplot_Multifacet(
#'   df = BetterVis_Dotplot_Multifacet_example,
#'   classified_var = "Species",
#'   color = c("#BEBADA", "#80B1D3", "#FDB462","#FB8072"),
#'   ncol = 3,
#'   # subset_display = c(1, 2, 4), # Display only the 1st, 2nd, and 4th plot
#'   legend_title = "Penguin Species",
#'   theme_custom = theme(axis.text = element_text(face = "bold"))
#' )
BetterVis_Dotplot_Multifacet <- function(
    df,
    classified_var,
    color = c("#BEBADA", "#80B1D3", "#FDB462","#FB8072"),
    ncol = 3 ,
    strokeSize = 0.2,
    pointSize = 3,
    strokeColor = 'gray30',
    alpha = 0.6,
    legend_title = "",
    legend_title_size = 14,
    legend_text_size = 12,
    x_text_angle = 0,
    x_text_size = 12,
    y_text_size = 12,
    x_title_size = 14,
    y_title_size = 14,
    theme_custom = NULL,
    guides_custom = NULL,
    subset_display = NULL
) {
  if (!(classified_var %in% colnames(df))) {
    stop("classified_var must be a column in the data frame.")
  }

  numeric_columns <- df[, sapply(df, is.numeric)]
  group <- df[[classified_var]]

  colnames_x_y <- colnames(numeric_columns)

  pair.pcs <- utils::combn(ncol(numeric_columns), 2, simplify = FALSE)

  if (!is.null(subset_display)) {
    if (any(subset_display > length(pair.pcs) | subset_display < 1)) {
      stop("Some of the specified indices in subset_display are out of bounds.")
    }
    pair.pcs <- pair.pcs[subset_display]
  }

  pList <- list()

  # Define p outside the loop to get the legend later
  p <- ggplot()

  for (i in 1:length(pair.pcs)) {
    x_index <- pair.pcs[[i]][1]
    y_index <- pair.pcs[[i]][2]

    x_col <- colnames_x_y[x_index]
    y_col <- colnames_x_y[y_index]

    p <- ggplot(mapping = aes(
      x = df[[x_col]],
      y = df[[y_col]],
      fill = group)) +
      xlab(x_col) +
      ylab(y_col) +
      geom_point(
        aes(fill = group),
        pch = 21,
        color = strokeColor,
        stroke = strokeSize,
        size = pointSize,
        alpha = alpha
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 1.1),
        axis.text.x = element_text(size = x_text_size, angle = x_text_angle, hjust = 1),
        axis.text.y = element_text(size = y_text_size),
        axis.title.x = element_text(size = x_title_size, face = "bold"),
        axis.title.y = element_text(size = y_title_size, face = "bold"),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = legend_title_size, face = "bold"),
        legend.text = element_text(size = legend_text_size)
      ) +
      scale_fill_manual(name = legend_title, values = color)

    if (!is.null(theme_custom)) p <- p + theme_custom
    if (!is.null(guides_custom)) p <- p + guides_custom

    xdens <- cowplot::axis_canvas(p, axis = "x") +
      geom_density(
        mapping = aes(x = df[[x_col]], fill = group),
        alpha = 0.7, size = 0.2
      ) +
      theme(legend.position = "none") +
      scale_fill_manual(values = color)

    ydens <- cowplot::axis_canvas(p, axis = "y", coord_flip = TRUE) +
      geom_density(
        mapping = aes(x = df[[y_col]], fill = group),
        alpha = 0.7, size = 0.2
      ) +
      theme(legend.position = "none") +
      scale_fill_manual(values = color) +
      coord_flip()

    p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
    p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

    pList[[i]] <- ggdraw(p2)
  }

  legend <- cowplot::get_legend(p + theme(legend.position = "right",
                                          legend.title = element_text(size = legend_title_size),
                                          legend.text = element_text(size = legend_text_size)))

  final_plot <- plot_grid(
    plotlist = pList,
    ncol = ncol,
    align = "hv"
  )

  final_layout <- plot_grid(
    final_plot, legend,
    ncol = 2,
    rel_widths = c(0.85, 0.15)
  )

  return(final_layout)
}
