#' Create a Mantel Test Heatmap
#'
#' This function creates a visualization that combines a correlation heatmap of one
#' dataset with the results of a Mantel test linking it to a second dataset. It
#' heavily relies on the `linkET` package.
#'
#' @name BetterVis_Heatmap_Mantel
#'
#' @param phenotype_data A data frame containing phenotype data (or the first matrix).
#' @param expression_data A data frame or matrix containing expression data (or the second matrix for the heatmap).
#' @param spec_select A character vector of column names from `phenotype_data` to be used in the Mantel test against `expression_data`.
#' @param significant_symbol Logical. If `TRUE`, adds significance asterisks to the correlation heatmap. Default is `TRUE`.
#' @param heatmap_color A vector of colors for the heatmap gradient. Defaults to a palette from `RColorBrewer`.
#' @param line_size A numeric vector of length 3 for the line widths corresponding to Mantel's r categories.
#' @param line_color A vector of 3 colors for the lines corresponding to Mantel's p-value categories.
#' @param phenotype_text_size Numeric. The font size for the labels of the phenotype variables.
#'
#' @return A `ggplot` object.
#'
#' @importFrom linkET mantel_test qcorrplot correlate geom_square geom_couple geom_mark
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 aes scale_fill_gradientn scale_size_manual scale_colour_manual guides guide_legend guide_colorbar theme element_text
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats na.omit setNames
#' @importFrom scales hue_pal
#'
#' @export
#'
#' @examples
#' library(linkET)
#' library(dplyr)
#' library(ggplot2)
#' library(tidyverse)
#' library(RColorBrewer)
#' library(vegan)
#'   # Load example data
#'   data("BetterVis_Heatmap_Mantel_example1", package = "BetterVis")
#'   data("BetterVis_Heatmap_Mantel_example2", package = "BetterVis")
#'
#'   # Example 1: Basic usage
#'   BetterVis_Heatmap_Mantel(
#'     phenotype_data = BetterVis_Heatmap_Mantel_example2,
#'     expression_data = BetterVis_Heatmap_Mantel_example1,
#'     spec_select = c("stage", "age", "gender", "CA125", "CEA", "CA15.3")
#'   )
#'
#'   # Example 2: More customized plot
#'   BetterVis_Heatmap_Mantel(
#'     phenotype_data = BetterVis_Heatmap_Mantel_example2,
#'     expression_data = BetterVis_Heatmap_Mantel_example1,
#'     spec_select = c("stage", "age", "gender", "CA125", "CEA", "CA15.3"),
#'     significant_symbol = TRUE,
#'     heatmap_color = brewer.pal(11, "RdYlBu"),
#'     line_size = c(0.5, 1, 1.5),
#'     line_color = c("blue", "green", "grey"),
#'     phenotype_text_size = 5
#'   ) +
#'   theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18))
BetterVis_Heatmap_Mantel <- function(phenotype_data, expression_data, spec_select,
                                     significant_symbol = TRUE,
                                     heatmap_color = RColorBrewer::brewer.pal(11, "PuOr"),
                                     line_size = c(0.5, 1, 2),
                                     line_color = scales::hue_pal()(3),
                                     phenotype_text_size = 5) {
  if (!is.character(spec_select)) {
    stop("spec_select must be a character vector of column names.")
  }

  col_indices <- match(spec_select, colnames(phenotype_data))

  if (any(is.na(col_indices))) {
    stop("Some column names in spec_select do not exist in phenotype_data.")
  }

  spec_select_list <- setNames(as.list(col_indices), spec_select)

  mantel <- mantel_test(phenotype_data, expression_data, spec_select = spec_select_list) %>%
    mutate(rd = cut(.data$r, breaks = c(-Inf, 0.2, 0.4, Inf),
                    labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
           pd = cut(.data$p, breaks = c(-Inf, 0.01, 0.05, Inf),
                    labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

  mantel <- na.omit(mantel)

  p <- qcorrplot(correlate(expression_data),
                 type = "lower",
                 diag = FALSE) +
    geom_square() +
    geom_couple(aes(colour = .data$pd,
                    size = .data$rd),
                data = mantel,
                curvature = 0.1,
                label.size = phenotype_text_size) +
    scale_fill_gradientn(colours = heatmap_color) +
    scale_size_manual(values = line_size) +
    scale_colour_manual(values = line_color) +
    guides(size = guide_legend(title = "Mantel's r",
                               override.aes = list(colour = "grey35"),
                               order = 2),
           colour = guide_legend(title = "Mantel's p",
                                 override.aes = list(size = 3),
                                 order = 1),
           fill = guide_colorbar(title = "Pearson's r", order = 3)) +
    theme(
      text = element_text(size = 14, family = "serif"),
      plot.title = element_text(size = 14, colour = "pink", hjust = 0.5),
      legend.title = element_text(color = "black", size = 14),
      legend.text = element_text(color = "black", size = 14),
      axis.text.y = element_text(size = 14, color = "black", vjust = 0.5, hjust = 1, angle = 0),
      axis.text.x = element_text(size = 14, color = "black", vjust = 1, hjust = 1, angle = 45) # Adjusted for better look
    )

  if (significant_symbol) {
    p <- p + geom_mark(sep = '\n', size = 3, sig_level = c(0.05, 0.01, 0.001),
                       sig_thres = 0.05, only_mark = TRUE, color = 'black')
  }

  return(p)
}

# This handles the "no visible binding for global variable" NOTE from R CMD check
utils::globalVariables(c("p", "r"))
