#' Create a Multi-Layered Heatmap with Annotations
#'
#' This function generates a composite plot featuring a main heatmap for numeric
#' data stacked below an annotation heatmap for categorical data. It uses `patchwork`
#' for layout and `ggnewscale` to support multiple color scales.
#'
#' @name BetterVis_Heatmap_Multi
#'
#' @param df A data frame containing the data for plotting.
#' @param x_var A character string specifying the column name for the x-axis variable (e.g., sample IDs).
#' @param number_vars (Optional) A character vector of column names for the numeric variables to include in the main heatmap. If `NULL`, all numeric columns are used.
#' @param annotation_var (Optional) A character vector of column names for the categorical variables to include in the annotation heatmap.
#' @param color_heatmap A vector of colors for the main heatmap's gradient. Defaults to a `MetBrewer` palette.
#' @param color_annotation (Optional) A list of color vectors for the annotation tracks. The list should have the same length as `annotation_var`.
#' @param legend_heatmap_position The position for the main heatmap's legend. Can be a character string (e.g., "right") or a numeric vector `c(x, y)`.
#' @param legend_annotation_position The position for the annotation legend(s).
#' @param heatmap_theme,annotation_theme (Optional) A `theme()` object to apply custom theme settings to the respective plots.
#' @param heatmap_plot_margin A `unit` object specifying the margins for the main heatmap.
#'
#' @return A `patchwork` object.
#'
#' @importFrom ggplot2 ggplot aes sym geom_tile scale_fill_manual scale_y_discrete labs theme element_blank element_text unit scale_fill_gradientn
#' @importFrom dplyr %>% select all_of mutate filter rename
#' @importFrom tidyselect where
#' @importFrom ggnewscale new_scale_fill
#' @importFrom MetBrewer met.brewer
#' @import patchwork
#' @importFrom patchwork plot_layout
#' @importFrom scales brewer_pal
#'
#' @export
#'
#' @examples
#' if (requireNamespace("ggnewscale", quietly = TRUE) &&
#'     requireNamespace("patchwork", quietly = TRUE)) {
#'    library(MetBrewer)
#'   # Load example data
#'   data("BetterVis_Heatmap_Multi_example", package = "BetterVis")
#'
#'   # Define a list of color palettes for the annotations
#'   color_annotation <- list(
#'     c("#E31A1C", "#FDBF6F"),
#'     c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99"),
#'     c("#FF7F00", "#CAB2D6")
#'   )
#'
#'   # Generate the plot
#'   BetterVis_Heatmap_Multi(
#'     df = BetterVis_Heatmap_Multi_example,
#'     x_var = "case.ID",
#'     annotation_var = c("Primary Metastasis", "Consensus classification", "UNC classification"),
#'     color_heatmap = met.brewer("Cassatt1"),
#'     color_annotation = color_annotation,
#'     legend_heatmap_position = c(1.08, 0.7),
#'     legend_annotation_position = c(1.22, -4),
#'     heatmap_theme = theme(axis.text.y = element_text(face = "bold")),
#'     annotation_theme = theme(
#'       axis.text.y = element_text(face = "bold"),
#'       legend.title = element_text(face = "bold"),
#'       legend.text = element_text(face = "bold")
#'     )
#'   )
#' }
#'
BetterVis_Heatmap_Multi <- function(df, x_var, number_vars = NULL, annotation_var = NULL,
                                    color_heatmap = MetBrewer::met.brewer("Cassatt1"),
                                    color_annotation = NULL,
                                    legend_heatmap_position = "right",
                                    legend_annotation_position = c(1.3, -4),
                                    heatmap_theme = theme(),
                                    annotation_theme = theme(),
                                    heatmap_plot_margin = unit(c(0, 2, -20, 0.5), "cm")) {
  x_var <- as.character(x_var)

  if (is.null(number_vars) || length(number_vars) == 0) {
    number_vars <- df %>% select(where(is.numeric)) %>% colnames()
  }

  df_char <- df %>%
    select(where(~ is.character(.) || is.factor(.))) %>%
    pivot_longer(-all_of(x_var), names_to = "name", values_to = "value")

  df_char$sample <- factor(df_char[[x_var]], levels = unique(df_char[[x_var]]))
  df_char$name <- factor(df_char$name, levels = unique(df_char$name) %>% rev())

  if (!is.null(annotation_var)) {
    missing_cols <- setdiff(annotation_var, colnames(df))
    if (length(missing_cols) > 0) {
      stop(paste("Error: The following columns in annotation_var do not exist in the data frame:",
                 paste(missing_cols, collapse = ", ")))
    }

    legend_data <- lapply(annotation_var, function(var) {
      df_char %>%
        filter(.data$name == var) %>%
        dplyr::rename(!!var := .data$value) %>%
        select(all_of(x_var), .data$name, !!var)
    })
  }

  df_long <- df %>%
    pivot_longer(cols = all_of(number_vars), names_to = "variable", values_to = "value") %>%
    mutate(value = log(.data$value))

  p2 <- ggplot(df_long, aes(x = !!sym(x_var), y = .data$variable)) +
    geom_tile(aes(fill = .data$value), color = "black") +
    scale_fill_gradientn(colors = color_heatmap, na.value = NA) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(color = "black"),
      legend.position = legend_heatmap_position,
      legend.direction = "vertical",
      legend.spacing.x = unit(0.1, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      plot.margin = heatmap_plot_margin,
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.text = element_text(color = "black"),
      legend.box.spacing = unit(0.5, "cm")
    ) +
    heatmap_theme

  if (!is.null(annotation_var)) {
    p1 <- ggplot(df_char, aes(x = !!sym(x_var), y = .data$name))

    for (i in seq_along(legend_data)) {
      unique_values <- unique(legend_data[[i]][[annotation_var[i]]])
      num_colors <- length(unique_values)

      if (!is.null(color_annotation) && length(color_annotation) >= i) {
        color_values <- color_annotation[[i]]
      } else {
        color_values <- scales::brewer_pal(palette = "Set3")(num_colors)
      }

      p1 <- p1 +
        geom_tile(data = legend_data[[i]], color = "black", aes(fill = !!sym(annotation_var[i]))) +
        scale_fill_manual(values = setNames(color_values, unique_values), name = annotation_var[i]) +
        new_scale_fill()
    }

    p1 <- p1 +
      scale_y_discrete(position = "right") +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black"),
        legend.position = legend_annotation_position,
        legend.direction = "vertical",
        legend.spacing.x = unit(0.1, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.height = unit(0.5, "cm"),
        legend.key.width = unit(0.5, "cm"),
        plot.margin = unit(c(0, 1, -20, 0.5), "cm"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(color = "black")
      ) +
      annotation_theme

    plot <- p1 / p2 + plot_layout(heights = c(0.3, 2))
    return(plot)
  } else {
    return(p2)
  }
}

# This handles "no visible binding for global variable" NOTES from R CMD check.
utils::globalVariables(c(".", "name", "value", "variable"))
