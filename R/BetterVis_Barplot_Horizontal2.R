#' Create an Advanced Horizontal Bar Plot
#'
#' This function generates a highly customizable horizontal bar plot using `ggplot2`.
#' It supports both continuous and categorical fill colors, in-bar text labels,
#' and a secondary categorical annotation track at the base of the plot.
#'
#' @name BetterVis_Barplot_Horizontal2
#'
#' @param data A data frame containing the data for plotting.
#' @param x_var A character string for the column name of the categorical variable for the y-axis (will be flipped).
#' @param y_var A character string for the column name of the numeric variable for bar length (x-axis).
#' @param fill_var A character string for the column name of the variable to map to the bar fill color. Can be numeric or categorical.
#' @param label_y_var A character string for the column name to use as bar labels (typically the same as `x_var`).
#' @param label_other_var (Optional) A character string for an additional in-bar text label.
#' @param Category_Var (Optional) A character string for a column name to create a categorical annotation track.
#' @param fill_var_color (Optional) A named vector of colors for a categorical `fill_var`.
#' @param category_var_color (Optional) A named vector of colors for the `Category_Var` annotation track.
#' @param fill_gradient (Optional) A list with `low`, `mid`, `high`, `midpoint` for a continuous `fill_var` gradient.
#' @param ... Additional parameters for detailed customization of text, legends, and layout (see function definition for full list).
#'
#' @return A `ggplot` object.
#'
#' @importFrom ggplot2 ggplot aes_string geom_bar coord_flip theme_test theme element_text element_blank element_line xlab ylab labs scale_fill_manual guide_legend scale_fill_gradient2 guide_colorbar scale_color_gradient2 scale_color_manual geom_text geom_rect geom_point scale_size_area
#' @importFrom dplyr %>% mutate row_number group_by summarise filter arrange
#' @importFrom ggnewscale new_scale_color new_scale_fill
#' @importFrom scales hue_pal
#' @importFrom rlang sym
#' @importFrom stats sd
#'
#' @export
#'
#' @examples
#' if (requireNamespace("ggnewscale", quietly = TRUE)) {
#'   library(ggplot2)
#'   library(dplyr)
#'   # Load and process the example data
#'   data("BetterVis_Barplot_Horizontal2_example", package = "BetterVis")
#'   df <- BetterVis_Barplot_Horizontal2_example
#'
#'   # Example 1: Continuous fill with a categorical annotation track
#'   BetterVis_Barplot_Horizontal2(
#'     data = df, x_var = "Description", y_var = "Count", fill_var = "LogP",
#'     label_y_var = "Description", label_other_var = "Genes",
#'     fill_gradient = list(low = "#486b98", mid = "#f5f2b1", high = "#b93735", midpoint = 3),
#'     y_label_out = FALSE, label_other_display = TRUE,
#'     Category_Var = "Category2",
#'     category_var_color = c("Group1" = "#c3e1e6", "Group2" = "#f3dfb7", "Group3" = "#dcc6dc"),
#'     annot_y_offset = -1, annot_y_width = 0.5
#'   )
#'
#'   # Example 2: Categorical fill with additional geom_point layers
#'   BetterVis_Barplot_Horizontal2(
#'     data = df, x_var = "Description", y_var = "LogP", fill_var = "Category2",
#'     fill_var_color = c("Group1" = "#c3e1e6", "Group2" = "#f3dfb7", "Group3" = "#dcc6dc"),
#'     label_y_var = "Description", label_other_var = "Genes",
#'     y_label_out = FALSE,
#'     Category_Var = "Category2",
#'     annot_y_offset = -0.5, annot_y_width = 0.2, fill_legend = TRUE,
#'     fill_legend_title = "Group", ylab_text = "LogP"
#'   ) +
#'   geom_point(aes(y = -0.2, size = Count, fill = Category2), shape = 21) +
#'   scale_size_area(max_size = 15) +
#'   geom_text(aes(y = -0.2, label = Count), color = "black", size = 5)
#' }
BetterVis_Barplot_Horizontal2 <- function(data,
                                          x_var, y_var, fill_var, label_y_var, label_other_var = NULL,
                                          bar_width = 0.5, base_size = 35,
                                          label_y_start = 0.5, label_y_size = 4.5, label_y_hjust = 0, label_y_vjust = 0,
                                          label_other_start = 0.5, label_other_size = 4, label_other_hjust = 0, label_other_vjust = 2.5,
                                          fill_gradient = NULL,
                                          color_gradient = NULL,
                                          xlab_text = "", ylab_text = "", plot_title = "",
                                          y_label_out = FALSE, label_other_display = TRUE,
                                          Category_Var = NULL,
                                          category_var_color = NULL,
                                          fill_var_color = NULL,
                                          fill_legend = TRUE,
                                          fill_legend_title = "logp",
                                          color_legend = FALSE,
                                          color_legend_title = "col_lengend",
                                          legend_title_size = 16,
                                          legend_text_size = 16,
                                          annot_y_offset = -0.5,
                                          annot_y_width = 0.2,
                                          category_text_angle = 90,
                                          category_text_display = TRUE) {

  if (is.factor(data[[fill_var]]) || is.character(data[[fill_var]])) {
    data[[fill_var]] <- as.factor(data[[fill_var]])
    fill_var_is_cat <- TRUE
  } else {
    fill_var_is_cat <- FALSE
  }

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
      legend.text = element_text(size = legend_text_size),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    xlab(xlab_text) +
    ylab(ylab_text) +
    labs(title = plot_title)

  if (fill_var_is_cat) {
    if (is.null(fill_var_color)) {
      fill_var_color <- scales::hue_pal()(length(levels(data[[fill_var]])))
      names(fill_var_color) <- levels(data[[fill_var]])
    } else {
      # Ensure colors are named if not already
      if (is.null(names(fill_var_color))) {
        names(fill_var_color) <- levels(data[[fill_var]])
      }
    }
    p <- p + scale_fill_manual(values = fill_var_color,
                               guide = if (fill_legend) guide_legend(title = fill_legend_title) else "none")
  } else {
    if (is.null(fill_gradient)) {
      fill_gradient <- list(low = "#486b98", mid = "#f5f2b1", high = "#b93735", midpoint = 3)
    }
    p <- p + scale_fill_gradient2(
      low = fill_gradient$low, mid = fill_gradient$mid, high = fill_gradient$high,
      midpoint = as.numeric(fill_gradient$midpoint),
      guide = if (fill_legend) guide_colorbar(title = fill_legend_title) else "none"
    )
  }

  if (!fill_var_is_cat) {
    if (is.null(color_gradient)) {
      color_gradient <- list(low = "#486b98", mid = "#f5f2b1", high = "#b93735", midpoint = 3)
    }
    p <- p + scale_color_gradient2(
      low = color_gradient$low, mid = color_gradient$mid, high = color_gradient$high,
      midpoint = as.numeric(color_gradient$midpoint),
      guide = "none"
    )
  }

  if (!y_label_out) {
    p <- p + theme(axis.text.y = element_blank()) +
      geom_text(aes_string(x = x_var, y = label_y_start, label = label_y_var),
                size = label_y_size,
                hjust = label_y_hjust, vjust = label_y_vjust)
  }

  if (!is.null(label_other_var) && label_other_display) {
    if (fill_var_is_cat) {
      p <- p + new_scale_color()
      p <- p + geom_text(aes_string(x = x_var, y = label_other_start, label = label_other_var, color = fill_var),
                         size = label_other_size,
                         hjust = label_other_hjust, vjust = label_other_vjust) +
        scale_color_manual(values = fill_var_color,
                           guide = if (color_legend) guide_legend(title = color_legend_title) else "none")
    } else {
      p <- p + geom_text(aes_string(x = x_var, y = label_other_start, label = label_other_var, color = fill_var),
                         size = label_other_size,
                         hjust = label_other_hjust, vjust = label_other_vjust)
    }
  }

  if (!is.null(Category_Var)) {
    data <- data %>%
      dplyr::mutate(index = dplyr::row_number(),
                    cat = factor(!!sym(Category_Var)))

    rect_data <- data %>%
      group_by(.data$cat) %>%
      dplyr::summarise(group_min = min(.data$index), group_max = max(.data$index), .groups = 'drop') %>%
      dplyr::mutate(
        ymin = annot_y_offset,
        ymax = annot_y_offset + annot_y_width,
        xmin = .data$group_min - 0.5,
        xmax = .data$group_max + 0.5
      )

    if (is.null(category_var_color)) {
      cat_levels <- levels(data$cat)
      category_var_color <- scales::hue_pal()(length(cat_levels))
      names(category_var_color) <- cat_levels
    } else {
      if (is.null(names(category_var_color))) {
        names(category_var_color) <- levels(data$cat)
      }
    }

    p <- p + new_scale_fill()
    p <- p +
      geom_rect(data = rect_data,
                aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax, fill = .data$cat),
                inherit.aes = FALSE, color = "black", alpha = 0.6, show.legend = FALSE)
    if (category_text_display) {
      p <- p + geom_text(data = rect_data,
                         aes(x = (.data$xmin + .data$xmax) / 2, y = (.data$ymin + .data$ymax) / 2, label = .data$cat),
                         inherit.aes = FALSE, size = label_y_size, color = "black", angle = category_text_angle)
    }
    p <- p + scale_fill_manual(values = category_var_color, guide = "none")
  }

  return(p)
}

# This handles "no visible binding for global variable" NOTES from R CMD check.
utils::globalVariables(c("cat", "index", "group_min", "group_max", "xmin", "xmax", "ymin", "ymax"))
