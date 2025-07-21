#' Create a Single Flow Sankey Diagram
#'
#' This function generates a Sankey diagram to visualize the flow and transitions
#' of data across multiple categorical variables, using the `ggsankey` package.
#'
#' @name BetterVis_Sankey_Single
#'
#' @param data A data frame where each column represents a stage in the flow.
#' @param sankey_alpha Numeric. The transparency of the Sankey flows (0-1). Default is `0.6`.
#' @param sankey_width Numeric. The width of the nodes (rectangles). Default is `0.3`.
#' @param border_color Character string. The color of the node borders. Default is `"gray30"`.
#' @param sankey_text_size Numeric. The font size for text inside the nodes. Default is `4`.
#' @param sankey_text_hjust Numeric. Horizontal justification of text inside nodes (0-1). Default is `0.5`.
#' @param sankey_text_vjust Numeric. Vertical justification of text inside nodes (0-1). Default is `0.5`.
#' @param sankey_color A vector of character strings specifying the colors for the nodes.
#' @param bottom_text_size Numeric. The font size for the x-axis labels (column names). Default is `18`.
#' @param bottom_text_color Character string. The color for the x-axis labels. Default is `"grey30"`.
#' @param title Character string. The main title of the plot. Default is `NULL` (no title).
#' @param title_size Numeric. The font size for the main title. Default is `18`.
#' @param text_adjust_global A single numeric value to uniformly scale all text elements
#'  (node text, axis text, title). `1` means no change. Default is `1`.
#'
#' @return A `ggplot` object representing the Sankey diagram.
#'
#' @importFrom ggsankey make_long geom_sankey geom_sankey_text theme_sankey
#' @importFrom ggplot2 ggplot aes scale_fill_manual labs theme element_text ggtitle
#' @importFrom dplyr %>% group_by summarise mutate right_join ungroup left_join n
#' @importFrom rlang syms !!!
#'
#' @export
#'
#' @examples
#' # Load example data from the package
#' data("BetterVis_Sankey_Single_example", package = "BetterVis")
#'
#' # Define a color palette
#' sankey_colors <- c("#F3BDA5", "#5FC8D1", "#e377c2", "#D8B4E2", "#1f77b4",
#'                    "#F9D68D", "#FDFD96", "#B7E4C7", "#7FBEEB", "#E0E0E0",
#'                    "#D8B4E2", "#AED9E0", "#68d0ce", "#e09a2a", "#de6a73")
#'
#' # Example 1: Simple usage with default settings
#' BetterVis_Sankey_Single(data = BetterVis_Sankey_Single_example, sankey_color = sankey_colors)
#'
#' # Example 2: More customized plot
#' BetterVis_Sankey_Single(
#'   data = BetterVis_Sankey_Single_example,
#'   sankey_alpha = 0.5,
#'   sankey_width = 0.4,
#'   border_color = "black",
#'   sankey_text_size = 5,
#'   bottom_text_size = 20,
#'   title = "Patient Treatment Flow",
#'   title_size = 22,
#'   text_adjust_global = 0.9,
#'   sankey_color = sankey_colors
#' )
BetterVis_Sankey_Single <- function(data,
                                    sankey_alpha = 0.6,
                                    sankey_width = 0.3,
                                    border_color = "gray30",
                                    sankey_text_size = 4,
                                    sankey_text_hjust = 0.5,
                                    sankey_text_vjust = 0.5,
                                    sankey_color = c("#F3BDA5", "#5FC8D1", "#e377c2", "#D8B4E2",
                                                     "#1f77b4", "#F9D68D", "#FDFD96", "#B7E4C7",
                                                     "#7FBEEB", "#E0E0E0", "#D8B4E2", "#AED9E0"),
                                    bottom_text_size = 18,
                                    bottom_text_color = "grey30",
                                    title = NULL,
                                    title_size = 18,
                                    text_adjust_global = 1) {

  col_names <- names(data)
  df <- data %>%
    make_long(!!!syms(col_names))

  # Calculate counts and percentages for labels
  df_summary <- df %>%
    group_by(.data$x, .data$node) %>%
    summarise(n = n(), .groups = "drop")

  df_with_labels <- df %>%
    left_join(df_summary, by = c("x", "node")) %>%
    group_by(.data$x) %>%
    mutate(pct = .data$n / sum(.data$n)) %>%
    ungroup()

  # Create the plot
  p <- ggplot(df_with_labels, aes(x = .data$x, next_x = .data$next_x, node = .data$node,
                                  next_node = .data$next_node, fill = factor(.data$node),
                                  label = paste0(.data$node, ",\nn=", .data$n, '\n(', round(.data$pct * 100, 1), '%)'))) +
    geom_sankey(flow.alpha = sankey_alpha, width = sankey_width,
                node.color = border_color) +
    geom_sankey_text(size = sankey_text_size * text_adjust_global, color = "black",
                     hjust = sankey_text_hjust, vjust = sankey_text_vjust) +
    scale_fill_manual(values = sankey_color) +
    theme_sankey(base_size = 18) +
    labs(x = NULL) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = .5, size = title_size * text_adjust_global),
          axis.text.x = element_text(size = bottom_text_size * text_adjust_global, color = bottom_text_color)) +
    ggtitle(title)

  return(p)
}
