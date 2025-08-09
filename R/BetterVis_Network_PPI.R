#' Create a Network Plot with Pie Chart Nodes
#'
#' This function generates a sophisticated network visualization (e.g., for PPI) where
#' each node is represented as a pie chart. It uses `ggraph` for layout and plotting,
#' with options to encircle groups of nodes.
#'
#' @name BetterVis_Network_PPI
#'
#' @param nodes_data A data frame for the nodes, containing a 'name' column and numeric columns for `fill_vars`.
#' @param edges_data A data frame for the edges, with 'from' and 'to' columns matching names in `nodes_data`.
#' @param fill_vars A character vector of 2 to 5 column names in `nodes_data` whose values will form the slices of the pie chart nodes.
#' @param fill_color (Optional) A vector of colors for the pie chart slices. If `NULL`, a default palette is used.
#' @param group_var (Optional) A character string for a column name in `nodes_data` used to group nodes with a surrounding border.
#' @param node_size A numeric vector of length 2 specifying the `min` and `max` scaled size of the nodes.
#' @param node_text_size Numeric. The font size for node labels.
#' @param line_size A numeric vector of length 2 specifying the `min` and `max` scaled width of the edges.
#' @param line_color Character string. The color of the edges.
#' @param niter Numeric. The number of iterations for the Fruchterman-Reingold layout algorithm.
#' @param display_border Logical. If `TRUE` and `group_var` is provided, displays borders around node groups.
#'
#' @return A `ggraph` object.
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph create_layout ggraph geom_edge_link scale_edge_width geom_node_text
#' @importFrom ggforce geom_arc_bar geom_mark_hull
#' @importFrom dplyr %>% mutate across all_of as_tibble group_by arrange lag ungroup left_join rename filter n summarise
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 aes scale_fill_manual coord_equal theme element_text geom_rect scale_colour_manual theme_void
#' @importFrom scales rescale
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang sym
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' library(BetterVis)
#'
#' ## Data Input
#' data("BetterVis_Network_PPI_nodes",package="BetterVis")
#' data("BetterVis_Network_PPI_edges",package="BetterVis")
#' colors<-c('#CD8280','#54D0B4','#78C3ED', '#E69F00', '#E98FBD')
#'
#' ## Usage Example
#' BetterVis_Network_PPI(nodes_data = BetterVis_Network_PPI_nodes ,edges_data =  BetterVis_Network_PPI_edges,
#' fill_vars = c("full","within5","after5","early","late"),
#' fill_color = colors,
#' group_var = "manual_grp",
#' node_size = c(0.08, 0.25),node_text_size = 4,
#' line_size = c(0.5, 2.5),line_color = "grey65",
#' niter = 800,display_border = TRUE)
#' #+theme(legend.text = element_text(size=12))+guides()
#'
#'
#'
#'

BetterVis_Network_PPI <- function(nodes_data, edges_data,
                                  fill_vars,
                                  node_size = c(0.1, 0.3),
                                  fill_color = NULL,
                                  line_size = c(1, 3),
                                  line_color = "grey65",
                                  node_text_size = 4,
                                  niter = 800,
                                  display_border = TRUE,
                                  group_var = NULL) {

  stopifnot(all(fill_vars %in% colnames(nodes_data)))
  stopifnot(length(fill_vars) >= 2 && length(fill_vars) <= 5)

  if (!is.null(group_var)) {
    stopifnot(group_var %in% colnames(nodes_data))
    nodes_data[[group_var]] <- as.factor(nodes_data[[group_var]])
  } else {
    display_border <- FALSE
  }

  if (is.null(fill_color)) {
    fill_color <- RColorBrewer::brewer.pal(length(fill_vars), "Set1")
  }
  fill_color_map <- setNames(fill_color, fill_vars)

  ig <- graph_from_data_frame(edges_data, FALSE, nodes_data)
  g <- as_tbl_graph(ig) %>%
    mutate(size_raw = rowSums(across(all_of(fill_vars))))

  set.seed(123)
  layout <- create_layout(g, layout = "fr", niter = niter)
  layout$radius <- scales::rescale(layout$size_raw, to = node_size)

  pie_df <- layout %>%
    as_tibble() %>%
    tidyr::pivot_longer(cols = all_of(fill_vars),
                        names_to = "stage", values_to = "value") %>%
    group_by(.data$name) %>%
    arrange(.data$stage) %>%
    mutate(end = cumsum(.data$value) / sum(.data$value) * 2 * pi,
           start = lag(.data$end, default = 0)) %>%
    ungroup()

  border_layers <- list()
  if (display_border) {
    layout_df <- layout %>%
      as_tibble() %>%
      left_join(
        nodes_data[, c("name", group_var)] %>%
          rename(group = !!sym(group_var)),
        by = "name"
      ) %>%
      mutate(group = as.factor(.data$group))

    grp_levels <- levels(layout_df$group)
    grp_cols <- setNames(RColorBrewer::brewer.pal(length(grp_levels), "Set2"), grp_levels)

    small_grps <- layout_df %>%
      group_by(.data$group) %>%
      filter(n() < 3) %>%
      ungroup()

    padding <- 0.3
    rect_df <- small_grps %>%
      group_by(.data$group) %>%
      summarise(
        xmin = min(.data$x) - padding,
        xmax = max(.data$x) + padding,
        ymin = min(.data$y) - padding,
        ymax = max(.data$y) + padding,
        .groups = "drop"
      ) %>%
      mutate(color = grp_cols[as.character(.data$group)])

    border_layers <- list(
      geom_mark_hull(
        data = layout_df %>% group_by(.data$group) %>% filter(n() >= 3),
        aes(x = .data$x, y = .data$y, group = .data$group, color = .data$group),
        fill = NA, linetype = "22", linewidth = 0.6, show.legend = FALSE
      ),
      geom_rect(
        data = rect_df,
        aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax, color = .data$group),
        fill = NA, linetype = "22", linewidth = 0.6, show.legend = FALSE, inherit.aes = FALSE
      )
    )
  }

  p <- ggraph(layout) +
    border_layers +
    geom_edge_link(aes(width = .data$weight),
                   colour = line_color, lineend = "round") +
    scale_edge_width(range = line_size, guide = "none") +
    geom_arc_bar(aes(x0 = .data$x, y0 = .data$y,
                     r0 = 0, r = .data$radius,
                     start = .data$start, end = .data$end,
                     fill = .data$stage),
                 data = pie_df, colour = NA) +
    geom_node_text(aes(label = .data$name), repel = TRUE, size = node_text_size) +
    scale_fill_manual(values = fill_color_map, name = NULL) +
    coord_equal() +
    theme_void(base_size = 11) +
    theme(legend.position = "right",
          legend.text = element_text(size = 9))

  if (display_border) {
    p <- p + scale_colour_manual(values = grp_cols, guide = "none")
  }

  return(p)
}

# This handles "no visible binding for global variable" NOTES from R CMD check.
utils::globalVariables(c("size_raw", "name", "stage", "value", "end", "start",
                         "group", "x", "y", "xmin", "xmax", "ymin", "ymax",
                         "weight", "radius"))
