#' Create a Circular Dendrogram Plot
#'
#' This function generates a circular dendrogram to visualize hierarchical data structures,
#' typically for relationships between traits, SNPs, and genes. It provides options for
#' custom coloring and utilizes `ggraph` for the visualization.
#'
#' @name BetterVis_Circle_Plot
#' @description Generates a circular dendrogram plot to visualize hierarchical data with customizable aesthetics.
#'
#' @param data A data frame containing the hierarchical data. It must include columns specified in the `index_level`.
#' @param root A character string specifying the name of the root node for the dendrogram.
#' @param color An optional vector of colors for the main branches (`trait`). If `NULL`, colors will be automatically generated using `BetterVis_Color`.
#'
#' @return A `ggraph` object representing the circular dendrogram visualization.
#'
#' @importFrom tidygraph tbl_graph
#' @importFrom ggraph ggraph geom_edge_diagonal geom_node_point geom_node_text node_angle scale_edge_color_manual
#' @importFrom ggplot2 guides coord_fixed theme element_rect scale_size scale_color_manual aes
#' @importFrom dplyr filter mutate select group_by summarise mutate_at as_tibble
#' @importFrom stringr str_split_i
#' @importFrom tidyr unite
#'
#' @examples
#' # Load necessary libraries
#' library(ggraph)
#' library(tidygraph)
#' library(dplyr)
#'
#' # Load example data
#' # Assuming 'BetterVis_Circle_Plot_example' is loaded from the package
#' # data("BetterVis_Circle_Plot_example", package = "BetterVis")
#' # df <- BetterVis_Circle_Plot_example
#'
#' # Create a sample dataframe for demonstration as the example data is not provided
#' df <- data.frame(
#'   trait = sample(c("TraitA", "TraitB"), 100, replace = TRUE),
#'   SNP = paste0("rs", sample(1:20, 100, replace = TRUE)),
#'   Gene = paste0("Gene", sample(c("X", "Y", "Z", "W"), 100, replace = TRUE)),
#'   value = runif(100) # A value column for node sizes
#' )
#'
#' # Generate the plot
#' if (requireNamespace("ggraph") & requireNamespace("tidygraph")) {
#'   BetterVis_Circle_Plot(data = df, root = "MyNetwork", color = c("TraitA" = "#e41a1c", "TraitB" = "#377eb8"))
#' }
#' @export
BetterVis_Circle_Plot <- function(data, root, color = NULL) {
  # Define node hierarchy
  index_level <- c("trait", "SNP", "Gene")

  # Build node data using the internal helper function
  nodes_data <- gather_graph_node(data, index = index_level, root = root, value = "value")
  nodes_data$trait <- nodes_data$node.branch

  # Build edge data using the internal helper function
  edges_data <- gather_graph_edge(data, index = index_level, root = root)
  edges_data$trait <- stringr::str_split_i(edges_data$from, "/", 1)

  # Get unique factor levels for coloring
  unique_traits <- unique(nodes_data$trait[nodes_data$trait != root])

  # Generate or validate colors
  if (is.null(color)) {
    # If no color is provided, generate using the package's color function
    plot_colors <- BetterVis_Color(type = "discrete", odd_or_even = "even", n = length(unique_traits), style = "Nature", option = 1)
  } else {
    plot_colors <- color
  }

  # Create a named vector for colors
  if (is.null(names(plot_colors))) {
    names(plot_colors) <- unique_traits
  }
  # Add root color if it's missing
  if (!root %in% names(plot_colors)) {
    plot_colors[root] <- "grey"
  }


  # Build the graph object
  graph_data <- tbl_graph(nodes = nodes_data, edges = edges_data)

  # Create the base circular plot
  gm <- ggraph(graph_data, layout = 'dendrogram', circular = TRUE) +
    geom_edge_diagonal(aes(color = trait), alpha = 1/3, show.legend = FALSE) +
    geom_node_point(aes(size = node.size, color = trait), alpha = 1/3, show.legend = TRUE) +
    guides(size = "none") +
    coord_fixed() +
    theme(
      legend.position = "right",
      panel.background = element_rect(fill = NA, color = NA),
      plot.background = element_rect(fill = NA, color = NA)
    ) +
    scale_size(range = c(1, 10)) +
    scale_color_manual(values = plot_colors) +
    ggraph::scale_edge_color_manual(values = plot_colors)

  # Filter node data for text labels
  gene_dat <- dplyr::filter(gm$data, node.level == "Gene")
  snp_dat  <- dplyr::filter(gm$data, node.level == "SNP")

  # Add text labels for nodes
  p <- gm +
    geom_node_text(
      data = gene_dat,
      aes(x = 1.02 * x, y = 1.02 * y,
          label = node.short_name,
          angle = -((-node_angle(x, y) + 90) %% 180) + 90),
      fontface = "bold",
      size = 2,
      hjust = "outward"
    ) +
    geom_node_text(
      data = snp_dat,
      aes(x = 1.02 * x, y = 1.02 * y,
          label = node.short_name,
          angle = -((-node_angle(x, y) + 90) %% 180) + 90,
          color = trait),
      size = 2.5,
      hjust = "outward"
    ) +
    geom_node_text(
      data = dplyr::filter(gm$data, node.level %in% c("trait", root)),
      aes(label = node.short_name,
          angle = -((-node_angle(x, y) + 90) %% 180) + 90,
          color = trait),
      fontface = "bold",
      show.legend = FALSE,
      size = 3.5
    )

  return(p)
}


# -------------------------------------------------------------------------
# Internal Helper Functions
# These functions are not exported to the user but are required by
# BetterVis_Circle_Plot.
# -------------------------------------------------------------------------

#' Gather graph edges from a data.frame (Internal Function)
#' @param df a data.frame
#' @param index grouping columns
#' @param root root name
#' @return a tibble of edges
#' @noRd
gather_graph_edge <- function(df, index = NULL, root = NULL) {
  if (length(index) < 2) {
    stop("Please specify at least two index columns.")
  } else if (length(index) == 2) {
    data <- df %>%
      mutate(from = .data[[index[[1]]]]) %>%
      unite("to", all_of(index), sep = "/") %>%
      select("from", "to") %>%
      mutate_at(c("from", "to"), as.character)
  } else {
    list_data <- lapply(seq(2, length(index)), function(i) {
      dots <- index[1:i]
      df %>%
        unite("from", all_of(dots[-length(dots)]), sep = "/", remove = FALSE) %>%
        unite("to", all_of(dots), sep = "/") %>%
        select("from", "to") %>%
        mutate_at(c("from", "to"), as.character)
    })
    data <- do.call("rbind", list_data)
  }

  data <- as_tibble(data)

  if (is.null(root)) {
    return(data)
  } else {
    root_data <- df %>%
      group_by(.data[[index[[1]]]]) %>%
      summarise(count = n()) %>%
      mutate(from = root, to = as.character(.data[[index[[1]]]]) ) %>%
      select("from", "to")
    return(rbind(root_data, data))
  }
}

#' Gather graph nodes from a data.frame (Internal Function)
#' @param df a data.frame
#' @param index grouping columns
#' @param value column for node size
#' @param root root name
#' @return a tibble of nodes
#' @noRd
gather_graph_node <- function(df, index = NULL, value = tail(colnames(df), 1), root = NULL) {
  if (length(index) < 2) {
    stop("Please specify at least two index columns.")
  } else {
    # Ensure the value column exists
    if (!value %in% colnames(df)) {
      stop(paste("The specified 'value' column '", value, "' does not exist in the data frame.", sep=""))
    }
    list_data <- lapply(seq_along(index), function(i) {
      dots <- index[1:i]
      df %>%
        group_by(!!!syms(dots)) %>%
        summarise(
          node.size = sum(.data[[value]]),
          node.level = index[[i]],
          node.count = n()
        ) %>%
        ungroup() %>% # Ungroup before mutate
        mutate(
          node.short_name = as.character(.data[[dots[[length(dots)]]]]),
          node.branch = as.character(.data[[dots[[1]]]])
        ) %>%
        unite("node.name", all_of(dots), sep = "/")
    })
    data <- do.call("rbind", list_data) %>% as_tibble()
    data$node.level <- factor(data$node.level, levels = index)

    if (is.null(root)) {
      return(data)
    } else {
      root_data <- data.frame(
        node.name = root,
        node.size = sum(df[[value]]),
        node.level = root,
        node.count = 1,
        node.short_name = root,
        node.branch = root,
        stringsAsFactors = FALSE
      )
      data <- rbind(root_data, data)
      data$node.level <- factor(data$node.level, levels = c(root, index))
      return(data)
    }
  }
}
