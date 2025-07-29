#' Create a Circular Dendrogram (Ring Plot)
#'
#' This function visualizes hierarchical data as a circular dendrogram. It takes a
#' data frame with columns representing different levels of a hierarchy (e.g., trait,
#' SNP, Gene), constructs a graph, and plots it using `ggraph`.
#'
#' @name BetterVis_Circos_Ringplot
#'
#' @param data A data frame containing the hierarchical data. It must include columns named "trait", "SNP", and "Gene".
#' @param root A character string specifying the name of the root node for the dendrogram.
#' @param color (Optional) A vector of colors for the main branches (`trait`). If `NULL`, colors will be automatically generated.
#'
#' @return A `ggraph` object representing the circular dendrogram.
#'
#' @importFrom stringr str_split_i
#' @importFrom tidygraph tbl_graph
#' @importFrom ggraph ggraph geom_edge_diagonal geom_node_point scale_edge_color_manual geom_node_text node_angle
#' @importFrom ggplot2 aes guides coord_fixed theme element_rect scale_size scale_color_manual
#' @importFrom dplyr %>% mutate select group_by summarise filter mutate_at as_tibble
#' @importFrom tidyr unite
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("ggraph", quietly = TRUE) && requireNamespace("tidygraph", quietly = TRUE)) {
#'   # Load example data from the package
#'   data("BetterVis_Circos_Ringplot_example", package = "BetterVis")
#'
#'   # Generate the plot
#'   BetterVis_Circos_Ringplot(data = BetterVis_Circos_Ringplot_example, root = "Cough", color = NULL)
#' }
#'}
BetterVis_Circos_Ringplot <- function(data, root, color = NULL) {
  # Add a value column if it doesn't exist, required by gather_graph_node
  if (!"value" %in% colnames(data)) {
    data$value <- 1
  }

  # Define node hierarchy
  index_level <- c("trait", "SNP", "Gene")

  # Build node and edge data using internal helper functions
  nodes_data <- gather_graph_node(data, index = index_level, root = root, value = "value")
  nodes_data$trait <- nodes_data$node.branch

  edges_data <- gather_graph_edge(data, index = index_level, root = root)
  edges_data$trait <- stringr::str_split_i(edges_data$from, "/", 1)

  # Get unique factor levels for coloring, excluding the root
  unique_traits <- unique(nodes_data$trait[nodes_data$trait != root])

  # Generate or validate colors
  if (is.null(color)) {
    plot_colors <- BetterVis_Color(type = "discrete", odd_or_even = "even", n = length(unique_traits), style = "Nature", option = 1)
  } else {
    plot_colors <- color
  }

  # Create a named vector for colors
  if (is.null(names(plot_colors)) && length(plot_colors) >= length(unique_traits)) {
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
    geom_edge_diagonal(aes(color = .data$trait), alpha = 1/3, show.legend = FALSE) +
    geom_node_point(aes(size = .data$node.size, color = .data$trait), alpha = 1/3, show.legend = TRUE) +
    guides(size = "none") +
    coord_fixed() +
    theme(legend.position = "right",
          panel.background = element_rect(fill = NA, color = NA),
          plot.background = element_rect(fill = NA, color = NA)) +
    scale_size(range = c(1, 10)) +
    scale_color_manual(values = plot_colors) +
    ggraph::scale_edge_color_manual(values = plot_colors)

  # Filter node data for text labels
  gene_dat <- dplyr::filter(gm$data, .data$node.level == "Gene")
  snp_dat  <- dplyr::filter(gm$data, .data$node.level == "SNP")

  # Add text labels for nodes
  p <- gm +
    geom_node_text(
      data = gene_dat,
      aes(x = 1.02 * .data$x, y = 1.02 * .data$y,
          label = .data$node.short_name,
          angle = -((-node_angle(.data$x, .data$y) + 90) %% 180) + 90),
      fontface = "bold", size = 2, hjust = "outward"
    ) +
    geom_node_text(
      data = snp_dat,
      aes(x = 1.02 * .data$x, y = 1.02 * .data$y,
          label = .data$node.short_name,
          angle = -((-node_angle(.data$x, .data$y) + 90) %% 180) + 90,
          color = .data$trait),
      size = 2.5, hjust = "outward"
    ) +
    geom_node_text(
      data = dplyr::filter(gm$data, .data$node.level %in% c("trait", root)),
      aes(label = .data$node.short_name,
          angle = -((-node_angle(.data$x, .data$y) + 90) %% 180) + 90,
          color = .data$trait),
      fontface = "bold", show.legend = FALSE, size = 3.5
    )

  return(p)
}


# -------------------------------------------------------------------------
# Internal Helper Functions
# These are not exported to the user but are required by BetterVis_Circos_Ringplot.
# -------------------------------------------------------------------------

#' Gather graph edges from a data.frame (Internal Function)
#' @noRd
gather_graph_edge <- function(df, index = NULL, root = NULL) {
  if (length(index) < 2) {
    stop("please specify at least two index column(s)")
  } else if (length(index) == 2) {
    data <- df %>%
      mutate(from = .data[[index[[1]]]]) %>%
      tidyr::unite("to", all_of(index), sep = "/") %>%
      select("from", "to") %>%
      mutate_at(c("from", "to"), as.character)
  } else {
    list_data <- lapply(seq(2, length(index)), function(i) {
      dots <- index[1:i]
      df %>%
        tidyr::unite("from", all_of(dots[-length(dots)]), sep = "/", remove = FALSE) %>%
        tidyr::unite("to", all_of(dots), sep = "/") %>%
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
#' @noRd
gather_graph_node <- function(df, index = NULL, value = tail(colnames(df), 1), root = NULL) {
  if (length(index) < 2) {
    stop("please specify at least two index column(s)")
  } else {
    list_data <- lapply(seq_along(index), function(i) {
      dots <- index[1:i]
      df %>%
        group_by(!!!syms(dots)) %>%
        summarise(
          node.size = sum(.data[[value]]),
          node.level = index[[i]],
          node.count = n(),
          .groups = "drop"
        ) %>%
        mutate(
          node.short_name = as.character(.data[[dots[[length(dots)]]]]),
          node.branch = as.character(.data[[dots[[1]]]])
        ) %>%
        tidyr::unite("node.name", all_of(dots), sep = "/")
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

# This handles "no visible binding for global variable" NOTES from R CMD check.
utils::globalVariables(c("node.branch", "node.size", "node.level", "node.short_name", "trait", "x", "y"))
