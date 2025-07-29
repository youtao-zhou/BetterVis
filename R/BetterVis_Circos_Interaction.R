#' Create an Interactive Circos Plot
#'
#' Generates a multi-track Circos plot to visualize relationships and data across different categories.
#' The plot can include up to three outer annotation tracks and an optional inner chord diagram to display interactions.
#'
#' @name BetterVis_Circos_Interaction
#'
#' @param df A data frame containing the primary data for sectors and annotation tracks.
#' @param interaction_data (Optional) A data frame defining the interactions (links) between sectors for the chord diagram. Default is `NULL`.
#' @param main_col A character string specifying the column name in `df` for the main sectors (e.g., gene IDs).
#' @param main_col_color_classify A character string for the column name in `df` used to group and color the main sectors.
#' @param col1_name A character string for the column name in `df` for the first (innermost) annotation track.
#' @param col2_name (Optional) A character string for the column name in `df` for the second (middle) annotation track.
#' @param col3_name (Optional) A character string for the column name in `df` for the third (outermost) annotation track.
#' @param main_col_color A vector of colors for the main sectors, corresponding to the levels in `main_col_color_classify`.
#' @param color1 A vector of colors for the first annotation track.
#' @param color2 (Optional) A vector of colors for the second annotation track.
#' @param color3 (Optional) A vector of colors for the third annotation track.
#' @param link_source_col (Optional) The column in `interaction_data` specifying the source of the interaction links.
#' @param link_target_col (Optional) The column in `interaction_data` specifying the target of the interaction links.
#' @param link_value_col (Optional) The column in `interaction_data` with numeric values for the link widths (e.g., correlation).
#' @param link_pvalue_col (Optional) The column in `interaction_data` with p-values, used to style the links (e.g., solid vs. dashed).
#' @param main_col_text A logical value indicating whether to display labels for the main sectors. Default is `TRUE`.
#' @param col1_text A logical value indicating whether to display labels for the first annotation track. Default is `TRUE`.
#' @param col2_text A logical value indicating whether to display labels for the second annotation track. Default is `TRUE`.
#' @param col3_text A logical value indicating whether to display labels for the third annotation track. Default is `TRUE`.
#'
#' @return This function does not return an object. It draws a plot on the current graphics device.
#'
#' @importFrom circlize circos.clear circos.par chordDiagram circos.initialize circos.track circos.rect circos.text highlight.sector mm_h CELL_META
#' @importFrom dplyr %>% filter select arrange
#' @importFrom rlang sym !!!
#' @importFrom methods is
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(circlize)
#' # Load example data from the package
#' data("BetterVis_Circos_Interaction_example1", package = "BetterVis")
#' data("BetterVis_Circos_Interaction_example2", package = "BetterVis")
#'
#' # Define color palettes
#' my_main_col_color <- c("#FBB4AE", "#B3CDE3", "#CCEBC5")
#' my_color1 <- c("#FED9A6", "#FFFFCC")
#' my_color2 <- c("#DEEBF7", "#C6DBEF", "#9ECAE1")
#'
#' # Example 1: Full plot with interaction links
#' BetterVis_Circos_Interaction(
#'   df = BetterVis_Circos_Interaction_example1,
#'   interaction_data = BetterVis_Circos_Interaction_example2,
#'   main_col = "ID_Gene",
#'   main_col_color_classify = "type_main",
#'   col1_name = "type_col1",
#'   col2_name = "type_col2",
#'   main_col_color = my_main_col_color,
#'   color1 = my_color1,
#'   color2 = my_color2,
#'   link_source_col = "ID",
#'   link_target_col = "Gene",
#'   link_value_col = "Correlation",
#'   link_pvalue_col = "Pvalue"
#' )
#'
#' # Example 2: Plot without interaction links
#' BetterVis_Circos_Interaction(
#'   df = BetterVis_Circos_Interaction_example1,
#'   main_col = "ID_Gene",
#'   main_col_color_classify = "type_main",
#'   col1_name = "type_col1",
#'   col2_name = "type_col2",
#'   main_col_color = my_main_col_color,
#'   color1 = my_color1,
#'   color2 = my_color2
#' )

BetterVis_Circos_Interaction <- function(
    df,
    interaction_data = NULL,
    main_col,
    main_col_color_classify,
    col1_name,
    col2_name = NULL,
    col3_name = NULL,
    main_col_color,
    color1,
    color2 = NULL,
    color3 = NULL,
    link_source_col = NULL,
    link_target_col = NULL,
    link_value_col = NULL,
    link_pvalue_col = NULL,
    main_col_text = TRUE,
    col1_text = TRUE,
    col2_text = TRUE,
    col3_text = TRUE
) {

  circos.clear()

  circos.par(
    gap.after = 0,
    start.degree = 90,
    canvas.xlim = c(-1, 1),
    canvas.ylim = c(-1, 1)
  )

  if (!is.null(interaction_data) && !is.null(link_source_col) && !is.null(link_target_col)) {
    nm <- unique(c(interaction_data[[link_source_col]], interaction_data[[link_target_col]]))
  } else {
    nm <- unique(df[[main_col]])
  }

  cols_to_select <- c(main_col, main_col_color_classify, col1_name)
  if (!is.null(col2_name)) cols_to_select <- c(cols_to_select, col2_name)
  if (!is.null(col3_name)) cols_to_select <- c(cols_to_select, col3_name)

  df_for_ordering <- df %>%
    filter(!!rlang::sym(main_col) %in% nm) %>%
    select(all_of(cols_to_select))

  arrange_args <- list(rlang::sym(main_col_color_classify))
  if (!is.null(col2_name)) arrange_args <- c(arrange_args, list(rlang::sym(col2_name)))
  arrange_args <- c(arrange_args, list(rlang::sym(col1_name)))
  if (!is.null(col3_name)) arrange_args <- c(arrange_args, list(rlang::sym(col3_name)))
  arrange_args <- c(arrange_args, list(rlang::sym(main_col)))

  df_ordered_sectors <- df_for_ordering %>%
    arrange(!!!arrange_args)

  group_for_chordDiagram <- structure(
    df_ordered_sectors[[main_col_color_classify]],
    names = df_ordered_sectors[[main_col]]
  )

  group_col1_ordered <- structure(
    df_ordered_sectors[[col1_name]],
    names = df_ordered_sectors[[main_col]]
  )
  if (!is.null(col2_name)) {
    group_col2_ordered <- structure(
      df_ordered_sectors[[col2_name]],
      names = df_ordered_sectors[[main_col]]
    )
  }
  if (!is.null(col3_name)) {
    group_col3_ordered <- structure(
      df_ordered_sectors[[col3_name]],
      names = df_ordered_sectors[[main_col]]
    )
  }

  grid.col <- setNames(
    main_col_color[as.numeric(factor(df_ordered_sectors[[main_col_color_classify]],
                                     levels = unique(df[[main_col_color_classify]])))],
    df_ordered_sectors[[main_col]]
  )

  preAllocateTracks_list <- list()
  track_indices <- list(col3 = NULL, col2 = NULL, col1 = NULL)
  current_allocated_track_idx <- 0

  if (!is.null(col3_name)) {
    current_allocated_track_idx <- current_allocated_track_idx + 1
    preAllocateTracks_list <- c(preAllocateTracks_list,
                                list(list(track.height = mm_h(8), track.margin = c(mm_h(4), 0))))
    track_indices$col3 <- current_allocated_track_idx
  }

  if (!is.null(col2_name)) {
    current_allocated_track_idx <- current_allocated_track_idx + 1
    preAllocateTracks_list <- c(preAllocateTracks_list,
                                list(list(track.height = mm_h(8), track.margin = c(mm_h(4), 0))))
    track_indices$col2 <- current_allocated_track_idx
  }

  current_allocated_track_idx <- current_allocated_track_idx + 1
  preAllocateTracks_list <- c(preAllocateTracks_list,
                              list(list(track.height = mm_h(8), track.margin = c(mm_h(4), 0))))
  track_indices$col1 <- current_allocated_track_idx

  if (!is.null(interaction_data) &&
      !is.null(link_source_col) &&
      !is.null(link_target_col) &&
      !is.null(link_value_col) &&
      !is.null(link_pvalue_col)) {

    chord_data <- interaction_data[, c(link_source_col, link_target_col, link_value_col)]

    chordDiagram(
      chord_data,
      annotationTrack = "grid",
      annotationTrackHeight = mm_h(16),
      directional = 1,
      col = "white",
      group = group_for_chordDiagram,
      grid.col = grid.col,
      direction.type = "arrows",
      link.auto = FALSE,
      link.lwd = 1,
      link.arr.length = 0.2,
      link.arr.lty = ifelse(interaction_data[[link_pvalue_col]] < 0.01, "solid", "dashed"),
      scale = TRUE,
      big.gap = 0,
      small.gap = 0,
      preAllocateTracks = preAllocateTracks_list
    )

    main_label_track_index <- length(preAllocateTracks_list) + 1

  } else {
    unique_ids <- unique(df_ordered_sectors[[main_col]])
    circos.initialize(
      factors = unique_ids,
      xlim = cbind(rep(0, length(unique_ids)), rep(1, length(unique_ids)))
    )

    for (track in preAllocateTracks_list) {
      circos.track(
        ylim = c(0, 1),
        track.height = track$track.height,
        track.margin = track$track.margin,
        bg.border = NA,
        panel.fun = function(x, y) {}
      )
    }

    circos.track(
      ylim = c(0, 1),
      bg.col = grid.col,
      bg.border = NA,
      track.height = mm_h(8),
      panel.fun = function(x, y) {
        sector.index <- CELL_META$sector.index
        xlim <- CELL_META$xlim
        ylim <- CELL_META$ylim
        circos.rect(
          xlim[1], ylim[1], xlim[2], ylim[2],
          col = grid.col[sector.index], border = NA
        )
      }
    )

    main_label_track_index <- length(preAllocateTracks_list) + 2
  }

  circos.track(
    track.index = main_label_track_index,
    ylim = c(0, 1),
    track.height = mm_h(8),
    track.margin = c(mm_h(4), 0),
    panel.fun = function(x, y) {
      if (main_col_text) {
        circos.text(
          CELL_META$xcenter,
          CELL_META$ylim[1],
          CELL_META$sector.index,
          facing = "clockwise",
          niceFacing = TRUE,
          adj = c(0, 0.5)
        )
      }
    },
    bg.border = "#969696"
  )

  for (t in seq_along(unique(group_col1_ordered))) {
    highlight.sector(
      names(group_col1_ordered[group_col1_ordered == unique(group_col1_ordered)[t]]),
      track.index = track_indices$col1,
      col = color1[t],
      text = if (col1_text) unique(group_col1_ordered)[t] else NULL,
      cex = 1,
      text.col = "black",
      niceFacing = TRUE,
      border = "#969696"
    )
  }

  if (!is.null(col2_name) && !is.null(color2)) {
    for (t in seq_along(unique(group_col2_ordered))) {
      highlight.sector(
        names(group_col2_ordered[group_col2_ordered == unique(group_col2_ordered)[t]]),
        track.index = track_indices$col2,
        col = color2[t],
        text = if (col2_text) unique(group_col2_ordered)[t] else NULL,
        cex = 1,
        text.col = "black",
        niceFacing = TRUE,
        border = "#969696"
      )
    }
  }

  if (!is.null(col3_name) && !is.null(color3)) {
    for (t in seq_along(unique(group_col3_ordered))) {
      highlight.sector(
        names(group_col3_ordered[group_col3_ordered == unique(group_col3_ordered)[t]]),
        track.index = track_indices$col3,
        col = color3[t],
        text = if (col3_text) unique(group_col3_ordered)[t] else NULL,
        cex = 1,
        text.col = "black",
        niceFacing = TRUE,
        border = "#969696"
      )
    }
  }

  circos.clear()
}

# This handles the "no visible binding for global variable 'CELL_META'" NOTE from R CMD check.
utils::globalVariables(c("CELL_META"))
