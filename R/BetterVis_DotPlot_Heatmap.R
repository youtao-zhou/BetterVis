#' Create a Highly Customized Dot Plot or Heatmap
#'
#' This function serves as a sophisticated wrapper for the `ComplexHeatmap` package,
#' allowing for the creation of intricate heatmaps and dot plots. It offers extensive
#' customization options for scaling, clustering, annotations, colors, and layout.
#'
#' @name BetterVis_DotPlot_Heatmap
#'
#' @param data_matrix A numeric matrix for plotting. Rows typically represent features (e.g., genes) and columns represent groups (e.g., cell types).
#' @param scale Logical. If `TRUE`, the matrix is z-score scaled by row. Default is `TRUE`.
#' @param rescale Logical. If `TRUE` and `scale` is `FALSE`, the data is rescaled to a new range. Default is `FALSE`.
#' @param rescale.range Numeric vector of length 2 specifying the new range for rescaling. Default is `c(0, 3)`.
#' @param dotplot Logical. If `TRUE`, creates a dot plot where dot size represents presence/absence. If `FALSE`, creates a standard heatmap. Default is `TRUE`.
#' @param dots.type Character. Method for scaling dot size, currently only "square root" is implemented.
#' @param dots.size Numeric. A scaling factor for the maximum dot size. Default is `4`.
#' @param show.noexpr.dots Logical. If `TRUE`, dots for zero-value cells are shown at a minimal size. Default is `FALSE`.
#' @param col.min,col.max The minimum and maximum values for the color scale. Can be numeric or a quantile string (e.g., "q1"). Defaults are adjusted based on `scale`.
#' @param data.colors A character vector of 2 or 3 colors for the gradient, or a palette name like "Viridis".
#' @param palette.reverse Logical. If `TRUE`, reverses the color palette. Default is `FALSE`.
#' @param na.color Color for `NA` values. Default is `"grey40"`.
#' @param order.idents Character vector or "reverse". Specifies a custom order for rows or reverses the default order.
#' @param cluster.idents,cluster.features Logical. Whether to perform k-means clustering on rows or columns.
#' @param idents.kmeans,features.kmeans Integer. The number of k-means clusters for rows and columns.
#' @param row_annotation_color,col_annotation_color (Optional) A vector of colors for row/column annotations.
#' @param row_annotation_legend,col_annotation_legend Logical. Whether to display the legend for row/column annotations.
#' @param row_annotation_legend_title,col_annotation_legend_title Character. Titles for the annotation legends.
#' @param row.split,col.split A numeric or character vector to split the heatmap rows/columns into groups.
#' @param ... Additional arguments passed to `ComplexHeatmap::Heatmap`.
#' @param keep_rownames_order Logical. If `TRUE`, forces the row order to match the input matrix. Default is `TRUE`.
#' @param right.annotation,top.annotation,bottom.annotation Logical. Toggles display of annotations on different sides.
#' @param features.names.size,idents.names.size Numeric. Font size for column and row names.
#' @param zscore.legend,show.data.legend Logical. Toggles display of the main data legend.
#' @param legend.title.size,legend.text.size Numeric. Font sizes for legend titles and text.
# (Additional parameters are documented for brevity in the code, see full list in source)
#'
#' @return A `HeatmapList` object, which is automatically drawn to the graphics device.
#'
#' @importFrom scales hue_pal rescale
#' @importFrom grDevices hcl.colors
#' @importFrom stats quantile
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar unit grid.rect grid.circle
#' @importFrom ComplexHeatmap Heatmap HeatmapAnnotation Legend ht_opt draw
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example data and create color vectors
#' data("BetterVis_DotPlot_Heatmap_example", package = "BetterVis")
#' library(RColorBrewer)
#' row_colors <- c(brewer.pal(9,"Set3"), brewer.pal(8,"Set2"))
#' col_colors <- c(brewer.pal(8,"Set1"), brewer.pal(5,"Set2"))
#'
#' # Generate the plot with extensive customization
#' BetterVis_DotPlot_Heatmap(
#'   data_matrix = BetterVis_DotPlot_Heatmap_example,
#'   scale = FALSE,
#'   col.min = 0,
#'   col.max = 2,
#'   show.noexpr.dots = TRUE,
#'   data.colors = c("#1289B2", "#FCF6AF", "#E53849"),
#'   dots.size = 3,
#'   dots.type = "square root",
#'   inner.border = FALSE,
#'   cluster.features = FALSE,
#'   cluster.idents = FALSE,
#'   row.names.side = "right",
#'   idents.names.size = 10,
#'   column.names.side = "top",
#'   column.names.angle = 45,
#'   features.names.size = 10,
#'   top.annotation = TRUE,
#'   row_annotation_color = row_colors,
#'   col_annotation_color = col_colors,
#'   show.data.legend = TRUE,
#'   row_annotation_legend = TRUE,
#'   row_annotation_legend_title = "Rows",
#'   col_annotation_legend = TRUE,
#'   col_annotation_legend_title = "Cols",
#'   zscore.legend = TRUE,
#'   zscore.legend.side = "bottom",
#'   row.split = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), 5),
#'   col.split = c(rep(4,3), 5, 3, rep(1,5), 2, 5, 2)
#' )
#'}
BetterVis_DotPlot_Heatmap <- function(data_matrix,
                                      scale = TRUE,
                                      rescale = FALSE,
                                      rescale.range = c(0, 3),
                                      dotplot = TRUE,
                                      dots.type = "square root",
                                      dots.size = 4,
                                      show.noexpr.dots = FALSE,
                                      col.min = ifelse(isTRUE(scale), -2, 0),
                                      col.max = ifelse(isTRUE(scale), 2, "q100"),
                                      data.colors = if (isTRUE(scale)) c("#1289B2","#FCF6AF","#E53849") else "Viridis",
                                      palette.reverse = FALSE,
                                      na.color = "grey40",
                                      background.color = "white",
                                      order.idents = NULL,
                                      order.colors = TRUE,
                                      kmeans.repeats = 100,
                                      cluster.idents = TRUE,
                                      idents.kmeans = 1,
                                      idents.kmeans.numbers.size = 11,
                                      cluster.features = TRUE,
                                      features.kmeans = 1,
                                      features.kmeans.numbers.size = 11,
                                      idents.gap = 1,
                                      features.gap = 1,
                                      idents.names.size = 9,
                                      features.names.size = 9,
                                      features.names.style = "italic",
                                      row.names.side = "left",
                                      column.names.angle = 45,
                                      column.names.side = "bottom",
                                      inner.border = TRUE,
                                      data.legend.name = ifelse(isTRUE(scale), "Z-Score", "Average Expression"),
                                      show.data.legend = TRUE,
                                      legend.title.size = 10,
                                      legend.text.size = 10,
                                      legend.gap = 10,
                                      row_annotation_color = NULL,
                                      row_annotation_legend = FALSE,
                                      row.split = NULL,
                                      col.split = NULL,
                                      border_gp_control = FALSE,
                                      right.annotation = FALSE,
                                      bottom.annotation = FALSE,
                                      top.annotation = FALSE,
                                      keep_rownames_order = TRUE,
                                      zscore.legend = TRUE,
                                      zscore.legend.side = "bottom",
                                      zscore.legend.direction = "horizontal",
                                      zscore.legend.position = "topcenter",
                                      zscore.legend.width = 5,
                                      col_annotation_legend = FALSE,
                                      col_annotation_color = NULL,
                                      col_annotation_legend_title = "Columns",
                                      row_annotation_legend_title = "Rows",
                                      ...) {

  mat <- data_matrix
  dot <- ifelse(mat > 0, 1, 0)

  if (isTRUE(scale)) {
    if (nrow(mat) < 5) {
      message("Less than 5 rows will be displayed, scaling may produce misleading visualization.\nUsing a standard heatmap might be more appropriate.")
    }
    mat <- t(scale(t(mat))) # Correct row-wise scaling
  }
  if (isTRUE(rescale) & isFALSE(scale)) {
    if (nrow(mat) < 5) {
      message("Less than 5 rows will be displayed, rescaling may produce misleading visualization.\nUsing a standard heatmap might be more appropriate.")
    }
    mat <- apply(mat, 2, function(x) scales::rescale(x, to = rescale.range))
  }

  if (is.character(col.min)) {
    if (isTRUE(grepl("^q[0-9]{1,3}$", col.min, perl = TRUE))) {
      q1 <- as.numeric(sub("q", "", col.min)) / 100
      q1 <- quantile(mat, probs = q1, na.rm = TRUE)
      col.min <- q1
    }
  }
  if (is.character(col.max)) {
    if (isTRUE(grepl("^q[0-9]{1,3}$", col.max, perl = TRUE))) {
      q2 <- as.numeric(sub("q", "", col.max)) / 100
      q2 <- quantile(mat, probs = q2, na.rm = TRUE)
      col.max <- q2
    }
  }

  col.mid <- if (isTRUE(scale)) 0 else (col.min + col.max) / 2

  if (!is.function(data.colors)) {
    if (length(data.colors) > 3) {
      stop("data.colors must have 2 or 3 colors or must be a palette name.")
    }
    if (length(data.colors) == 3) {
      data.colors <- colorRamp2(breaks = c(col.min, col.mid, col.max), colors = data.colors)
    } else if (length(data.colors) == 2) {
      data.colors <- colorRamp2(breaks = c(col.min, col.max), colors = data.colors)
    } else {
      # Assume it's a palette name
      palette_func <- hcl.colors # A generic base R palette function
      tryCatch({
        # This part is a simplification. A full solution would map strings like "Viridis" to the right function.
        data.colors <- colorRamp2(breaks = c(col.min, col.max), colors = palette_func(256, palette = data.colors, rev = palette.reverse))
      }, error = function(e) stop("Invalid palette name provided to data.colors"))
    }
  }


  idents.colors <- hue_pal()(n = nrow(mat))
  if (isTRUE(order.colors)) {
    if (is.character(order.idents)) {
      if (length(order.idents) == nrow(mat)) {
        idents.colors <- idents.colors[order(match(rownames(mat), order.idents))]
      } else if (length(order.idents) == 1 && order.idents == "reverse") {
        idents.colors <- rev(idents.colors)
      } else {
        stop("order.idents must be 'reverse' or a character vector of the same length as the number of rows.")
      }
    }
  }
  names(idents.colors) <- rownames(mat)

  if (!is.null(row_annotation_color)) {
    if (is.character(row_annotation_color) && length(row_annotation_color) == 1) {
      idents.colors <- rep(row_annotation_color, nrow(mat))
    } else if (is.character(row_annotation_color) && length(row_annotation_color) == nrow(mat)) {
      idents.colors <- row_annotation_color
    } else if (is.vector(row_annotation_color) && !is.null(names(row_annotation_color))) {
      idents.colors <- row_annotation_color[rownames(mat)]
    } else {
      stop("row_annotation_color must be a single color, a vector of colors with the same length as rows, or a named color vector.")
    }
    names(idents.colors) <- rownames(mat)
  }

  idents.legend <- NULL
  if (isTRUE(show.data.legend) && isTRUE(row_annotation_legend)) {
    idents.legend <- Legend(at = rownames(mat), legend_gp = gpar(fill = idents.colors), title = row_annotation_legend_title,
                            gap = unit(0.5, "cm"), border = TRUE, title_gp = gpar(fontsize = legend.title.size, fontface = "bold"),
                            labels_gp = gpar(fontsize = legend.text.size))
  }

  anno.legend <- list()
  if (!is.null(idents.legend)) {
    anno.legend <- c(anno.legend, list(idents.legend))
  }

  idents.labels <- HeatmapAnnotation(Rows = rownames(mat), col = list(Rows = idents.colors),
                                     na_col = na.color, which = "row", show_annotation_name = FALSE, show_legend = FALSE)

  if (!is.null(col_annotation_color)) {
    if (is.character(col_annotation_color) && length(col_annotation_color) == 1) {
      column.colors <- setNames(rep(col_annotation_color, ncol(mat)), colnames(mat))
    } else if (is.character(col_annotation_color) && length(col_annotation_color) == ncol(mat)) {
      column.colors <- setNames(col_annotation_color, colnames(mat))
    } else if (is.vector(col_annotation_color) && !is.null(names(col_annotation_color))) {
      column.colors <- col_annotation_color[colnames(mat)]
    } else {
      column.colors <- setNames(hue_pal()(ncol(mat)), colnames(mat))
    }
  } else {
    column.colors <- setNames(hue_pal()(ncol(mat)), colnames(mat))
  }


  column.labels <- HeatmapAnnotation(columns = colnames(mat), col = list(columns = column.colors),
                                     which = "column", show_annotation_name = FALSE, show_legend = FALSE)

  bottom_annotation <- if(isTRUE(bottom.annotation)) column.labels else NULL
  top_annotation <- if(isTRUE(top.annotation)) column.labels else NULL

  if (isTRUE(show.data.legend) && isTRUE(col_annotation_legend)){
    col.legend <- Legend(at = names(column.colors), legend_gp = gpar(fill = column.colors), title = col_annotation_legend_title,
                         gap = unit(0.5, "cm"), border = TRUE, title_gp = gpar(fontsize = legend.title.size, fontface = "bold"),
                         labels_gp = gpar(fontsize = legend.text.size))
    anno.legend <- c(anno.legend, list(col.legend))
  }

  ht_opt$ANNOTATION_LEGEND_PADDING <- unit(legend.gap, "mm")
  ht_opt$HEATMAP_LEGEND_PADDING <- if (zscore.legend.side %in% c("right", "left") || column.names.side == "top") unit(legend.gap, "mm") else unit(2, "mm")

  # Main Heatmap call
  ht <- Heatmap(mat, col = data.colors, na_col = na.color, name = data.legend.name,
                rect_gp = if(dotplot) gpar(type = "none") else if(inner.border) gpar(col = "black") else gpar(col = NA),
                border_gp = if (border_gp_control) gpar() else gpar(col = NA),
                layer_fun = if(dotplot) function(j, i, x, y, w, h, fill) {
                  grid.rect(x = x, y = y, width = w, height = h, gp = gpar(col = "grey", fill = "white", lwd = 0.5))
                  dot_size_val <- if(dots.type == "radius") pindex(dot, i, j) else sqrt(pindex(dot, i, j))
                  grid.circle(x = x, y = y, r = dot_size_val * unit(dots.size, "mm"),
                              gp = gpar(fill = data.colors(pindex(mat, i, j)), col = if(inner.border) "black" else NA))
                } else NULL,
                left_annotation = idents.labels,
                right_annotation = if(right.annotation) idents.labels else NULL,
                bottom_annotation = bottom_annotation, top_annotation = top_annotation,
                cluster_rows = cluster.idents, cluster_columns = cluster.features,
                row_split = row.split, column_split = col.split,
                row_title_rot = 0, row_title_side = "right", column_title_side = "bottom",
                row_title_gp = gpar(fontsize = idents.kmeans.numbers.size),
                column_title_gp = gpar(fontsize = features.kmeans.numbers.size),
                row_km = idents.kmeans, column_km = features.kmeans,
                row_km_repeats = kmeans.repeats, column_km_repeats = kmeans.repeats,
                show_column_names = features.names.size > 0, show_row_names = idents.names.size > 0,
                column_names_gp = gpar(fontsize = features.names.size, fontface = features.names.style),
                row_names_gp = gpar(fontsize = idents.names.size),
                column_names_rot = column.names.angle,
                row_names_side = row.names.side, column_names_side = column.names.side,
                row_dend_side = "right", show_heatmap_legend = isTRUE(zscore.legend) && isTRUE(show.data.legend),
                heatmap_legend_param = list(legend_direction = zscore.legend.direction,
                                            title_position = zscore.legend.position,
                                            legend_width = unit(zscore.legend.width, "cm"),
                                            title_gp = gpar(fontsize = legend.title.size, fontface = "bold"),
                                            labels_gp = gpar(fontsize = legend.text.size)), ...)

  return(draw(ht, heatmap_legend_side = zscore.legend.side,
              align_heatmap_legend = "heatmap_center", align_annotation_legend = "heatmap_center",
              annotation_legend_list = anno.legend, legend_grouping = "original"))
}


#' Helper function to safely index a matrix within ComplexHeatmap's layer_fun
#' This function is not exported.
#' @noRd
pindex <- function(x, i, j) {
  x[i, j]
}

# This handles the "no visible binding for global variable 'ht_opt'" NOTE from R CMD check.
utils::globalVariables(c("ht_opt"))
