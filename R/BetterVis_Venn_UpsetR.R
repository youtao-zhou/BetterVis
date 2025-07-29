#' Create a Customizable UpSet Plot
#'
#' This function serves as a wrapper for `UpSetR::upset`, providing a convenient
#' way to create UpSet plots for visualizing set intersections with enhanced
#' control over colors and text scaling.
#'
#' @name BetterVis_Venn_UpsetR
#'
#' @param input_list A named list where each element is a vector representing a set.
#' @param ratio A numeric vector of length 2 controlling the ratio of the main
#'   intersection size plot to the set size plot. Default is `c(0.7, 0.3)`.
#' @param point.size Numeric. The size of the points in the matrix. Default is `3`.
#' @param line.size Numeric. The thickness of the lines connecting points in the matrix. Default is `1`.
#' @param y.label Character string. The label for the y-axis (intersection size). Default is `"Intersection Size"`.
#' @param x.label Character string. The label for the x-axis (set size). Default is `"Set Size"`.
#' @param main.bar.color Character string. The color of the main intersection size bars. Default is `"#FCB2AF"`.
#' @param sets.bar.color Character string. The color of the set size bars. Default is `"#9BDFDF"`.
#' @param matrix.color Character string. The color of the points and lines in the matrix. Default is `"#8C9FCA"`.
#' @param all_text_adjust A numeric value for scaling text elements. Can be:
#'   \itemize{
#'     \item A single numeric value to scale all text uniformly (Default is `2`).
#'     \item A numeric vector of length 6 to scale elements individually:
#'       `c(intersection_size_title, intersection_size_tick_labels, set_size_title, set_size_tick_labels, set_names, numbers_above_bars)`.
#'   }
#'
#' @return Prints the UpSet plot to the current graphics device and invisibly returns the UpSet plot object (`grob`).
#'
#' @importFrom UpSetR upset fromList
#'
#' @export
#'
#' @examples
#' library(UpSetR)
#' # Load example data from the package
#' data("BetterVis_Venn_UpsetR_example", package = "BetterVis")
#'
#' # Example 1: Simple usage with uniform text scaling
#' BetterVis_Venn_UpsetR(BetterVis_Venn_UpsetR_example, all_text_adjust = 1.5)
#'
#' # Example 2: Using a vector for fine-tuned text scaling
#' BetterVis_Venn_UpsetR(BetterVis_Venn_UpsetR_example, all_text_adjust = c(1.5, 1, 1.5, 1, 2, 1.2))
#'
#' # Example 3: Customizing colors and labels
#' BetterVis_Venn_UpsetR(
#'   input_list = BetterVis_Venn_UpsetR_example,
#'   ratio = c(0.6, 0.4),
#'   y.label = "Common Elements",
#'   x.label = "Total Elements",
#'   main.bar.color = "skyblue",
#'   sets.bar.color = "navyblue",
#'   matrix.color = "red",
#'   point.size = 5,
#'   line.size = 2
#' )
BetterVis_Venn_UpsetR <- function(input_list,
                                  ratio = c(0.7, 0.3),
                                  point.size = 3,
                                  line.size = 1,
                                  y.label = "Intersection Size",
                                  x.label = "Set Size",
                                  main.bar.color = "#FCB2AF",
                                  sets.bar.color = "#9BDFDF",
                                  matrix.color = "#8C9FCA",
                                  all_text_adjust = 2) {
  # Convert the list to the format required by UpSetR
  data <- fromList(input_list)

  n <- length(input_list)

  # Calculate the number of intersections to display (show all pairwise for simplicity)
  # UpSetR's `nintersects` is more flexible, NA shows all.
  nintersect <- NA

  # Validate and set text scaling parameter
  if (length(all_text_adjust) == 1) {
    text.scale <- all_text_adjust
  } else if (length(all_text_adjust) == 6) {
    text.scale <- all_text_adjust
  } else {
    stop("'all_text_adjust' must be a single numeric value or a vector of length 6.")
  }

  # Generate the plot object
  p <- upset(data,
             nsets = n,
             nintersects = nintersect,
             order.by = c("degree", "freq"),
             decreasing = c(TRUE, TRUE),
             mb.ratio = ratio,
             point.size = point.size,
             line.size = line.size,
             mainbar.y.label = y.label,
             sets.x.label = x.label,
             main.bar.color = main.bar.color,
             sets.bar.color = sets.bar.color,
             text.scale = text.scale,
             matrix.color = matrix.color)

  # The print() command is necessary to draw the plot
  print(p)

  # Return the plot object invisibly, so it doesn't auto-print again if assigned
  return(invisible(p))
}
