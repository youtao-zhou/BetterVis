#' Create a Single Pie Chart
#'
#' This function generates a customizable pie chart from a data frame using
#' `ggplot2`. It allows for custom colors and optional in-slice labels.
#'
#' @name BetterVis_Pie_Single
#'
#' @param df A data frame containing the data for plotting.
#' @param ID_var A character string for the column name of the categorical variable that defines the pie slices.
#' @param Proportion_var A character string for the column name of the numeric variable that defines the slice proportions.
#' @param colors (Optional) A vector of colors for the pie slices. If `NULL`, a default color palette is generated.
#' @param label Logical. If `TRUE`, adds labels with names and percentages directly onto the pie slices. Default is `FALSE`.
#' @param size_adjust A single numeric value to uniformly scale text elements (labels and legend). Default is `1`.
#'
#' @return A `ggplot` object representing the pie chart.
#'
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar scale_fill_manual theme element_blank element_text geom_text
#' @importFrom dplyr %>% arrange desc mutate
#' @importFrom rlang sym
#' @importFrom ggtext element_markdown
#' @importFrom scales percent
#' @importFrom grDevices hcl
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("BetterVis_Pie_Single_example", package = "BetterVis")
#'
#' # Define a color palette
#' col <- c("#A2D4AB", "#82B3C9", "#E0BBE4", "#FAD0C3", "#C9E4DE", "#FFECB3")
#'
#' BetterVis_Pie_Single(df = BetterVis_Pie_Single_example,ID_var = "Cancer",Proportion_var = "Percent",
#'                     colors = col,label = TRUE ,size_adjust = 1)+theme()+guides()
#'
#'
BetterVis_Pie_Single <- function(df, ID_var, Proportion_var, colors = NULL, label = FALSE, size_adjust = 1) {

  # Input validation
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame.")
  }
  if (!(ID_var %in% colnames(df) && Proportion_var %in% colnames(df))) {
    stop(paste0("One or both of '", ID_var, "' and '", Proportion_var, "' not found in the data frame."))
  }
  if (!is.numeric(df[[Proportion_var]])) {
    stop(paste0("'", Proportion_var, "' column must be numeric."))
  }
  if (!is.logical(label)) {
    stop("'label' must be a logical value (TRUE or FALSE).")
  }
  if (!is.numeric(size_adjust) || length(size_adjust) != 1 || size_adjust <= 0) {
    stop("'size_adjust' must be a single positive numeric value.")
  }

  df[[ID_var]] <- factor(df[[ID_var]], levels = df[[ID_var]])

  if (label) {
    df <- df %>%
      dplyr::arrange(dplyr::desc(!!sym(ID_var))) %>%
      dplyr::mutate(
        percentage = .data[[Proportion_var]] / sum(.data[[Proportion_var]]),
        label_y_pos = cumsum(.data$percentage) - 0.5 * .data$percentage
      )
  }

  if (is.null(colors)) {
    num_categories <- length(unique(df[[ID_var]]))
    colors <- hcl(h = seq(15, 375, length = num_categories + 1), l = 65, c = 100)[1:num_categories]
  } else if (length(colors) != length(unique(df[[ID_var]]))) {
    warning("Number of provided colors does not match the number of categories. Colors will be recycled or truncated.")
  }

  base_legend_text_size <- 13
  base_legend_title_size <- 14
  base_label_size <- 4

  p <- ggplot(df, aes(x = "", y = .data[[Proportion_var]], fill = .data[[ID_var]])) +
    geom_bar(width = 1, stat = "identity", color = "black") +
    coord_polar("y") +
    scale_fill_manual(values = colors) +
    theme(
      legend.background = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.text = element_markdown(color = "black", size = base_legend_text_size * size_adjust),
      legend.title = element_text(color = "black", size = base_legend_title_size * size_adjust)
    )

  if (label) {
    p <- p +
      geom_text(aes(y = .data$label_y_pos * sum(df[[Proportion_var]]),
                    label = paste0(.data[[ID_var]], " (", scales::percent(.data$percentage, accuracy = 0.1), ")")),
                color = "black",
                size = base_label_size * size_adjust,
                fontface = "bold")
  }

  return(p)
}
