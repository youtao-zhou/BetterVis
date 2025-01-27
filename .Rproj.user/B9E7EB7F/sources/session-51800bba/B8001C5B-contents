#' #' BetterVis Boxplot Function
#' @name BetterVis_Boxplot1
#' @description Creates enhanced boxplots with customization options for ggplot2.
#' @param data A data frame.
#' @param x_var A string specifying the x-axis variable.
#' @param y_var A string specifying the y-axis variable.
#' @param fill_var A string specifying the fill variable.
#' @param use_facet Logical. Whether to use facets in the plot.
#' @param title Optional. The title of the plot.
#' @param x_label_angle Numeric. Angle of x-axis labels (default: 45 degrees).
#' @param x_label_bold Logical. Whether to bold the x-axis labels.
#' @param y_label_bold Logical. Whether to bold the y-axis labels.
#' @param axis_titles Character vector. Titles for x and y axes.
#' @param axis_title_bold Logical. Whether to bold axis titles.
#' @param custom_colors Optional. Custom colors for the fill variable.
#' @param jitter Logical. Whether to add jitter to the points.
#' @param jitter_color Optional. Custom colors for jitter points.
#' @param jitter_width Numeric. Width of jitter (default: 0.1).
#' @param jitter_alpha Numeric. Transparency of jitter points (default: 1).
#' @param jitter_size Numeric. Size of jitter points (default: 1).
#' @param legend_show Logical. Whether to show the legend (default: TRUE).
#' @param legend_title Optional. Title of the legend.
#' @param box_width Numeric. Width of the boxplots (default: 0.75).
#' @param comparison_col Character. Column used for statistical comparison.
#' @param comparison List. Comparisons for statistical testing.
#' @param comparison_method Character. Statistical test method ("wilcox" or "t.test").
#' @param box_background Logical. Whether to include a background for the boxplot.
#' @param jitter_legend Logical. Whether jitter points should appear in the legend.
#' @param fill_legend Logical. Whether the fill variable should appear in the legend.
#' @param background_color Character. Color of the plot background.
#' @param background_border Logical. Whether to include a border around the background.
#' @param sig_type Character. Type of significance label ("SYMBOL" or "p.format").
#' @param guides_modifications Optional. Custom modifications to guides.
#' @param violin Logical. Whether to include a violin layer (default: FALSE).
#' @param ... Additional arguments passed to ggplot2 layers.
#' @return A ggplot object.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_violin geom_jitter
#' @importFrom ggplot2 position_dodge position_jitterdodge scale_fill_manual scale_color_manual
#' @importFrom ggplot2 ggtitle theme element_text element_rect ylab xlab facet_wrap ylim
#' @importFrom RColorBrewer brewer.pal
#' @importFrom cowplot theme_cowplot
#' @importFrom ggpubr stat_compare_means
#' @importFrom grid unit
#' @examples
#'
#' library(ggpubr)
#' library(ggplot2)
#' library(RColorBrewer)
#' library(dplyr)
#' library(cowplot)
#'
#' library(BetterVis)
#'
#' data("boxplot_example_1", package = "BetterVis")
#' df <- boxplot_example_1
#' df$supp<-as.factor(df$supp)
#' df$dose<-as.factor(df$dose)
#'
#' set.seed(123)  # 设置随机种子以保证结果可复现
#' random_sample <- sample(1:40000, 1000, replace = FALSE)
#' df <- data.frame(
#'   dose = c(rep("dose1",30), rep("dose2",30),rep("dose3",30),rep("dose1",30), rep("dose2",30),rep("dose3",30)),
#'   supp = c(rep("OJ",90),rep("VC",90)),
#'   len = c(sample(1:100, 90),sample(200:300, 90))
#' )
#'
#'
#'
#'
#'
#'
#' BetterVis_Boxplot1(
#'   df, x_var = "supp", y_var = "len", fill_var = "dose",
#'   use_facet = FALSE,
#'   title = "",
#'   x_label_angle = 45,
#'   x_label_bold = TRUE,
#'   y_label_bold = FALSE,
#'   axis_titles = c("X Axis", "Y Axis"),
#'   axis_title_bold = TRUE,
#'   custom_colors = c("#FC5D5C", "#4B6A9D", "#167D7D"),
#'   jitter = TRUE,
#'   jitter_color = c("black", "black", "black"),
#'   legend_show = TRUE,
#'   legend_title = "",
#'   box_width = 0.6,
#'   comparison_col = "supp",
#'   comparison = list(c("OJ", "VC")),
#'   comparison_method = "t.test",
#'   box_background = TRUE,background_color = "white",background_border = TRUE,sig_type = "SYMBOL"
#' )
#'
#' BetterVis_Boxplot1(
#'   data=df, x_var = "supp", y_var = "len", fill_var = "dose",
#'   use_facet = TRUE,
#'   title = "",
#'   x_label_angle = 45,
#'   x_label_bold = TRUE,
#'   y_label_bold = TRUE,
#'   axis_titles = c("X Axis", "Y Axis"),
#'   axis_title_bold = TRUE,
#'   custom_colors = c("#FC5D5C", "#4B6A9D", "#167D7D"),
#'   jitter = TRUE,
#'   jitter_color = c("#FC5D5C", "#4B6A9D", "#167D7D"),
#'   jitter_width = 1,
#'   jitter_alpha = 1,
#'   jitter_size = 2,
#'   legend_show = FALSE,
#'   legend_title = "",
#'   box_width = 0.6,
#'   comparison_col = "dose",
#'   comparison = list(c("dose_1", "dose_2"),c("dose_1", "dose_3"),c("dose_2", "dose_3")),
#'   comparison_method = "t.test",
#'   box_background = FALSE,background_color = "white",background_border = TRUE,sig_type = "p.signif",violin = TRUE
#' )
# +theme(axis.title.x = element_blank())+guides(fill=FALSE)

#' @export


utils::globalVariables(".data")
BetterVis_Boxplot1 <- function(data, x_var, y_var, fill_var, use_facet = FALSE,
                               title = NULL, x_label_angle = 45, x_label_bold = FALSE, y_label_bold = FALSE,
                               axis_titles = c("", ""), axis_title_bold = FALSE, custom_colors = NULL,
                               jitter = TRUE, jitter_color = NULL, jitter_width = 0.1, jitter_alpha = 1, jitter_size = 1,
                               legend_show = TRUE, legend_title = NULL,
                               box_width = 0.75, comparison_col = NULL, comparison = NULL, comparison_method = "wilcox",
                               box_background = TRUE,
                               jitter_legend = FALSE, fill_legend = TRUE,
                               background_color = "white", background_border = TRUE, sig_type = "SYMBOL",
                               guides_modifications = NULL, violin = FALSE) {
  if (!all(c(x_var, y_var, fill_var) %in% colnames(data))) {
    stop("确保 x_var, y_var 和 fill_var 是提供的数据框中的列名。")
  }
  if (!is.numeric(data[[y_var]])) {
    data[[y_var]] <- as.numeric(as.character(data[[y_var]]))
  }
  data <- data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]), !is.na(.data[[fill_var]]))

  x_title <- ifelse(axis_titles[1] == "", "", axis_titles[1])
  y_title <- ifelse(axis_titles[2] == "", "", axis_titles[2])

  x_title_bold <- ifelse(axis_title_bold, "bold", "plain")
  y_title_bold <- ifelse(axis_title_bold, "bold", "plain")

  unique_fill_levels <- length(unique(data[[fill_var]]))
  if (!is.null(custom_colors)) {
    if (length(custom_colors) < unique_fill_levels) {
      stop("颜色数量不足以覆盖所有 fill_var 的水平。")
    }
    fill_colors <- custom_colors
  } else {
    fill_colors <- brewer.pal(min(max(unique_fill_levels, 3), 12), "Paired")
  }

  if (is.null(jitter_color)) {
    jitter_color <- fill_colors
  }

  if (use_facet && comparison_col == fill_var) {
    plot_aes <- aes(x = .data[[fill_var]], y = .data[[y_var]], fill = .data[[fill_var]])
  } else {
    plot_aes <- aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])
  }

  if (violin) {
    p <- ggplot(data, plot_aes) +
      geom_violin(position = position_dodge(width = box_width), alpha = 0.6) +
      geom_boxplot(width = 0.2, position = position_dodge(width = box_width), outlier.shape = NA)
  } else if (box_background) {
    p <- ggplot(data, plot_aes) +
      geom_boxplot(outlier.shape = NA, position = position_dodge(width = box_width), width = box_width)
  } else {
    p <- ggplot(data, plot_aes) +
      geom_boxplot(outlier.shape = NA, position = position_dodge(width = box_width), width = box_width,
                   aes(color = .data[[fill_var]]), fill = NA) +
      scale_color_manual(values = jitter_color, guide = ifelse(jitter_legend, "legend", "none"))
  }

  if (jitter) {
    p <- p + geom_jitter(position = position_jitterdodge(jitter.width = jitter_width, dodge.width = box_width),
                         size = jitter_size, alpha = jitter_alpha, aes(color = .data[[fill_var]])) +
      scale_color_manual(values = jitter_color, guide = ifelse(jitter_legend, "legend", "none"))
  }

  p <- p + scale_fill_manual(values = fill_colors, name = legend_title, guide = ifelse(fill_legend, "legend", "none"))

  y_min <- min(data[[y_var]], na.rm = TRUE)
  y_max <- max(data[[y_var]], na.rm = TRUE)
  y_range <- y_max - y_min
  p <- p + ylim(y_min - 0.1 * y_range, y_max + 0.2 * y_range)

  if (!is.null(title)) {
    p <- p + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
  }

  p <- p + ylab(y_title) + xlab(x_title)

  p <- p + theme_cowplot() +
    theme(
      legend.position = ifelse(legend_show, "right", "none"),
      axis.title.x = element_text(size = 15, face = ifelse(x_title != "", x_title_bold, "plain")),
      axis.title.y = element_text(size = 15, face = ifelse(y_title != "", y_title_bold, "plain")),
      axis.text = element_text(size = 15),
      axis.text.x = element_text(angle = x_label_angle, hjust = 1, face = ifelse(x_label_bold, "bold", "plain")),
      axis.text.y = element_text(face = ifelse(y_label_bold, "bold", "plain")),
      plot.margin = unit(c(0.5, 1, 0, 1), "cm"),
      plot.background = element_rect(fill = background_color,
                                     color = ifelse(background_border, "black", "white"),
                                     size = ifelse(background_border, 1, 0))
    ) +
    theme(panel.border = element_rect(color = ifelse(background_border, "black", NA), fill = NA, size = ifelse(background_border, 1, 0)))

  if (!is.null(guides_modifications)) {
    p <- p + guides_modifications
  }

  if (!is.null(comparison_col) && !is.null(comparison)) {
    if (!(comparison_col %in% c(x_var, fill_var))) {
      stop("comparison_col 必须是 x_var 或 fill_var 之一。")
    }

    if (comparison_method == "wilcox") {
      method <- "wilcox.test"
    } else if (comparison_method == "t.test") {
      method <- "t.test"
    } else {
      stop("不支持的比较方法。请选择 'wilcox' 或 't.test'。")
    }

    if (use_facet && comparison_col == fill_var) {
      p <- p + stat_compare_means(
        comparisons = comparison,
        method = comparison_method,
        label = ifelse(sig_type == "SYMBOL", "p.signif", "p.format"),
        tip.length = 0.01,
        size = 5
      )
    } else if (comparison_col == x_var && !use_facet) {
      p <- p + stat_compare_means(
        comparisons = comparison,
        method = comparison_method,
        label = ifelse(sig_type == "SYMBOL", "p.signif", "p.format"),
        label.y = y_max + 0.05 * y_range,
        tip.length = 0.01,
        size = 5
      )
    } else {
      warning("当 use_facet = TRUE 且 comparison_col 不等于 fill_var 时，比较标注可能无法正确添加。")
    }
  }

  if (use_facet) {
    p <- p + facet_wrap(~ .data[[x_var]], scales = "free_x")
  }

  return(p)
}
