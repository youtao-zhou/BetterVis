#' Experiment Barplot Visualization Function
#'
#' This function creates bar plots with extensive options for customization, including features like jittered points, faceting, and statistical comparisons.
#'
#' @name BetterVis_Barplot_Experiment
#' @description Provides functionality for generating bar plots with customizable aesthetics, error bars, jittering, and statistical comparisons, enabling detailed visual analysis of data.
#'
#' @param data A data frame containing the variables for plotting.
#' @param x_var A string specifying the variable for the x-axis, which should be a factor.
#' @param y_var A string specifying the numeric variable for the y-axis.
#' @param fill_var A string specifying the variable used for fill colors, which should also be a factor.
#' @param jitter Logical, indicating whether to display jittered points (default: \code{FALSE}).
#' @param jitter_color A vector specifying the colors for jittered points, matching the number of factor levels.
#' @param jitter_width Numeric, defines the width of the jittered points (default: \code{0.2}).
#' @param jitter_alpha Numeric, sets the transparency for jittered points (default: \code{0.7}).
#' @param jitter_size Numeric, specifies the size of the jittered points (default: \code{1.5}).
#' @param legend_show Logical, controls the display of the legend (default: \code{TRUE}).
#' @param legend_title A string for the legend title.
#' @param comparison_col A string specifying the variable to be used for comparison.
#' @param comparison_method A string specifying the statistical method for comparison (\code{"t.test"} or \code{"wilcox"}, default: \code{"t.test"}).
#' @param comparison A list of groups to be compared statistically.
#' @param barplot_color A vector specifying fill colors for the bars, with a length equal to the number of factor levels.
#' @param facet Logical, indicating whether to use faceting in the plot (default: \code{FALSE}).
#' @param facet_title_size Numeric, size of the facet titles.
#' @param y_label_bold Logical, determines whether the y-axis labels should be bold (default: \code{FALSE}).
#' @param axis_titles_y A string for the y-axis title, use "" if no title is desired.
#' @param x_label_angle Numeric, angle for the x-axis labels.
#' @param x_label_bold Logical, specifies if the x-axis labels should be bold (default: \code{FALSE}).
#' @param axis_titles_x A string for the x-axis title, use "" if no title is desired.
#' @param title A string for the plot title.
#' @param axis_title_bold Logical, determines if axis titles should be bold (default: \code{FALSE}).
#' @param error_bar_width Numeric, width of the error bars.
#' @param error_bar_size Numeric, thickness of the error bars.
#' @param bar_border Logical, indicates if a border should be added to the bars (default: \code{TRUE}).
#' @param bar_border_size Numeric, thickness of the bar borders.
#' @param background_color A string for the background color, default is \code{"white"}.
#' @param sig_type A string indicating the type of significance mark to use (\code{"SYMBOL"} or \code{"NUMBER"}).
#' @param ylim A numeric vector specifying the limits for the y-axis.
#' @param custom_theme, custom_guides Used for integration with additional theme or guide customization.
#'
#' @return A \code{ggplot} object representing the customized bar plot.
#' @importFrom ggpubr stat_compare_means
#' @importFrom ggplot2 ggplot aes geom_bar stat_summary geom_point position_jitter scale_fill_manual
#' @importFrom ggplot2 facet_wrap scale_y_continuous labs theme theme_classic element_text element_rect
#' @importFrom dplyr mutate
#' @importFrom cowplot plot_grid
#'
#' @examples
#' library(ggpubr)
#' library(ggplot2)
#' library(RColorBrewer)
#' library(dplyr)
#' library(cowplot)
#'
#' ##  Data Input
#' data("BetterVis_Barplot_Experiment_example", package = "BetterVis")
#'
#' ##  Simple Example
#' BetterVis_Barplot_Experiment(
#' data = BetterVis_Barplot_Experiment_example,x_var = "supp",y_var = "len",fill_var = "dose",
#' comparison_col = "dose",comparison_method = "t.test",
#' comparison = list(c("dose_1", "dose_2"),c("dose_1", "dose_3"),c("dose_2", "dose_3")),
#' barplot_color = c("#E84A32", "#4CBCD2", "#00A18A"),
#' facet = TRUE,
#' ylim = c(0,45)
#' )
#'
#' ## Advanced Example
#'
#' BetterVis_Barplot_Experiment(
#' data = BetterVis_Barplot_Experiment_example,x_var = "supp",y_var = "len",fill_var = "dose",
#' jitter = TRUE,jitter_color =c("black","black","black"),
#' jitter_width = 0.2,jitter_alpha = 0.7,jitter_size = 3,
#' legend_show = FALSE,legend_title = "Dose",
#' comparison_col = "dose",comparison_method = "t.test",
#' comparison = list(c("dose_1", "dose_2"),c("dose_1", "dose_3"),c("dose_2", "dose_3")),
#' barplot_color = c("#E84A32", "#4CBCD2", "#00A18A"),
#' facet = TRUE,facet_title_size = 16,
#' y_label_bold = TRUE,axis_titles_y = "",
#' x_label_angle = 45,x_label_bold = TRUE,axis_titles_x = c(""),
#' title = "",axis_title_bold = TRUE,
#' error_bar_width = 0.5,error_bar_size = 0.5,bar_border = TRUE,bar_border_size = 0.8,
#' background_color = "white",sig_type = "SYMBOL",ylim = c(0,45)
#' )+theme()+guides()
#'
#'
#' ## We have modified the iris dataset to create these boxplots
#' iris$Group <- rep(rep(paste0("Group", 1:5), each = 10),3)
#' iris$Group <-as.factor(iris$Group)
#' BetterVis_Barplot_Experiment(
#' data = iris,x_var = "Group",y_var = "Petal.Width",fill_var = "Species",
#' jitter = TRUE,jitter_color =c("black","black","black"),
#' jitter_width = 0.2,jitter_alpha = 0.7,jitter_size = 3,
#' legend_show = FALSE,legend_title = "Dose",
#' comparison_col = "Species",comparison_method = "t.test",
#' comparison = list(c("setosa", "versicolor"),c("versicolor", "virginica"),c("setosa", "virginica")),
#' barplot_color = c("#E84A32", "#4CBCD2", "#00A18A"),
#' facet = TRUE,facet_title_size = 16,
#' y_label_bold = TRUE,axis_titles_y = "",
#' x_label_angle = 45,x_label_bold = TRUE,axis_titles_x = c("a"),
#' title = "",axis_title_bold = TRUE,
#' error_bar_width = 0.5,error_bar_size = 0.5,bar_border = TRUE,bar_border_size = 0.8,
#' background_color = "white",sig_type = "SYMBOL",ylim = c(0,4)
#' )
#'
#' @export
BetterVis_Barplot_Experiment <- function(
    data, x_var, y_var, fill_var, jitter = FALSE, jitter_width = 0.2, jitter_color = NULL,
    jitter_alpha = 0.7, jitter_size = 1.5, legend_show = TRUE, legend_title = NULL,
    barplot_color = NULL, facet = FALSE, facet_title_size = 16, comparison = NULL,
    comparison_col = NULL, ylim = NULL, title = NULL, x_label_angle = 45, x_label_bold = FALSE,
    axis_titles_x = "", axis_titles_y = "Y Axis", axis_title_bold = FALSE, y_label_bold = FALSE,
    background_color = "white", bar_border = TRUE, sig_type = "SYMBOL", error_bar_width = 0.2,
    error_bar_size = 1, bar_border_size = 1, custom_theme = NULL, custom_guides = NULL,
    comparison_method = "t.test"  # New parameter for selecting comparison method
) {
  if (!all(c(x_var, y_var, fill_var) %in% colnames(data))) {
    stop("请检查 x_var, y_var 和 fill_var 是否为数据框的列名。")
  }

  if (!is.null(comparison_col) && !comparison_col %in% c(x_var, fill_var)) {
    stop("comparison_col 必须是 x_var 或 fill_var。")
  }

  data[[fill_var]] <- as.factor(data[[fill_var]])

  unique_levels <- levels(data[[fill_var]])

  # 动态生成 barplot_color 和 jitter_color，确保它们与 fill_var 的因子水平数一致
  if (is.null(barplot_color)) {
    color_palette <- colorRampPalette(c("#84bd00", "#efdf00", "#fe5000", "#e4002b",
                                        "#da1884", "#a51890", "#0077c8", "#008eaa"))(length(unique_levels))
    barplot_color <- setNames(color_palette, unique_levels)
  } else if (length(barplot_color) != length(unique_levels)) {
    stop("barplot_color 的长度应与 fill_var 的因子水平数相等。")
  }

  if (is.null(jitter_color)) {
    jitter_color <- barplot_color  # 如果未提供 jitter_color，则使用 barplot_color
  } else if (length(jitter_color) != length(unique_levels)) {
    stop("jitter_color 的长度应与 fill_var 的因子水平数相等。")
  }

  max_y <- max(data[[y_var]], na.rm = TRUE)
  min_y <- min(data[[y_var]], na.rm = TRUE)

  # 动态设置 ylim，如果用户没有指定，则设置最大值为 max_y + 5，最小值为 0
  if (is.null(ylim)) {
    ylim <- c(0, max_y + 5)  # 设置最大值为 max_y + 5
  }

  if (facet && comparison_col == fill_var) {
    x_axis_var <- fill_var
    facet_var <- x_var
  } else {
    x_axis_var <- x_var
    facet_var <- NULL
  }

  # Main plot
  p <- ggplot(data, aes_string(x = x_axis_var, y = y_var, fill = fill_var)) +
    # Bar plot
    geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.7,
             color = "black", size = bar_border_size, show.legend = TRUE) +  # Ensure legend for fill
    # Error bars
    stat_summary(
      fun.data = function(x) {
        data.frame(
          y = mean(x),
          ymin = mean(x) - sd(x),
          ymax = mean(x) + sd(x)
        )
      },
      geom = "errorbar", width = error_bar_width, size = error_bar_size, position = position_dodge(0.7)
    )

  # Jitter points (optional)
  if (jitter) {
    p <- p + geom_point(
      position = position_jitter(width = jitter_width),
      color = unname(jitter_color[data[[fill_var]]]),  # Explicitly use jitter_color mapped to fill_var
      alpha = jitter_alpha,
      size = jitter_size, show.legend = TRUE  # Ensure legend for jitter points
    )
  }

  # Comparisons
  if (!is.null(comparison) && !is.null(comparison_col)) {
    if (comparison_col == fill_var) {
      if (facet) {
        p <- p + stat_compare_means(
          comparisons = comparison,
          method = comparison_method,  # Use selected comparison method
          label = ifelse(sig_type == "NUMBER", "p.format", "p.signif"),
          tip.length = 0.01,
          size = 5
        )
      } else {
        max_y_fill <- aggregate(data[[y_var]], by = list(data[[fill_var]]), FUN = mean)
        max_y_fill$adjusted_y <- max_y_fill$x + 0.1 * max_y_fill$x
        for (i in seq_along(comparison)) {
          current_comparison <- comparison[[i]]
          y_pos <- max(max_y_fill$adjusted_y[match(current_comparison, max_y_fill$Group.1, nomatch = 0)])
          p <- p + stat_compare_means(
            comparisons = list(current_comparison),
            method = comparison_method,  # Use selected comparison method
            label = ifelse(sig_type == "NUMBER", "p.format", "p.signif"),
            label.y = y_pos,
            tip.length = 0.01,
            size = 5
          )
        }
      }
    } else if (comparison_col == x_var) {
      p <- p + stat_compare_means(
        comparisons = comparison,
        method = comparison_method,  # Use selected comparison method
        label = ifelse(sig_type == "NUMBER", "p.format", "p.signif"),
        label.y = max_y + 5,  # Adjust label y-position
        tip.length = 0.01,
        size = 5
      )
    } else {
      warning("当 use_facet = TRUE 且 comparison_col 不等于 fill_var 时，比较标注可能无法正确添加。")
    }
  }

  # Faceting
  if (facet && !is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_x") +
      theme(
        strip.text.x = element_text(size = facet_title_size, face = "bold")  # Use facet_title_size to control facet title size
      )
  }

  # Y-axis limits
  p <- p + scale_y_continuous(limits = ylim, expand = c(0, 0))

  # Fill color
  p <- p + scale_fill_manual(values = barplot_color)

  # Legend
  p <- p + theme(
    legend.position = ifelse(legend_show, "right", "none")  # 控制图例位置
  )

  if (!is.null(legend_title)) {
    p <- p + labs(fill = legend_title)
  }

  # Adjust x-axis label position
  if (x_label_angle == 45) {
    x_label_adjustment <- 1  # Move labels up a little when the angle is 45
  } else {
    x_label_adjustment <- 0
  }

  p <- p + theme_classic() +
    theme(
      legend.position = ifelse(legend_show, "right", "none"),
      strip.background = element_rect(fill = "gray90", color = "gray80"),
      strip.text = element_text(color = "black", size = facet_title_size, face = "bold"),
      strip.placement = "outside",
      axis.text.x = element_text(
        color = "black",
        angle = x_label_angle,
        vjust = x_label_adjustment,
        hjust = 1,
        size = 15,
        face = ifelse(x_label_bold, "bold", "plain")
      ),
      axis.text.y = element_text(color = "black", size = 15),
      axis.line = element_line(size = 1),
      axis.ticks = element_line(color = "black", size = 1),
      axis.title = element_text(color = "black", size = 18, face = ifelse(axis_title_bold, "bold", "plain")),
      axis.ticks.length.x = unit(0.2, "cm"),
      panel.background = element_rect(fill = background_color),
      plot.background = element_rect(fill = background_color)
    ) +
    labs(x = ifelse(axis_titles_x == "", "", axis_titles_x),
         y = ifelse(axis_titles_y == "", "", axis_titles_y),
         title = title)

  # Add custom theme and guides if provided
  if (!is.null(custom_theme)) {
    p <- p + custom_theme
  }

  if (!is.null(custom_guides)) {
    p <- p + custom_guides
  }

  return(p)
}
