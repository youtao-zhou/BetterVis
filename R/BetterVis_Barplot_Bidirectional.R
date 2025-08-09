#' Bidirectional Barplot Visualization Function
#'
#' Creates bidirectional bar plots with extensive customization options. This function provides users with the ability to tailor themes, axes, titles, legends, and additional graphic elements such as arrows and annotations.
#'
#' @name BetterVis_Barplot_Bidirectional
#' @description Generates bidirectional bar plots meant to display data comparison across categories with customized aesthetics and additional annotations to highlight specific data insights.
#'
#' @param data A data frame containing the variables for plotting.
#' @param x_var A string specifying the variable for the x-axis (should be a factor variable).
#' @param y_var A string specifying the numeric variable for the y-axis.
#' @param fill_var A string specifying the variable used for fill colors (should be a factor variable).
#' @param barplot_color A vector specifying the fill colors corresponding to each level of the fill_var factor.
#' @param axis_titles_x A string for the x-axis title (default: \code{""}).
#' @param x_label_angle Numeric, indicating the angle for x-axis labels.
#' @param x_label_bold Logical, indicating if x-axis labels should be bold (default: \code{FALSE}).
#' @param axis_title_x_size Numeric, defining the font size of the x-axis title.
#' @param axis_titles_y A string for the y-axis title (default: \code{""}).
#' @param y_label_bold Logical, indicating if y-axis labels should be bold (default: \code{FALSE}).
#' @param y_label_angle Numeric, indicating the angle for y-axis labels.
#' @param axis_title_y_size Numeric, defining the font size of the y-axis title.
#' @param title Character, the main title of the plot (default: \code{""}).
#' @param title_size Numeric, defines the font size of the title (default: \code{14}).
#' @param title_bold Logical, indicating if the title should be bold (default: \code{FALSE}).
#' @param legend_show Logical, indicating if the legend should be shown (default: \code{TRUE}).
#' @param legend_title Character, the title for the legend (default: \code{""}).
#' @param legend_size Numeric, specifying the font size of the legend text.
#' @param background_color Character, the background color of the plot (default: \code{"white"}).
#' @param ylim A numeric vector to set the y-axis limits.
#' @param arrow Logical, whether to display arrows (default: \code{FALSE}).
#' @param plot_margin A \code{unit} object specifying the plot margin.
#' @param arrow_x1, arrow_x2, arrow_y1, arrow_y2 Numeric vectors defining the start and end points of arrows.
#' @param arrow_col Character, color of the arrows.
#' @param arrow_size Numeric, size of the arrows.
#' @param arrow_text A character vector of length 2, providing the text for labels adjacent to arrows.
#' @param arrow_text_size Numeric, size of the text adjacent to arrows.
#' @param arrow_text_adjust Numeric, vertical adjustment for text adjacent to arrows.
#' @param right_text Logical, whether to display text in the lower right (default: \code{FALSE}).
#' @param right_text1, right_text2 Characters, content for the text displayed in the lower right.
#' @param right_text_hjust, right_text_vjust Numeric adjustments for horizontal and vertical placement of right-side text.
#' @param right_text_size Numeric, size of the right-side text.
#'
#' @return A \code{ggplot} object with the customized bidirectional bar plot.
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual theme_bw theme element_text element_blank element_rect coord_cartesian ggtitle annotate annotation_custom
#' @importFrom grid unit
#' @importFrom cowplot ggdraw draw_plot draw_line

#'
#' @examples
#' library(BetterVis)
#' library(grid)
#'
#' ##  Data Input
#' data("BetterVis_Barplot_Bidirectional_example", package = "BetterVis")
#'
#' ## Simple Example
#' BetterVis_Barplot_Bidirectional(
#' data = BetterVis_Barplot_Bidirectional_example,x_var = "Percentage", y_var = "pathway", fill_var = "Species",
#' barplot_color = c("#ABDDDE", "#FAD510", "#C6CDF7", "#F4B5BD", "#0A9F9D", "#FAEED1", "#005295", "#E6A0C4", "#C52E19", "orange"),
#' title = "Species Contribution to Pathways",legend_title = "Species Contribution",
#' ylim = c(1, 8),
#' arrow = FALSE,plot_margin = unit(c(1,0.3,0.3,0.3),unit="cm"))
#'
#'
#'
#' ##  Advanced Example
#' BetterVis_Barplot_Bidirectional(
#' data = BetterVis_Barplot_Bidirectional_example,x_var = "Percentage", y_var = "pathway", fill_var = "Species",
#' barplot_color = c("#ABDDDE", "#FAD510", "#C6CDF7", "#F4B5BD", "#0A9F9D",
#'                     "#FAEED1", "#005295", "#E6A0C4", "#C52E19", "orange"),
#' axis_titles_x = "", x_label_angle = 0, x_label_bold = TRUE, axis_title_x_size = 14,
#' axis_titles_y = "", y_label_bold = TRUE, y_label_angle = 0,axis_title_y_size = 14,
#' title = "Species Contribution to Pathways", title_size = 16, title_bold = TRUE,
#' legend_show = TRUE, legend_title = "Species Contribution", legend_size = 10,
#' background_color = "white",
#' ylim = c(1, 8),
#' plot_margin = unit(c(2,0.3,0.3,0.3),unit="cm"),
#' arrow = TRUE,
#' arrow_x1 = c(0.35, 0.2), arrow_x2 = c(0.48, 0.63),
#' arrow_y1 = c(0.88, 0.88), arrow_y2 = c(0.88, 0.88),
#' arrow_col = "black", arrow_size = 0.5,
#' arrow_text = c("Enriched in Enriched in Disease Group","Enriched in Control Group"),arrow_text_size = 12,arrow_text_adjust = 0.03,
#' right_text = TRUE,right_text1 = "n = 154 for Disease",right_text2 = "n = 192 for Control",right_text_hjust = 1.2,right_text_vjust = -3,right_text_size =4
#' )
#'
#' ## We have modified the iris dataset to create these boxplots
#' iris$Group <- c(rep("Group2",75),rep("Group1",75))
#' iris$Group <-as.factor(iris$Group)
#' iris$Group2 <- rep(rep(paste0("Group", 1:10), each = 5),3)
#' iris$Group2 <-as.factor(iris$Group2)
#' BetterVis_Barplot_Bidirectional(
#' data = iris,x_var = "Petal.Width",  y_var = "Group2", fill_var = "Species",
#' barplot_color = c("#ABDDDE", "#FAD510", "#C6CDF7", "#F4B5BD", "#0A9F9D","#FAEED1", "#005295", "#E6A0C4", "#C52E19", "orange"),
#' axis_titles_x = "", x_label_angle = 0, x_label_bold = TRUE, axis_title_x_size = 14,
#' axis_titles_y = "", y_label_bold = TRUE, y_label_angle = 0,axis_title_y_size = 14,
#' title = "Species Contribution to Pathways", title_size = 16, title_bold = TRUE,
#' legend_show = TRUE, legend_title = "Species Contribution", legend_size = 10,
#' background_color = "white",
#' ylim = c(1, 10),
#' plot_margin = unit(c(2,2,1,0.5),unit="cm"),
#' arrow = TRUE,
#' arrow_x1 = c(0.35, 0.2), arrow_x2 = c(0.48, 0.63),
#' arrow_y1 = c(0.88, 0.88), arrow_y2 = c(0.88, 0.88),
#' arrow_col = "black", arrow_size = 0.5,
#' arrow_text = c("Enriched in stable low DDS","Enriched in stable high DDS"),arrow_text_size = 12,arrow_text_adjust = 0.03,right_text = TRUE,right_text1 = "n = 154 for stable high DDS",right_text2 = "n = 192 for stable low  DDS",right_text_hjust = 1.2,right_text_vjust = -6,right_text_size =4)
#' # +theme(axis.title.x = element_blank())+guides(fill=FALSE)



#' @export
BetterVis_Barplot_Bidirectional <- function(data, x_var, y_var, fill_var, barplot_color,
                                            x_label_angle = 0, x_label_bold = FALSE,
                                            axis_titles_x = "", axis_title_x_size = 12,
                                            y_label_bold = FALSE, axis_titles_y = "",
                                            y_label_angle = 0, axis_title_y_size = 12,
                                            title = "", title_size = 14, title_bold = FALSE,
                                            legend_show = TRUE, legend_title = "",
                                            legend_size = 10,
                                            background_color = "white", ylim = c(NA, NA),
                                            arrow = FALSE,
                                            plot_margin = unit(c(2, 0.3, 0.3, 0.3), "cm"),
                                            arrow_x1 = c(0.35, -0.2), arrow_x2 = c(0.48, 0.63),
                                            arrow_y1 = c(0.88, 0.88), arrow_y2 = c(0.88, 0.88),
                                            arrow_col = "black", arrow_size = 1,
                                            arrow_text = c("Stable Low DDS", "Stable High DDS"),
                                            arrow_text_size = 2, arrow_text_adjust = 0,
                                            right_text = FALSE, right_text1 = "", right_text2 = "",
                                            right_text_hjust = 1, right_text_vjust = 1,
                                            right_text_size = 12
) {

  # 基础柱状图
  plot <- ggplot(data, aes_string(x = x_var, y = y_var, fill = fill_var)) +
    geom_col(color = "black", size = 0.5) +
    scale_fill_manual(values = barplot_color) +
    theme_bw() +
    theme(axis.ticks = element_blank(),
          axis.text = element_text(color = "black"),
          plot.margin = plot_margin)  # 使用 plot_margin 参数设置边距

  # X轴标签倾斜
  plot <- plot + theme(axis.text.x = element_text(angle = x_label_angle, hjust = 1))

  # X轴标签加粗
  if (x_label_bold) {
    plot <- plot + theme(axis.text.x = element_text(face = "bold"))
  }

  # 设置X轴标题
  plot <- plot + xlab(axis_titles_x)

  # 设置X轴标题字体大小
  plot <- plot + theme(axis.title.x = element_text(size = axis_title_x_size))

  # Y轴标签倾斜
  plot <- plot + theme(axis.text.y = element_text(angle = y_label_angle, hjust = 1))

  # Y轴标签加粗
  if (y_label_bold) {
    plot <- plot + theme(axis.text.y = element_text(face = "bold"))
  }

  # 设置Y轴标题
  plot <- plot + ylab(axis_titles_y)

  # 设置Y轴标题字体大小
  plot <- plot + theme(axis.title.y = element_text(size = axis_title_y_size))

  # 设置图表标题
  plot <- plot + ggtitle(title)

  # 图表标题加粗
  if (title_bold) {
    plot <- plot + theme(plot.title = element_text(face = "bold"))
  }

  # 设置标题字体大小
  plot <- plot + theme(plot.title = element_text(size = title_size))

  # 图例相关设置
  if (legend_show) {
    plot <- plot +
      theme(legend.title = element_text(size = legend_size, face = "bold"),
            legend.text = element_text(size = legend_size)) +
      guides(fill = guide_legend(title = legend_title))
  } else {
    plot <- plot + theme(legend.position = "none")
  }

  # 设置背景颜色
  plot <- plot + theme(plot.background = element_rect(fill = background_color, color = NA))

  # 设置Y轴范围
  plot <- plot + coord_cartesian(ylim = ylim)

  # 如果 arrow = TRUE，则绘制箭头
  if (arrow) {

    # 计算箭头1的中点
    arrow_text_x1 <- (arrow_x1[1] + arrow_x1[2]) / 2
    arrow_text_y1 <- (arrow_y1[1] + arrow_y1[2]) / 2

    # 计算箭头2的中点
    arrow_text_x2 <- (arrow_x2[1] + arrow_x2[2]) / 2
    arrow_text_y2 <- (arrow_y2[1] + arrow_y2[2]) / 2

    # 根据 arrow_text_adjust 调整文字的纵向位置
    arrow_text_y1 <- arrow_text_y1 + arrow_text_adjust
    arrow_text_y2 <- arrow_text_y2 + arrow_text_adjust

    # 绘制图形并加上箭头
    plot_with_arrows <- ggdraw() +
      draw_plot(plot) +
      # 绘制第一个箭头
      draw_line(x = c(arrow_x1[1], arrow_x1[2]), y = c(arrow_y1[1], arrow_y1[2]),
                arrow = arrow(type = "closed", length = unit(0.2, "inches")),
                size = arrow_size, col = arrow_col, lineend = "square") +
      # 绘制第二个箭头
      draw_line(x = c(arrow_x2[1], arrow_x2[2]), y = c(arrow_y1[1], arrow_y2[1]),
                arrow = arrow(type = "closed", length = unit(0.2, "inches")),
                size = arrow_size, col = arrow_col, lineend = "square") +
      # 添加第一个箭头的文字
      annotation_custom(
        grob = textGrob(arrow_text[1], gp = gpar(fontsize = arrow_text_size)),
        xmin = arrow_text_x1 - 0.05, xmax = arrow_text_x1 + 0.05,
        ymin = arrow_text_y1 + 0.03, ymax = arrow_text_y1 + 0.03
      ) +
      # 添加第二个箭头的文字
      annotation_custom(
        grob = textGrob(arrow_text[2], gp = gpar(fontsize = arrow_text_size)),
        xmin = arrow_text_x2 - 0.05, xmax = arrow_text_x2 + 0.05,
        ymin = arrow_text_y2 + 0.03, ymax = arrow_text_y2 + 0.03
      )
  } else {
    plot_with_arrows <- plot
  }

  # 如果需要右下角文本，则添加文本
  if (right_text) {
    plot_with_arrows <- plot_with_arrows +
      annotate("text", x = 1, y = 0, label = right_text1,
               hjust = right_text_hjust, vjust = right_text_vjust,
               size = right_text_size) +
      annotate("text", x = 1, y = 0.05, label = right_text2,
               hjust = right_text_hjust, vjust = right_text_vjust,
               size = right_text_size)
  }
  # 返回最终的图形
  return(plot_with_arrows)
}
