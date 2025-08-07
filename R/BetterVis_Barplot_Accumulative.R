#' BetterVis Accumulative Barplot Function
#'
#' Creates accumulative bar plots with extensive customization options, allowing users to visualize both numeric and percentage data across different categories.
#' The function provides various options for customizing themes, axes, titles, and legend settings.
#'
#' @name BetterVis_Barplot_Accumulative
#' @description Generates accumulative bar plots to showcase numerical distribution and sample ratios with comprehensive customization and layout options.
#'
#' @param data A data frame containing the variables for plotting.
#' @param x_var A string specifying the variable for the x-axis, which should be a factor variable.
#' @param y1_var A string specifying the numeric variable for the y-axis of the first plot.
#' @param y2_var A string specifying the numeric variable for the y-axis of the second plot.
#' @param fill_var A string specifying the variable used for fill colors, which should be a factor variable.
#' @param display An integer specifying which plot(s) to display: 1 for the first plot, 2 for the second plot, 3 for both plots.
#' @param barplot_color A vector specifying the fill colors corresponding to each level of the fill_var factor.
#' @param ylim1 A numeric vector, adjusts the y-axis limits for the first plot (optional).
#' @param ylim2 A numeric vector, adjusts the y-axis limits for the second plot (optional).
#' @param axis_titles_x A string for the x-axis title (default: \code{""}).
#' @param x_label_angle Numeric, defines the angle for x-axis labels.
#' @param x_label_bold Logical, determines if x-axis labels should be bold (default: \code{FALSE}).
#' @param axis_title_x_size Numeric, specifies the font size for the x-axis title.
#' @param axis_titles_y1 A string for the y-axis title of the first plot.
#' @param axis_titles_y2 A string for the y-axis title of the second plot.
#' @param y1_label_bold Logical, determines if the y-axis labels on the first plot should be bold (default: \code{FALSE}).
#' @param y2_label_bold Logical, determines if the y-axis labels on the second plot should be bold (default: \code{FALSE}).
#' @param axis_title_y1_size Numeric, specifies the font size for the y-axis title on the first plot.
#' @param axis_title_y2_size Numeric, specifies the font size for the y-axis title on the second plot.
#' @param title1 A string for the title of the first plot.
#' @param title2 A string for the title of the second plot.
#' @param title_bold Logical, determines if plot titles should be bold (default: \code{FALSE}).
#' @param title_size Numeric, specifies the font size for plot titles.
#' @param title1_size Numeric, specifies the font size for the first plot title.
#' @param title2_size Numeric, specifies the font size for the second plot title.
#' @param title1_bold Logical, determines if the first plot title should be bold.
#' @param title2_bold Logical, determines if the second plot title should be bold.
#' @param legend_show Logical, determines if the legend should be displayed (default: \code{TRUE}).
#' @param legend_title A string for the legend title.
#' @param legend_size Numeric, specifies the font size for legend text.
#' @param legend_position A string specifying the position of the legend, default is \code{"bottom"}.
#' @param background_color A string for the background color, default is \code{"white"}.
#' @param coord_flip Logical, determines if the coordinates should be flipped (default: \code{FALSE}).
#' @param grid_ncol Numeric, specifies the number of columns for arranging plots (default: \code{2}).
#' @param grid_layout Numeric matrix, defines the layout for arranging plots.
#'
#' @return A plot or arranged plots according to specified options.
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual labs theme_classic
#' @importFrom ggplot2 element_text element_line element_rect coord_flip scale_y_continuous
#' @importFrom dplyr %>%
#' @importFrom grid unit
#' @importFrom gridExtra grid.arrange

#'
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#' library(grid)
#' library(gridExtra)
#' library(plyr)
#'
#'
#' ##  示例数据1
#' data("BetterVis_Barplot_Accumulative_example", package = "BetterVis")
#' df <- BetterVis_Barplot_Accumulative_example
#' df$celltype <- factor(df$celltype,levels=rev(unique(df$celltype)))
#' df$Patient <- factor(df$Patient,levels=c("P21","P48","P53","P54","P08")) #指定柱内顺序
#' df = ddply(df,'celltype',transform,percent=cell_num/sum(cell_num))
#'
#' barplot_color = c("#ABDDDE", "#FAD510", "#C6CDF7", "#F4B5BD",  "#FAEED1",
#' "#0A9F9D",  "#005295", "#E6A0C4", "#C52E19", "orange")
#'
#'
#' ## 示例数据2
#' iris$Group <- rep(paste0("Group", 1:10), times = 15)
#' iris$Group <-as.factor(iris$Group)
#' iris$Group2 <- rep(rep(paste0("Color", 1:5), each = 10),3)
#' iris$Group2 <- sample(iris$Group2)
#' iris$Group2 <-as.factor(iris$Group2)
#' iris<-iris%>%dplyr::select(Petal.Width,Group2,Group)
#' iris<-distinct(iris,Group,Group2,.keep_all = TRUE)
#' iris = ddply(iris,'Group',transform,percent=Petal.Width/sum(Petal.Width))
#'
#' BetterVis_Barplot_Accumulative(
#'   data = BetterVis_Barplot_Accumulative_example, x_var = "celltype", y1_var = "cell_num", y2_var = "percent", fill_var = "Patient",
#'   display = 3, barplot_color = barplot_color,
#'   ylim1 = c(0, 15000), ylim2 = c(0, 1),
#'   coord_flip = TRUE
#' )
#' BetterVis_Barplot_Accumulative(
#'   data = df, x_var = "celltype", y1_var = "cell_num", y2_var = "percent", fill_var = "Patient",
#'   display = 3, barplot_color = barplot_color,
#'   ylim1 = c(0, 15000), ylim2 = c(0, 1),
#'   axis_titles_x = "Cell Types", x_label_angle = 0, x_label_bold = TRUE, axis_title_x_size = 14,
#'   axis_titles_y1 = "Cell Count", axis_titles_y2 = "Sample Ratio",
#'   y1_label_bold = TRUE,y2_label_bold =  TRUE, axis_title_y1_size = 14,axis_title_y2_size = 14,
#'   title1 = "Cell Number Distribution", title2 = "Sample Ratio Distribution",
#'   title_size = 10,title_bold = TRUE,
#'   legend_show = TRUE, legend_title = "Legend", legend_size = 14, legend_position = "bottom", legend_row = NULL,legend_col=NULL,
#'   background_color = "white",
#'   coord_flip = TRUE,
#'   grid_ncol = 2,grid_layout  = rbind(c(1, 1, 1, 1, 1, 2, 2),
#'                                                   c(1, 1, 1, 1, 1, 2, 2)),
#'   p1_theme = NULL,p2_theme = NULL,p1_guide = NULL,p2_guide = NULL
#' )
#' BetterVis_Barplot_Accumulative(
#'   data = iris, x_var = "Group", y1_var = "Petal.Width", y2_var = "percent", fill_var = "Group2",
#'   display = 3, barplot_color = barplot_color,
#'   ylim1 = c(0, 4.5), ylim2 = c(0, 1),
#'   axis_titles_x = "Cll Types", x_label_angle = 0, x_label_bold = TRUE, axis_title_x_size = 14,
#'   axis_titles_y1 = "Cell Count", axis_titles_y2 = "Sample Ratio",
#'   y1_label_bold = TRUE,y2_label_bold =  TRUE, axis_title_y1_size = 14,axis_title_y2_size = 14,
#'   title1 = "Cell Number Distribution", title2 = "Sample Ratio Distribution",
#'   title_size = 16,title_bold = TRUE,
#'   title1_size = 16, title1_bold = TRUE, title2_size = 16, title2_bold = TRUE,
#'   legend_show = TRUE, legend_title = "Legend", legend_size = 14, legend_position = "bottom",
#'   background_color = "white",
#'   coord_flip = TRUE,
#'   grid_ncol = 2,grid_layout  = rbind(c(1, 1, 1, 1, 1, 2, 2),
#'                                      c(1, 1, 1, 1, 1, 2, 2)),
#'   p1_theme = NULL,p2_theme = NULL,p1_guide = NULL,p2_guide = NULL
#' )
#'
#' @export
BetterVis_Barplot_Accumulative <- function(data, x_var, y1_var, y2_var, fill_var, display = 3, barplot_color = NULL,
                                           axis_titles_x = "", x_label_angle = 0, x_label_bold = FALSE,
                                           axis_title_x_size = 12, axis_titles_y1 = "", axis_titles_y2 = "",
                                           y1_label_bold = FALSE, y2_label_bold = FALSE,
                                           axis_title_y1_size = 12, axis_title_y2_size = 12,
                                           title1 = "", title2 = "",
                                           title1_size = NULL, title1_bold = NULL,
                                           title2_size = NULL, title2_bold = NULL,
                                           title_size = 14, title_bold = FALSE,
                                           legend_show = TRUE, legend_title = "", legend_size = 10,
                                           legend_position = "bottom",legend_row=NULL,legend_col=NULL, background_color = "white",
                                           ylim1 = NULL, ylim2 = NULL, coord_flip = FALSE,
                                           grid_ncol = 2, grid_layout = rbind(c(1, 1, 1, 1, 1, 2, 2),
                                                                              c(1, 1, 1, 1, 1, 2, 2)),
                                           p1_theme=NULL,p2_theme=NULL,p1_guide=NULL,p2_guide=NULL) {

  title1_size <- ifelse(is.null(title1_size), title_size, title1_size)
  title1_bold <- ifelse(is.null(title1_bold), title_bold, title1_bold)
  title2_size <- ifelse(is.null(title2_size), title_size, title2_size)
  title2_bold <- ifelse(is.null(title2_bold), title_bold, title2_bold)

  fill_levels <- levels(as.factor(data[[fill_var]]))
  n_levels <- length(fill_levels)

  if (!is.null(barplot_color)) {
    if (length(barplot_color) < n_levels) {
      stop("The number of colors in barplot_color is less than the levels of fill_var.")
    }
    barplot_color <- barplot_color[1:n_levels]
  } else {
    barplot_color <- c("#528AA7", "#6EB467", "#D86F6F", "#80689C", "#CEB481")
  }

  p1 <- ggplot(data, aes_string(x = x_var, y = y1_var, fill = fill_var)) +
    geom_bar(stat = 'identity', position = "stack") +
    scale_fill_manual(values = barplot_color) +
    labs(x = axis_titles_x, y = axis_titles_y1, title = title1) +
    {if (coord_flip) coord_flip() else NULL} +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = axis_title_x_size, face = ifelse(x_label_bold, 'bold', 'plain')),
      axis.text.x = element_text(size = 16, angle = x_label_angle,face = ifelse(x_label_bold, 'bold', 'plain')),
      axis.text.y = element_text(size = 16, face = ifelse(x_label_bold, 'bold', 'plain')),
      axis.line.x = element_line(size = 1.2),
      axis.line.y = element_line(size = 1.2),
      axis.title.y = element_text(size = axis_title_y1_size, face = ifelse(y1_label_bold, 'bold', 'plain')),
      legend.position = ifelse(legend_show, legend_position, "none"),
      legend.title = element_text(size = legend_size, face = 'bold'),
      plot.title = element_text(size = title1_size, face = ifelse(title1_bold, 'bold', 'plain')),
      plot.background = element_rect(fill = background_color)
    ) +
    guides(fill = guide_legend(title = legend_title, nrow = legend_row,ncol = legend_col, byrow = TRUE)) +
    scale_y_continuous(limits = if (!is.null(ylim1)) ylim1 else NULL)
  p1<-p1+p1_theme
  p1<-p1+p1_guide

  p2 <- ggplot(data, aes_string(x = x_var, y = y2_var, fill = fill_var)) +
    geom_bar(stat = 'identity', position = "stack") +
    scale_fill_manual(values = barplot_color) +
    labs(x = axis_titles_x, y = axis_titles_y2, title = title2) +
    {if (coord_flip) coord_flip() else NULL} +
    theme_classic() +
    theme(
      axis.title.x = element_text(size = axis_title_x_size, face = ifelse(x_label_bold, 'bold', 'plain')),
      axis.text.x = element_text(size = 16, angle = x_label_angle,face = ifelse(x_label_bold, 'bold', 'plain')),
      axis.line.x = element_line(size = 1.2),
      axis.line.y = element_line(size = 1.2),
      axis.title.y = element_text(size = axis_title_y2_size, face = ifelse(y2_label_bold, 'bold', 'plain')),
      axis.text.y = element_blank(),
      legend.position = "none",
      plot.background = element_rect(fill = background_color),
      plot.title = element_text(size = title2_size, face = ifelse(title2_bold, 'bold', 'plain'))
    ) +
    scale_y_continuous(limits = if (!is.null(ylim2)) ylim2 else NULL)
  p2<-p2+p2_theme
  p2<-p2+p2_guide

  if (display == 1) {
    return(p1)
  } else if (display == 2) {
    return(p2)
  } else if (display == 3) {
    grid.arrange(p1, p2, ncol = grid_ncol, layout_matrix = grid_layout)
  }
}

