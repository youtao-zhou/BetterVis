#' BetterVis Color Function
#'
#' This function provides enhanced color customization options for creating plots. It supports both discrete and continuous color palettes,
#' with several predefined styles such as "Nature", "Science", "Cell", and "Lancet". The function allows for specifying odd or even numbers
#' of colors, pairing options, and custom schemes.
#'
#' @name BetterVis_Color
#' @description Provides discrete and continuous color schemes for visualizations with advanced customization options.
#'
#' @param type A character string specifying whether to use "discrete" or "continuous" color schemes. Default is \code{NULL}.
#' @param odd_or_even A character string specifying whether the total number of colors should be "odd" or "even" (only applicable to discrete types). Default is \code{NULL}.
#' @param n An integer specifying the total number of colors needed. Required for both discrete and continuous types.
#' @param style A character string specifying the color style to use. Options are \code{"Science"}, \code{"Nature"}, \code{"Cell"}, or \code{"Lancet"}.
#' @param option An integer specifying the color scheme option. Options are \code{1}, \code{2}, or \code{3}.
#' @param paired A logical value indicating whether to use paired colors. Default is \code{FALSE}. If \code{TRUE}, colors will be repeated in groups based on \code{pair_num}.
#' @param pair_num An integer specifying the number of colors in each group when \code{paired = TRUE}. This must be specified when using paired colors.
#' @param ... Additional arguments passed to \code{ggplot2} layers (if applicable).
#'
#' @return A vector of color values, either discrete or continuous, depending on the specified parameters.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_violin geom_jitter
#' @importFrom ggplot2 position_dodge position_jitterdodge scale_fill_manual scale_color_manual
#' @importFrom ggplot2 ggtitle theme element_text element_rect ylab xlab facet_wrap ylim
#' @importFrom RColorBrewer brewer.pal
#' @importFrom cowplot theme_cowplot
#' @importFrom ggpubr stat_compare_means
#' @importFrom grid unit
#'
#' @examples
#' # 离散型非配对取色
#' BetterVis_Color(type = "discrete", odd_or_even = "even",  n = 10, style = "Science", option = 1)

#' # 离散型配对取色
#' BetterVis_Color(type = "discrete", odd_or_even = "even",  n = 9, style = "Science", option = 2,paired = TRUE, pair_num = 3)

#' # 连续型取色
#' BetterVis_Color(type = "continuous", n = 9, style = "Nature", option = 1)

#' @export



BetterVis_Color <- function(type = c("discrete", "continuous"),
                            odd_or_even = NULL,
                            paired = FALSE,
                            pair_num = NULL,
                            n = NULL,
                            option = 1,
                            style = c("Nature", "Science", "Cell", "Lancet")) {
  type <- match.arg(type)
  style <- match.arg(style)
  color_library <- .get_color_library()

  if (type == "discrete") {
    if (is.null(odd_or_even)) stop("For 'discrete' type, please specify 'odd_or_even'.")
    if (is.null(n)) stop("For 'discrete' type, please specify 'n'.")
    if (!paired && !is.null(pair_num)) stop("You should use 'paired = TRUE' when specifying 'pair_num'.")
    if (paired && is.null(pair_num)) stop("When 'paired = TRUE', you must specify 'pair_num'.")

    # 获取离散颜色库
    discrete_colors <- color_library$discrete[[odd_or_even]][[style]]
    max_available_n <- max(as.numeric(names(discrete_colors)))

    # 如果 n 超出范围，动态扩展
    if (n > max_available_n) {
      base_colors <- unlist(lapply(discrete_colors, function(x) x[[option]]), use.names = FALSE)
      if (length(base_colors) == 0) stop("Invalid option or no colors available for this style.")

      # 去重选择颜色（确保尽量不重复）
      if (paired) {
        if (n %% pair_num != 0) stop("'n' must be divisible by 'pair_num' when using paired colors.")
        unique_colors <- unique(base_colors)
        if (length(unique_colors) < n / pair_num) stop("Not enough unique colors for pairing.")
        return(rep(unique_colors[1:(n / pair_num)], each = pair_num))
      } else {
        return(base_colors[1:n])
      }
    }

    # 获取具体配色方案
    if (!as.character(n) %in% names(discrete_colors)) stop(paste("Invalid 'n'. Available:", paste(names(discrete_colors), collapse = ", ")))
    available_combinations <- discrete_colors[[as.character(n)]]
    option <- ((option - 1) %% length(available_combinations)) + 1
    base_colors <- available_combinations[[option]]

    # 配对逻辑
    if (paired) {
      if (n %% pair_num != 0) stop("'n' must be divisible by 'pair_num' when using paired colors.")
      repeat_count <- n / pair_num
      unique_colors <- unique(base_colors)
      if (length(unique_colors) < repeat_count) stop("Not enough unique colors for pairing.")
      repeated_colors <- rep(unique_colors[1:repeat_count], each = pair_num)
      return(repeated_colors[1:n])
    }

    return(base_colors)
  }

  else if (type == "continuous") {
    continuous_colors <- color_library$continuous[[style]]
    if (length(continuous_colors) < option) {
      stop("Option exceeds the number of available continuous schemes.")
    }
    base_colors <- continuous_colors[[option]]

    # 如果 n > 7 且为奇数或 n > 6 且为偶数，智能分配
    if (n > 7 && n %% 2 == 1) {
      smaller_n <- floor(n / 2)
      extra_colors <- BetterVis_Color(type = "continuous", n = n - smaller_n, style = style, option = 2)
      return(c(grDevices::colorRampPalette(base_colors)(smaller_n), extra_colors))
    } else if (n > 6) {
      smaller_n <- floor(n / 2)
      extra_colors <- BetterVis_Color(type = "continuous", n = n - smaller_n, style = style, option = 3)
      return(c(grDevices::colorRampPalette(base_colors)(smaller_n), extra_colors))
    }

    return(grDevices::colorRampPalette(base_colors)(n))
  } else {
    stop("Invalid type specified.")
  }
}





.get_color_library <- function() {
  # 离散型颜色库
  discrete_colors <- list(
    odd = list(
      Nature = list(
        `1` = list(
          c("#1b9e77"),
          c("#d95f02"),
          c("#7570b3")
        ),
        `3` = list(
          c("#1b9e77", "#d95f02", "#7570b3"),
          c("#66a61e", "#e6ab02", "#a6761d"),
          c("#fc8d59", "#fee08b", "#d7191c")
        ),
        `5` = list(
          c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"),
          c("#e6ab02", "#a6761d", "#666666", "#fc8d59", "#fee08b"),
          c("#d7191c", "#abd9e9", "#66c2a5", "#fc8d59", "#fee08b")
        )
      ),
      Science = list(
        `1` = list(
          c("#377eb8"),
          c("#4daf4a"),
          c("#984ea3")
        ),
        `3` = list(
          c("#377eb8", "#4daf4a", "#984ea3"),
          c("#ff7f00", "#ffff33", "#a65628"),
          c("#f781bf", "#999999", "#e41a1c")
        ),
        `5` = list(
          c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33"),
          c("#a65628", "#f781bf", "#999999", "#e41a1c", "#d7191c"),
          c("#66c2a5", "#fc8d59", "#fee08b", "#d7191c", "#abd9e9")
        )
      ),
      Cell = list(
        `1` = list(
          c("#8dd3c7"),
          c("#ffffb3"),
          c("#bebada")
        ),
        `3` = list(
          c("#8dd3c7", "#ffffb3", "#bebada"),
          c("#fb8072", "#80b1d3", "#fdb462"),
          c("#b3de69", "#fccde5", "#bc80bd")
        ),
        `5` = list(
          c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3"),
          c("#fdb462", "#b3de69", "#fccde5", "#bc80bd", "#ffed6f"),
          c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99")
        )
      ),
      Lancet = list(
        `1` = list(
          c("#e41a1c"),
          c("#377eb8"),
          c("#4daf4a")
        ),
        `3` = list(
          c("#e41a1c", "#377eb8", "#4daf4a"),
          c("#984ea3", "#ff7f00", "#ffff33"),
          c("#a65628", "#f781bf", "#999999")
        ),
        `5` = list(
          c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"),
          c("#ffff33", "#a65628", "#f781bf", "#999999", "#d73027"),
          c("#4575b4", "#91bfdb", "#fc8d59", "#fee090", "#d73027")
        )
      )
    ),
    even = list(
      Nature = list(
        `2` = list(
          c("#1b9e77", "#d95f02"),
          c("#66a61e", "#e6ab02"),
          c("#fc8d59", "#fee08b")
        ),
        `4` = list(
          c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
          c("#66a61e", "#e6ab02", "#a6761d", "#666666"),
          c("#fc8d59", "#fee08b", "#d7191c", "#abd9e9")
        ),
        `6` = list(
          c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02"),
          c("#a6761d", "#666666", "#fc8d59", "#fee08b", "#d7191c", "#abd9e9"),
          c("#66c2a5", "#fc8d59", "#fee08b", "#d7191c", "#abd9e9", "#e6ab02")
        )
      ),
      Science = list(
        `2` = list(
          c("#377eb8", "#4daf4a"),
          c("#984ea3", "#ff7f00"),
          c("#ffff33", "#a65628")
        ),
        `4` = list(
          c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00"),
          c("#ffff33", "#a65628", "#f781bf", "#999999"),
          c("#e41a1c", "#d7191c", "#66c2a5", "#fc8d59")
        ),
        `6` = list(
          c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628"),
          c("#f781bf", "#999999", "#e41a1c", "#d7191c", "#66c2a5", "#fc8d59"),
          c("#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf")
        )
      )
    )
  )

  # 连续型颜色库
  continuous_colors <- list(
    Nature = list(
      c("#1b9e77", "#66a61e", "#e6ab02"),
      c("#7570b3", "#e7298a", "#66a61e"),
      c("#e6ab02", "#a6761d", "#fc8d59")
    ),
    Science = list(
      c("#377eb8", "#4daf4a", "#ff7f00"),
      c("#ffff33", "#a65628", "#f781bf"),
      c("#999999", "#e41a1c", "#d7191c")
    ),
    Cell = list(
      c("#8dd3c7", "#80b1d3", "#fdb462"),
      c("#b3de69", "#fccde5", "#bc80bd"),
      c("#a6cee3", "#33a02c", "#fb9a99")
    ),
    Lancet = list(
      c("#e41a1c", "#377eb8", "#4daf4a"),
      c("#984ea3", "#ff7f00", "#ffff33"),
      c("#a65628", "#f781bf", "#999999")
    )
  )

  return(list(discrete = discrete_colors, continuous = continuous_colors))
}





