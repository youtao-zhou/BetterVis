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
#' # Discrete And Unpaired color selection
#' BetterVis_Color(type = "discrete", odd_or_even = "even",  n = 10, style = "Science", option = 1)

#' # Discrete And paired color selection
#' BetterVis_Color(type = "discrete", odd_or_even = "even",  n = 9, style = "Science", option = 2,paired = TRUE, pair_num = 3)

#' # Continuous color extraction连续型取色
#' BetterVis_Color(type = "discrete", odd_or_even = "even",  n = 9, style = "Science", option = 2,paired = TRUE, pair_num = 3)


#' @export



BetterVis_Color <- function(type = c("discrete", "continuous"),
                            odd_or_even = NULL,
                            paired = FALSE,
                            pair_num = NULL,
                            n = NULL,
                            option = 1,
                            style = c("rcolorbrewer", "Science", "Cell", "Lancet")) {
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
          c("#E05B5B"),
          c("#519CBA"),
          c("#79C377")
        ),
        `3` = list(
          c("#b8b8bb", "#80bcc8", "#d88f91"),
          c("#2C91E0", "#3ABF99", "#F0A73A"),
          c("#252323", "#313b87", "#dc252a")
        ),
        `5` = list(
          c("#e3716e", "#eca680", "#7ac7e2", "#f7df87", "#54beaa"),
          c("#ECA8A9", "#74AED4", "#D3E2B7", "#CFAFD4", "#F7C97E"),
          c("#67ADB7", "#F5E1D8", "#E4A6BD", "#F3D8E1", "#AFACB7")
        )
      ),
      Nature = list(
        `1` = list(
          c("#009000"),
          c("#FF9E04"),
          c("#3179F0")
        ),
        `3` = list(
          c("#EF767A", "#456990", "#48C0AA"),
          c("#99b9e9", "#b0d992", "#fccccb"),
          c("#e3716e", "#7ac7e2", "#54beaa")
        ),
        `5` = list(
          c("#A6D5DB", "#EAA9C1", "#FACABC", "#C0BFDF", "#CCDCAD"),
          c("#F3A17C", "#FAC45A", "#78A040", "#36600E", "#E6E0B0"),
          c("#6CBE45", "#3954A5", "#25AAE2", "#F6EA1C", "#949599")
        )
      ),
      Science = list(
        `1` = list(
          c("#929292"),
          c("#202020"),
          c("#FF2500")
        ),
        `3` = list(
          c("#3B54A3", "#CD61A4", "#CD1C68"),
          c("#BD554E", "#88B37E", "#AEDFE4"),
          c("#3BCACA", "#138787", "#EFDB30")
        ),
        `5` = list(
          c("#9DD0C7", "#9180AC", "#D9BDD8", "#E58579", "#8AB1D2"),
          c("#D2BCDE", "#6E348C", "#B5D4E9", "#ED7C72", "#1D75B5"),
          c("#66c2a5", "#fc8d59", "#fee08b", "#d7191c", "#abd9e9")
        )
      ),
      ggsci_npg = list(
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
          c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"),#npg
          c("#F39B7FFF", "#8491B4FF", "#91D1C2FF", "#DC0000FF", "#B09C85FF"),
          c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "#7876B1FF")#NEJM
        )
      ),
      ggsci_lancet = list(
        `1` = list(
          c("#e41a1c"),
          c("#377eb8"),
          c("#FBEB66")
        ),
        `3` = list(
          c("#4EA660", "#79CAFB", "#5292F7"),
          c("#984ea3", "#ff7f00", "#ffff33"),
          c("#a65628", "#f781bf", "#999999")
        ),
        `5` = list(
          c("#00468BFF", "#ED0000FF", "#42B540FF", "#0099B4FF", "#925E9FFF"),
          c("#00468B99", "#ED000099", "#42B54099", "#0099B499", "#925E9F99"),
          c("#4DBBD5B2", "#00A087B2", "#F39B7FB2", "#8491B4B2", "#DC0000B2")
        )
      )
    ),
    even = list(
      Nature = list(
        `2` = list(
          c("#3A53A4", "#6999D1"),
          c("#ED2224", "#F99F44"),
          c("#32B44A", "#ADD694")
        ),
        `4` = list(
          c("#E9292A", "#2D2B2B", "#9C9CA0", "#3B54A3"),
          c("#E48B9A", "#B3E3F4", "#BE1D2C", "#8EB5CB"),
          c("#FFE35E", "#FBB050", "#98694E", "#C85C19")
        ),
        `6` = list(
          c("#F3BA1E", "#43B6E1", "#80539F", "#ED7B7B", "#BCD53B", "#4D6CAA"),
          c("#F89C74", "#5DE0DD", "#DCB0F2", "#C9DB74", "#1C66A7", "#F6CF71"),
          c("#2D80B9", "#F98519", "#F1BCE1", "#3CA63B", "#BFC02C", "#DA3839")
        )
      ),
      Science = list(
        `2` = list(
          c("#85BAD9", "#5488BF"),
          c("#A91E22", "#E04832"),
          c("#FBBA73", "#FDE39F")
        ),
        `4` = list(
          c("#015493", "#019092", "#999999", "#F4A99B"),
          c("#909090", "#0095FF", "#019092", "#6FDCB5"),
          c("#069DFF", "#808080", "#A4E048", "#010101")
        ),
        `6` = list(
          c("#0C4E9B", "#6B98C4", "#C72228", "#F5867F", "#F98F34", "#FFBC80"),
          c("#3B549D", "#6BBC46", "#9EAAD1", "#B4DEA2", "#D3272B", "#F29091"),
          c("#354898", "#6B6B6B", "#73ABCF", "#972D36", "#C55645", "#FAAE5F")
        )
      ),
      BMJ = list(
        `2` = list(
          c("#4484b1", "#F2A24F"),
          c("#9BBAD6", "#F6B88F"),
          c("#FBEB66", "#79CAFB")
        ),
        `4` = list(
          c("#c2d0ea", "#0070b8", "#00b3b0", "#d40d8c"),
          c("#7b95c6", "#49c2d9", "#a1d8e8", "#a2c986"),
          c("#4EA660", "#79CAFB", "#5292F7", "#AA77E9")
        ),
        `6` = list(
          c("#168676", "#27447C", "#4871B3", "#991F22", "#B88640", "#E73C36"),
          c("#80B1D3", "#8DD1C6", "#BDBADB", "#F47F72", "#FBB463", "#FBF8B4"),
          c("#F59B7B", "#ED8828", "#81B21F", "#8D73BA", "#ABD7E8", "#33ABC1")
        )
      )
    )
  )

  # 连续型颜色库
  continuous_colors <- list(
    rcolorbrewer = list(
      c("#9EBCDA", "#8C96C6", "#8C6BB1"),
      c("#99D8C9", "#66C2A4", "#41AE76"),
      c("#9ECAE1", "#6BAED6", "#4292C6"),
      c("#FEE08B", "#FDAE61", "#F46D43")
    ),
    Science = list(
      c("#58B893", "#9AD6BD", "#CEEADC"),
      c("#F07651", "#FBAE95", "#FCD6C9"),
      c("#7B8DBD", "#ABB0CD", "#D9DCEF")
    ),
    Cell = list(
      c("#E071B6", "#EBAAD4", "#F5D4E9"),
      c("#004A80", "#0079AF", "#9BCCE3"),
      c("#C85C19", "#F58228", "#FBB050")
    ),
    Lancet = list(
      c("#af8fd0", "#caadd8", "#f0ecf7"),
      c("#80CDC1", "#35978F", "#01665E"),
      c("#C51B7D", "#DE77AE", "#F1B6DA")
    )
  )

  return(list(discrete = discrete_colors, continuous = continuous_colors))
}




