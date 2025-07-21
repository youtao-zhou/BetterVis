# 创建数据
Boxplot_Experiment_example <- boxplot_example_1
usethis::use_data(Boxplot_Experiment_example)


BetterVis_Barplot_Accumulative_example <- fread("~/test/BetterVis_Example/02_Barplot/BetterVis_Barplot_Accumulative_example.csv")
usethis::use_data(BetterVis_Barplot_Accumulative_example)

BetterVis_Barplot_Bidirectional_example <- fread("~/test/BetterVis_Example/02_Barplot/BetterVis_Barplot_Bidirectional_example.csv")
usethis::use_data(BetterVis_Barplot_Bidirectional_example,overwrite = TRUE)



BetterVis_Barplot_Circle_example <- fread("~/test/BetterVis_Example/02_Barplot/BetterVis_Barplot_Circle_example.csv")
usethis::use_data(BetterVis_Barplot_Circle_example)


BetterVis_Barplot_Horizontal_example <- fread("~/test/BetterVis_Example/02_Barplot/BetterVis_Barplot_Horizontal.csv")
usethis::use_data(BetterVis_Barplot_Horizontal_example)


BetterVis_Ring_example <- fread("~/test/BetterVis_Example/03_Ringplot/BetterVis_Ringplot.csv")
usethis::use_data(BetterVis_Ring_example)






BetterVis_DotPlot_Heatmap_example <-  fread("~/test/BetterVis_Example/05_Dotplot/BetterVis_DotPlot_Heatmap_example.csv")
usethis::use_data(BetterVis_DotPlot_Heatmap_example, overwrite = TRUE)


BetterVis_Dotplot_Beeswarm_example  <-  fread("~/test/BetterVis_Example/05_Dotplot/BetterVis_Dotplot_Beeswarm_example.csv")
usethis::use_data(BetterVis_Dotplot_Beeswarm_example, overwrite = TRUE)






BetterVis_Ridgeline_Facet_example <-  fread("~/test/BetterVis_Example/06_Ridgeline/BetterVis_Ridgeline_Facet_example.csv")
usethis::use_data(BetterVis_Ridgeline_Facet_example, overwrite = TRUE)



BetterVis_Ridgeline_Single_example <-  fread("~/test/BetterVis_Example/06_Ridgeline/BetterVis_Ridgeline_Single_example.csv")
usethis::use_data(BetterVis_Ridgeline_Single_example, overwrite = TRUE)


BetterVis_Sankey_Single_example <-  fread("~/test/BetterVis_Example/07_Sankey/BetterVis_Sankey_Single_example.csv")
usethis::use_data(BetterVis_Sankey_Single_example, overwrite = TRUE)



BetterVis_Venn_UpsetR_example <- readRDS("~/test/BetterVis_Example/08_Venn/BetterVis_Venn_UpsetR.rds")
usethis::use_data(BetterVis_Venn_UpsetR_example, overwrite = TRUE)


BetterVis_Circos_Interaction_example1 <- fread("~/test/BetterVis_Example/09_Circos/BetterVis_Circos_Interaction_example1.csv")
usethis::use_data(BetterVis_Circos_Interaction_example1)


BetterVis_Circos_Interaction_example2 <- fread("~/test/BetterVis_Example/09_Circos/BetterVis_Circos_Interaction_example2.csv")
usethis::use_data(BetterVis_Circos_Interaction_example2)










# Run these lines in your console
usethis::use_package("ggraph")
usethis::use_package("stringr")
usethis::use_package("tidygraph")
usethis::use_package("tidyr")
usethis::use_package("circlize")
usethis::use_package("rlang")
usethis::use_package("methods")
usethis::use_package("UpSetR")
usethis::use_package("ggsankey")
usethis::use_package("ggridges")
usethis::use_package("cowplot")
usethis::use_package("ggridges")
usethis::use_package("cowplot")

usethis::use_package("ggbeeswarm")
usethis::use_package("ggpubr") # Already a dependency from other functions, but good to ensure
usethis::use_package("Hmisc")    # For median_hilow()
# 核心依赖
usethis::use_package("ComplexHeatmap")
usethis::use_package("circlize") # for colorRamp2
usethis::use_package("scales")   # for hue_pal, rescale

# 示例中使用的包，建议作为 "Suggests"
usethis::use_package("RColorBrewer")


# 最后运行编写文档
roxygen2::roxygenise()



