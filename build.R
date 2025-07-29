# 创建数据
Boxplot_Experiment_example <- boxplot_example_1
usethis::use_data(Boxplot_Experiment_example)


BetterVis_Barplot_Accumulative_example <- fread("~/test/BetterVis_Example/02_Barplot/BetterVis_Barplot_Accumulative_example.csv")
usethis::use_data(BetterVis_Barplot_Accumulative_example)

BetterVis_Barplot_Bidirectional_example <- fread("~/test/BetterVis_Example/02_Barplot/BetterVis_Barplot_Bidirectional_example.csv")
usethis::use_data(BetterVis_Barplot_Bidirectional_example,overwrite = TRUE)



BetterVis_Barplot_Circle_example <- fread("~/test/BetterVis_Example/02_Barplot/BetterVis_Barplot_Circle_example.csv")
usethis::use_data(BetterVis_Barplot_Circle_example, overwrite = TRUE)


BetterVis_Barplot_Horizontal_example <- fread("~/test/BetterVis_Example/02_Barplot/BetterVis_Barplot_Horizontal.csv")
usethis::use_data(BetterVis_Barplot_Horizontal_example)




# 在你的 R 控制台中运行一次
# Load data
nodes_data <- read.csv("~/test/BetterVis_Example/03_Network/BetterVis_Network_PPI_example1.csv")
edges_data <- read.csv("~/test/BetterVis_Example/03_Network/BetterVis_Network_PPI_example2.csv")

# Name the data objects
BetterVis_Network_PPI_nodes <- nodes_data
BetterVis_Network_PPI_edges <- edges_data

# Save to data/ directory
usethis::use_data(BetterVis_Network_PPI_nodes, BetterVis_Network_PPI_edges, overwrite = TRUE)








BetterVis_Heatmap_Bicontinuous_example <-  fread("~/test/BetterVis_Example/04_Heatmap/BetterVis_Heatmap_Bicontinuous_example.csv")
usethis::use_data(BetterVis_Heatmap_Bicontinuous_example, overwrite = TRUE)




BetterVis_Heatmap_Circle_example <-  fread("~/test/BetterVis_Example/04_Heatmap/BetterVis_Heatmap_Circle_example.csv")
usethis::use_data(BetterVis_Heatmap_Circle_example, overwrite = TRUE)


BetterVis_Heatmap_Common_example <-  read.csv("~/test/BetterVis_Example/04_Heatmap/BetterVis_Heatmap_Common_example.csv",row.names = 1)
usethis::use_data(BetterVis_Heatmap_Common_example, overwrite = TRUE)



BetterVis_Heatmap_Mantel_example1 <-  fread("~/test/BetterVis_Example/04_Heatmap/BetterVis_Heatmap_Mantel_example1.csv")
usethis::use_data(BetterVis_Heatmap_Mantel_example1, overwrite = TRUE)


BetterVis_Heatmap_Mantel_example2 <-  fread("~/test/BetterVis_Example/04_Heatmap/BetterVis_Heatmap_Mantel_example2.csv")
usethis::use_data(BetterVis_Heatmap_Mantel_example2, overwrite = TRUE)

BetterVis_Heatmap_Multi_example <-  fread("~/test/BetterVis_Example/04_Heatmap/BetterVis_Heatmap_Multi_example.csv")
usethis::use_data(BetterVis_Heatmap_Multi_example, overwrite = TRUE)


BetterVis_DotPlot_Heatmap_example <-  fread("~/test/BetterVis_Example/05_Dotplot/BetterVis_DotPlot_Heatmap_example.csv")
usethis::use_data(BetterVis_DotPlot_Heatmap_example, overwrite = TRUE)


BetterVis_Dotplot_Beeswarm_example  <-  fread("~/test/BetterVis_Example/05_Dotplot/BetterVis_Dotplot_Beeswarm_example.csv")
usethis::use_data(BetterVis_Dotplot_Beeswarm_example, overwrite = TRUE)


BetterVis_Dotplot_Mixbar_example   <-  fread("~/test/BetterVis_Example/05_Dotplot/BetterVis_Dotplot_Mixbar_example.csv")
usethis::use_data(BetterVis_Dotplot_Mixbar_example, overwrite = TRUE)

BetterVis_Dotplot_Multifacet_example   <-  fread("~/test/BetterVis_Example/05_Dotplot/BetterVis_Dotplot_Multifacet_example.csv")
usethis::use_data(BetterVis_Dotplot_Multifacet_example, overwrite = TRUE)



BetterVis_Dotplot_Smoothline_example   <-  fread("~/test/BetterVis_Example/05_Dotplot/BetterVis_Dotplot_Smoothline_example.csv")
usethis::use_data(BetterVis_Dotplot_Smoothline_example, overwrite = TRUE)



BetterVis_Dotplot_Ternary_example   <-  fread("~/test/BetterVis_Example/05_Dotplot/BetterVis_Dotplot_Ternary.csv")
usethis::use_data(BetterVis_Dotplot_Ternary_example, overwrite = TRUE)


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




BetterVis_Circos_Ringplot_example <- fread("~/test/BetterVis_Example/09_Circos/BetterVis_Circos_Ringplot_example.csv")
usethis::use_data(BetterVis_Circos_Ringplot_example)


BetterVis_LinePlot_Chart_example <- fread("~/test/BetterVis_Example/10_Lineplot/BetterVis_LinePlot_Chart_example.csv")
usethis::use_data(BetterVis_LinePlot_Chart_example)

BetterVis_Pie_Single_example <- fread("~/test/BetterVis_Example/11_Pie/BetterVis_Pie_Single_example.csv")
usethis::use_data(BetterVis_Pie_Single_example)


# 在你的 R 控制台中运行一次
# Load data
df_in <- read.csv("~/test/BetterVis_Example/11_Pie/BetterVis_Pie_Multi_example1.csv")
df_out <- read.csv("~/test/BetterVis_Example/11_Pie/BetterVis_Pie_Multi_example2.csv")

# Name the data objects
BetterVis_Pie_Multi_example_in <- df_in
BetterVis_Pie_Multi_example_out <- df_out

# Save to data/ directory
usethis::use_data(BetterVis_Pie_Multi_example_in, BetterVis_Pie_Multi_example_out, overwrite = TRUE)




usethis::use_package("ggtext")
usethis::use_package("scales")

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
usethis::use_package("ggpubr") # Already a dependency from other functions, but good to ensure0110
usethis::use_package("Hmisc")    # For median_hilow()
# 核心依赖
usethis::use_package("ComplexHeatmap")
usethis::use_package("circlize") # for colorRamp2
usethis::use_package("scales")   # for hue_pal, rescale
usethis::use_package("ggExtra")
# 示例中使用的包，建议作为 "Suggests"
usethis::use_package("RColorBrewer")
usethis::use_package("tidyselect")

usethis::use_package("cowplot")
usethis::use_package("scales")

# 由于 MASS 包仅在示例中使用，我们将其添加到 "Suggests" 字段
usethis::use_package("MASS", type = "Suggests")
usethis::use_package("ggprism")
usethis::use_package("paletteer")
# ggpubr is likely already a dependency, but it's good practice to ensure it's there
usethis::use_package("ggpubr")
usethis::use_package("ggtern")
usethis::use_package("MetBrewer")
usethis::use_package("MetBrewer")
usethis::use_package("ggtext")
usethis::use_package("geomtextpath")
usethis::use_package("MetBrewer") # Likely already a dependency, but good to ensure

usethis::use_package("tidyr")
usethis::use_package("tibble")
usethis::use_package("magrittr")
usethis::use_package("MetBrewer") # Likely already a dependency, but good to ensure

usethis::use_package("linkET")
usethis::use_package("scales")
usethis::use_package("RColorBrewer") # Also used in the function signature
usethis::use_package("tidyverse")
usethis::use_package("ggnewscale")
usethis::use_package("patchwork")
usethis::use_package("tidyverse", type = "depends")
usethis::use_package("scales") # For brewer_pal
usethis::use_package("Hmisc")
usethis::use_package("tidyselect") # For the where() helper
usethis::use_package("ggnewscale")

usethis::use_package("igraph")
usethis::use_package("tidygraph")
usethis::use_package("ggraph")
usethis::use_package("ggforce")
usethis::use_package("RColorBrewer")
usethis::use_package("scales")
usethis::use_package("jjPlot")
usethis::use_package("ggnewscale")
# 最后运行编写文档
roxygen2::roxygenise()



