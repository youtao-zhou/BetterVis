# 创建数据
Boxplot_Experiment_example <- boxplot_example_1
usethis::use_data(Boxplot_Experiment_example,overwrite = TRUE)


BetterVis_Barplot_Accumulative_example <- fread("~/test/BetterVis_Example/02_Barplot/BetterVis_Barplot_Accumulative_example.csv")
usethis::use_data(BetterVis_Barplot_Accumulative_example,overwrite = TRUE)
# 最后运行编写文档
roxygen2::roxygenise()



