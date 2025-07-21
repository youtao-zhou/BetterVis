#' @importFrom tibble tibble
NULL

#' Boxplot Example Data
#'
#' A dataset to demonstrate boxplot functionality. It includes measurements
#' for different supplement types and doses.
#'
#' @format A data frame with 60 rows and 3 variables:
#' \describe{
#'   \item{len}{Numeric. Length of the sample.}
#'   \item{supp}{Character. Type of supplement (\code{"VC"} or \code{"OJ"}).}
#'   \item{dose}{Factor. Dose level of the supplement (\code{"dose_1"}, \code{"dose_2"}, or \code{"dose_3"}).}
#' }
#' @source Simulated example data.
"Boxplot_Experiment_example"


#' Accumulative Barplot Example Data
#'
#' A dataset to demonstrate the functionality of creating accumulative bar plots.
#' It contains cell count and percentage data for various cell types across different patients.
#'
#' @format A data frame with 50 rows and 4 variables:
#' \describe{
#'   \item{Patient}{Character. Identifier for the patient, with levels including \code{"P21"}, \code{"P48"}, \code{"P53"}, \code{"P54"}, \code{"P08"}.}
#'   \item{celltype}{Factor. Type of cell being measured, including \code{"Plasmacytoid DCs"}, \code{"B cells"}, \code{"Treg cells"}, \code{"Plasma cells"}, \code{"Monocytes"}, \code{"Endothelial cells"}, \code{"Cancer cells"}, \code{"Fibroblasts"}, \code{"Columner epithelial cells"}.}
#'   \item{cell_num}{Numeric. Number of cells counted for each patient and cell type.}
#'   \item{percent}{Numeric. Proportion of the total cell count represented by this cell type for each patient.}
#' }
#' @source Simulated example data for visualization examples.
"BetterVis_Barplot_Accumulative_example"

#' Bidirectional Barplot Example Dataset
#'
#' A dataset designed to illustrate the use of bidirectional bar plots. It includes example data on different pathways, groupings, species, and their respective contribution percentages.
#'
#' @format A data frame with 10 rows and 4 variables:
#' \describe{
#'   \item{pathway}{Character. Name of the biochemical pathway, indicating the specific pathway being referenced.}
#'   \item{group}{Character. Defines the enrichment group for the dataset, specifying categories such as \code{"Enriched in stable low DDS"}.}
#'   \item{Species}{Character. Represents the species involved, such as \code{"Bacteroides massiliensis"}, \code{"Escherichia coli"}, and others.}
#'   \item{Percentage}{Numeric. The percentage contribution of each species to the pathway, which can be both positive or negative to indicate directionality.}
#' }
#' @source Simulated data for visualization examples.
"BetterVis_Barplot_Bidirectional_example"


#' KEGG Pathway Enrichment Data for Horizontal Barplot
#'
#' This dataset provides information on various KEGG pathways, associated gene counts, and statistical measures. It is designed for illustrating horizontal barplots depicting enrichment analysis results.
#'
#' @format A data frame with 14 rows and 13 variables:
#' \describe{
#'   \item{Category}{Character. Category of the pathway, typically indicating it's a KEGG pathway.}
#'   \item{Description}{Character. Description of the specific pathway, including KEGG identifiers.}
#'   \item{Count}{Numeric. Number of genes associated with the pathway.}
#'   \item{\%}{Numeric. Percentage of total genes related to the pathway.}
#'   \item{PValue}{Numeric. Statistical p-value indicating the significance of the pathway's enrichment.}
#'   \item{Genes}{Character. List of gene identifiers associated with the pathway, separated by commas.}
#'   \item{List Total}{Numeric. Total number of genes in the list being analyzed.}
#'   \item{Pop Hits}{Numeric. Number of pathway hits in the population.}
#'   \item{Pop Total}{Numeric. Total number of genes in the population.}
#'   \item{Fold Enrichment}{Numeric. Measure of the fold increase in pathway representation.}
#'   \item{Bonferroni}{Numeric. Bonferroni-adjusted p-value for multiple comparisons correction.}
#'   \item{Benjamini}{Numeric. Benjamini-adjusted value for false discovery rate control.}
#'   \item{FDR}{Numeric. False discovery rate-adjusted p-value.}
#' }
#' @source Simulated data for demonstrating horizontal barplot functionality.
"BetterVis_Barplot_Horizontal_example"




#' Ring Example Data
#'
#' A dataset to demonstrate the functionality of creating circular dendrogram plots.
#' It contains hierarchical data representing relationships between traits, SNPs, and genes.
#'
#' @format A data frame with example rows and 4 variables:
#' \describe{
#'   \item{trait}{Character. The highest level of the hierarchy, e.g., a disease or trait.}
#'   \item{SNP}{Character. Single Nucleotide Polymorphism identifier, the second level.}
#'   \item{Gene}{Character. Gene identifier, the lowest level of the hierarchy.}
#'   \item{value}{Numeric. A value associated with each record, used to determine node size.}
#' }
#' @source Simulated data for visualization examples.
"BetterVis_Ring_example"



#' Example Data for a Bi-Continuous Heatmap
#'
#' A dataset in long format suitable for creating a tile-based heatmap with
#' `BetterVis_Heatmap_Bicontinuous`.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{ID}{A variable for the x-axis.}
#'   \item{name}{A variable for the y-axis.}
#'   \item{value}{A continuous variable for the fill color.}
#' }
#' @source Simulated data for visualization examples, originally from a CSV file.
"BetterVis_Heatmap_Bicontinuous_example"


#' Example Data for a Circular Heatmap
#'
#' A dataset in long format suitable for creating a circular heatmap with
#' `BetterVis_Heatmap_Circle`.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{value}{A continuous variable for the fill color.}
#'   \item{id}{A categorical variable for the outer ring labels.}
#'   \item{name}{A categorical variable for the inner rings.}
#' }
#' @source Simulated data for visualization examples, originally from a CSV file.
"BetterVis_Heatmap_Circle_example"


#' Example Data for a Common Heatmap
#'
#' A numeric data frame in wide format suitable for creating a heatmap with
#' `BetterVis_Heatmap_Common`. Rows and columns represent different features
#' or samples, and the cells contain numeric values.
#'
#' @format A data frame with numeric values.
#' @source Simulated data for visualization examples, originally from a CSV file.
"BetterVis_Heatmap_Common_example"




#' Example Expression Data for Mantel Test Heatmap
#'
#' An example numeric data frame representing expression data, suitable for the
#' `expression_data` argument in `BetterVis_Heatmap_Mantel`.
#'
#' @format A data frame with numeric values.
#' @source Simulated data for visualization examples.
"BetterVis_Heatmap_Mantel_example1"

#' Example Phenotype Data for Mantel Test Heatmap
#'
#' An example data frame representing clinical or phenotype data, suitable for the
#' `phenotype_data` argument in `BetterVis_Heatmap_Mantel`.
#'
#' @format A data frame with various clinical indicators.
#' @source Simulated data for visualization examples.
"BetterVis_Heatmap_Mantel_example2"



#' Example Data for a Multi-Layered Heatmap
#'
#' A dataset containing a mix of numeric and categorical variables suitable for
#' creating a composite heatmap with `BetterVis_Heatmap_Multi`.
#'
#' @format A data frame with columns for sample IDs, numeric values, and
#'   categorical annotations.
#' \describe{
#'   \item{case.ID}{Character. A unique identifier for each sample on the x-axis.}
#'   \item{Primary Metastasis}{Character. An example annotation variable.}
#'   \item{Consensus classification}{Character. An example annotation variable.}
#'   \item{UNC classification}{Character. An example annotation variable.}
#'   \item{...}{Other columns are numeric variables for the main heatmap.}
#' }
#' @source Simulated data for visualization examples, originally from a CSV file.
"BetterVis_Heatmap_Multi_example"







#' Example Data for a Dot Plot / Heatmap
#'
#' A numeric matrix representing gene expression data, suitable for use with
#' `BetterVis_DotPlot_Heatmap`. Rows represent genes and columns represent
#' cell types or sample groups.
#'
#' @format A numeric matrix.
#' @source Simulated data for visualization examples, originally from a CSV file.
"BetterVis_DotPlot_Heatmap_example"



#' Example Data for a Beeswarm Plot
#'
#' A dataset containing city miles per gallon (MPG) for various vehicle types,
#' suitable for creating a beeswarm plot with `BetterVis_Dotplot_Beeswarm`.
#'
#' @format A data frame with columns including:
#' \describe{
#'   \item{Type}{A categorical variable for the x-axis, representing vehicle type.}
#'   \item{MPG.city}{A numeric variable for the y-axis, representing city MPG.}
#' }
#' @source Simulated data for visualization examples, originally from a CSV file.
"BetterVis_Dotplot_Beeswarm_example"


#' Example Data for a Mixed Dot Plot and Bar Plot
#'
#' A dataset containing measurements for different species of penguins, suitable
#' for creating a scatter plot with `BetterVis_Dotplot_Mixbar`. This data is
#' derived from the `palmerpenguins` package.
#'
#' @format A data frame with columns including:
#' \describe{
#'   \item{species}{A character string indicating the penguin species.}
#'   \item{bill_length_mm}{A numeric variable for the x-axis.}
#'   \item{bill_depth_mm}{A numeric variable for the y-axis.}
#'   \item{body_mass_g}{A numeric variable for point size.}
#' }
#' @source Data originally from the `palmerpenguins` package, by way of a CSV file.
"BetterVis_Dotplot_Mixbar_example"


#' Example Data for a Multi-Faceted Dot Plot
#'
#' A dataset containing the classic `iris` measurements, suitable for creating
#' a matrix of scatter plots with `BetterVis_Dotplot_Multifacet`.
#'
#' @format A data frame with 150 rows and 5 columns:
#' \describe{
#'   \item{Sepal.Length}{Numeric. Length of the sepal.}
#'   \item{Sepal.Width}{Numeric. Width of the sepal.}
#'   \item{Petal.Length}{Numeric. Length of the petal.}
#'   \item{Petal.Width}{Numeric. Width of the petal.}
#'   \item{Species}{Factor. The species of iris.}
#' }
#' @source The base R `iris` dataset, by way of a CSV file.
"BetterVis_Dotplot_Multifacet_example"


#' Example Data for a Smoothline Dot Plot
#'
#' A dataset containing the classic `iris` measurements, suitable for creating
#' a scatter plot with smoothed lines using `BetterVis_Dotplot_Smoothline`.
#'
#' @format A data frame with 150 rows and 5 columns:
#' \describe{
#'   \item{Sepal.Length}{Numeric. Length of the sepal.}
#'   \item{Sepal.Width}{Numeric. Width of the sepal.}
#'   \item{Petal.Length}{Numeric. Length of the petal.}
#'   \item{Petal.Width}{Numeric. Width of the petal.}
#'   \item{Species}{Factor. The species of iris.}
#' }
#' @source The base R `iris` dataset, by way of a CSV file.
"BetterVis_Dotplot_Smoothline_example"



#' Example Data for a Ternary Plot
#'
#' A dataset containing three compositional variables (x, y, z) that sum to a
#' constant, plus a fourth variable ('size') for mapping to point aesthetics.
#' This data is suitable for use with `BetterVis_Dotplot_Ternary`.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{x, y, z}{Numeric. The three compositional variables for the ternary axes.}
#'   \item{size}{Numeric. A variable to be mapped to point size and color.}
#' }
#' @source Simulated data for visualization examples, originally from a CSV file.
"BetterVis_Dotplot_Ternary_example"






#' Example Data for a Faceted Ridgeline Plot
#'
#' A dataset containing numeric scores and multiple categorical groupings
#' suitable for creating a faceted ridgeline plot with
#' `BetterVis_Ridgeline_Facet`.
#'
#' @format A data frame with columns including:
#' \describe{
#'   \item{CytoTRACEScore}{A numeric variable for the x-axis.}
#'   \item{patient}{A categorical variable for the y-axis, defining the ridges.}
#'   \item{smoking1}{A categorical variable for faceting.}
#'   \item{Group}{Another categorical variable for faceting.}
#'   \item{mutationGroup}{An additional categorical variable for annotation.}
#' }
#' @source Simulated data for visualization examples, originally from a CSV file.
"BetterVis_Ridgeline_Facet_example"


#' Example Data for a Single Ridgeline Plot
#'
#' A dataset containing numeric scores and categorical groupings suitable for
#' creating a ridgeline plot with `BetterVis_Ridgeline_Single`.
#'
#' @format A data frame with columns including:
#' \describe{
#'   \item{CytoTRACEScore}{A numeric variable for the x-axis.}
#'   \item{patient}{A categorical variable for the y-axis, defining the ridges.}
#'   \item{mutationGroup}{An additional categorical variable for annotation.}
#' }
#' @source Simulated data for visualization examples, originally from a CSV file.
"BetterVis_Ridgeline_Single_example"


#' Example Data for UpSet Plot
#'
#' A named list of character vectors, where each vector represents a set of
#' elements. This dataset is used to demonstrate the functionality of
#' `BetterVis_Venn_UpsetR`.
#'
#' @format A named list with multiple elements, each being a character vector.
#' @source Simulated data for visualization examples, originally stored in an .rds file.
"BetterVis_Venn_UpsetR_example"


#' Example Data for Circos Interaction Plot (Sectors)
#'
#' A dataset containing the primary sector and annotation track information
#' for demonstrating `BetterVis_Circos_Interaction`.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{ID_Gene}{Character. Unique identifiers for the main sectors.}
#'   \item{type_main}{Character. A classification for coloring the main sectors.}
#'   \item{type_col1}{Character. Classification for the first annotation track.}
#'   \item{type_col2}{Character. Classification for the second annotation track.}
#' }
#' @source Simulated data for visualization examples.
"BetterVis_Circos_Interaction_example1"







#' Example Data for Circos Interaction Plot (Links)
#'
#' A dataset containing interaction data (links) for demonstrating
#' the chord diagram functionality in `BetterVis_Circos_Interaction`.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{ID}{Character. The source of the interaction link.}
#'   \item{Gene}{Character. The target of the interaction link.}
#'   \item{Correlation}{Numeric. The strength of the interaction, used for link width.}
#'   \item{Pvalue}{Numeric. The statistical significance of the interaction.}
#' }
#' @source Simulated data for visualization examples.
"BetterVis_Circos_Interaction_example2"
