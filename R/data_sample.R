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
