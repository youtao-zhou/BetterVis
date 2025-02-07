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





