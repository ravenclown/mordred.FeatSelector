#' Read Mordred Descriptor File
#'
#' Reads a CSV file containing Mordred descriptors, assuming the first column contains row names.
#'
#' @param file Path to the CSV file.
#' @return A data frame containing the Mordred descriptors.
#' @export
read_mordred <- function(file) {
  mordred_file <- data.frame(utils::read.csv(file, header = TRUE, row.names = 1))
  return(mordred_file)
}

#' Remove Columns with NA Values
#'
#' Filters out any columns in the data frame that contain one or more NA values.
#'
#' @param df A data frame.
#' @return A data frame with no NA values.
#' @export
drop_na_values <- function(df) {
  # Fixed variable name from 'mordred' to 'df' for internal consistency
  mordred_clean <- df[, colSums(is.na(df)) == 0]
  return(mordred_clean)
}

#' Drop Near-Zero Variance Features
#'
#' Removes features that have very little variance based on a percentage cutoff.
#'
#' @param df A data frame.
#' @param cutPercentage The percentage cutoff for unique values (used by caret).
#' @return A data frame with near-zero variance features removed.
#' @importFrom caret nearZeroVar
#' @export
drop_zero_var <- function(df, cutPercentage) {
  drop <- caret::nearZeroVar(x = df, names = TRUE, uniqueCut = cutPercentage)
  df.ZeroVarCleared <- df[, !(names(df) %in% drop)]
  return(df.ZeroVarCleared)
}

#' Standardize and Remove Highly Correlated Features
#'
#' Scales the data and identifies features that are highly correlated to reduce redundancy.
#'
#' @param df A data frame of numeric features.
#' @param cutoff Correlation coefficient cutoff (e.g., 0.9).
#' @return A standardized data frame with redundant features removed.
#' @importFrom stats cor scale
#' @importFrom caret findCorrelation
#' @export
standartize_correlate <- function(df, cutoff) {
  scaled.file <- data.frame(scale(df))
  cor.matrix <- stats::cor(scaled.file)
  # Ensure caret is used for finding correlations
  hc <- caret::findCorrelation(cor.matrix, cutoff = cutoff) 
  hc <- sort(hc)
  redundancy.cleared.scaled.file <- scaled.file[, -c(hc)]
  return(redundancy.cleared.scaled.file)
}

#' Perform Principal Component Analysis (PCA)
#'
#' Runs a PCA on the cleaned and standardized data frame.
#'
#' @param clean_df A cleaned and standardized data frame.
#' @return A `prcomp` object containing the PCA results.
#' @importFrom stats prcomp
#' @export
pca_analysis <- function(clean_df) {
  pc.result <- stats::prcomp(clean_df)
  print(summary(pc.result)) 
  return(pc.result)
}

#' Visualize PCA Results
#'
#' Generates plots for PCA variable contributions and Cos2 quality.
#'
#' @param pc_file A `prcomp` result object.
#' @param pccomp The number of principal components to visualize.
#' @return Displays PCA plots.
#' @importFrom factoextra get_pca_var fviz_pca_var fviz_cos2
#' @export
visualize <- function(pc_file, pccomp) {
  var <- factoextra::get_pca_var(pc_file)
  # Variables of the PCA
  print(factoextra::fviz_pca_var(pc_file, col.var = "black"))
  # Contribution of top 10 variables to the selected components
  print(factoextra::fviz_cos2(pc_file, choice = "var", axes = 1:pccomp, top = 10))
}