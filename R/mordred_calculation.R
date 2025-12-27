#' Read Mordred Descriptor File
#'
#' Reads a CSV file containing Mordred descriptors. It handles common Mordred errors
#' (like "Error" or "nan" strings) by converting them to numeric `NA` values.
#'
#' @param file Path to the CSV file.
#' @return A numeric data frame with row names preserved.
#' @export
read_mordred <- function(file) {
  # Read as character initially to prevent factor conversion
  raw_df <- utils::read.csv(file, header = TRUE, row.names = 1, stringsAsFactors = FALSE)

  # Convert all columns to numeric. Non-numeric strings become NA.
  numeric_df <- as.data.frame(lapply(raw_df, function(x) {
    as.numeric(as.character(x))
  }))

  # Restore naming metadata
  rownames(numeric_df) <- rownames(raw_df)
  colnames(numeric_df) <- colnames(raw_df)

  return(numeric_df)
}

#' Remove Non-Numeric and NA Values
#'
#' Filters the data frame to keep only numeric columns and then removes any
#' column containing one or more NA values.
#'
#' @param df A data frame.
#' @return A numeric data frame with no missing values.
#' @export
drop_na_values <- function(df) {
  # Security check: Ensure we only work with numeric columns
  numeric_only <- df[, sapply(df, is.numeric)]

  # Remove columns containing NAs (often caused by Mordred calculation errors).
  mordred_clean <- numeric_only[, colSums(is.na(numeric_only)) == 0]

  return(mordred_clean)
}

#' Drop Near-Zero Variance Features
#'
#' Removes features that have very little variance (e.g., constant or nearly
#' constant values) based on a percentage cutoff.
#'
#' @param df A data frame.
#' @param cutPercentage The percentage cutoff for unique values (used by caret).
#' @return A data frame with low-variance features removed.
#' @importFrom caret nearZeroVar
#' @export
drop_zero_var <- function(df, cutPercentage) {
  drop <- caret::nearZeroVar(x = df, names = TRUE, uniqueCut = cutPercentage)
  df_cleared <- df[, !(names(df) %in% drop)]
  return(df_cleared)
}

#' Standardize and Remove Highly Correlated Features
#'
#' Scales the data to unit variance and identifies features that are highly
#' correlated (redundant) to remove them.
#'
#' @param df A data frame of numeric features.
#' @param cutoff Correlation coefficient cutoff (e.g., 0.9).
#' @return A standardized data frame with redundant features removed.
#' @importFrom stats cor
#' @importFrom caret findCorrelation
#' @export
standartize_correlate <- function(df, cutoff) {
  # Center and scale the data using the base scale function
  scaled_file <- data.frame(base::scale(df))

  # Calculate correlation matrix using stats package
  cor_matrix <- stats::cor(scaled_file)

  # Find indices of features to drop based on cutoff
  hc <- caret::findCorrelation(cor_matrix, cutoff = cutoff)
  hc <- sort(hc)

  # Remove identified columns if they exist
  if(length(hc) > 0) {
    redundancy_cleared <- scaled_file[, -c(hc)]
  } else {
    redundancy_cleared <- scaled_file
  }

  return(redundancy_cleared)
}

#' Perform Principal Component Analysis (PCA)
#'
#' Runs a PCA on the cleaned and standardized data frame to reduce dimensionality.
#'
#' @param clean_df A cleaned and standardized data frame.
#' @return A `prcomp` object containing the PCA results.
#' @importFrom stats prcomp
#' @export
pca_analysis <- function(clean_df) {
  pc_result <- stats::prcomp(clean_df)
  print(summary(pc_result))
  return(pc_result)
}

#' Visualize PCA Results
#'
#' Generates professional plots for PCA variable contributions and the
#' quality of representation (Cos2).
#'
#' @param pc_file A `prcomp` result object.
#' @param pccomp The number of principal components to visualize.
#' @return Displays PCA plots in the active graphics device.
#' @importFrom factoextra get_pca_var fviz_pca_var fviz_cos2
#' @export
visualize <- function(pc_file, pccomp) {
  # Get PCA variable results
  var <- factoextra::get_pca_var(pc_file)

  # Plot variables (Correlation circle)
  print(factoextra::fviz_pca_var(pc_file, col.var = "black"))

  # Plot Cos2 (Quality of representation) for top 10 variables
  print(factoextra::fviz_cos2(pc_file, choice = "var", axes = 1:pccomp, top = 10))
}
