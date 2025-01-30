#' Handle Missing Values
#'
#' The handle_missing_values function checks for missing values in the predictor variables (X) and the response variable (y).
#' Depending on the user's choice, it offers methods to handle missing values such as NA omit removal, mean imputation, median imputation, mode imputation, or retaining NA values.
#'
#' @param X A data frame or matrix containing the predictor variables.
#' @param y A vector containing the response variable (Binary or continuous).
#' @return The function a list containing the predictor variables (X) and the response variable (y), after handling missing values.
#'
#' @details
#' Missing values are a common issue in datasets and can affect the performance of models if not handled properly.
#' The `handle_missing_values` function provides several options for handling missing values, including:
#' \enumerate{
#' \item  Removing rows with missing values (`na.omit`)
#' \item  Imputing missing values with the mean
#' \item  Imputing missing values with the median
#' \item  Imputing missing values with the mode
#' \item  Keeping NA values in the dataset for the next step calculation
#' }
#' \cr
#' \cr
#' For more details about handling missing value, we recommend mice package if you can use.
#'
#' @section How to Use:
#' 1. Load the package using `library(my_package)`.
#' \cr
#' \cr
#' 2. Prepare your dataset `my_data`, ensuring it contains both predictors and the response variable.
#' \cr Or you already have the X (predictors) and y (response) variable.
#' \cr
#' 3. Using `handle_missing_values(X, y)` with your dataset to handle missing values.
#' \cr
#' \cr
#' 4. Choose an option to handle missing values:
#' \enumerate{
#'   \item   - Enter 1 to remove rows with missing values.
#'   \item   - Enter 2 to impute missing values with the mean.
#'   \item   - Enter 3 to impute missing values with the median.
#'   \item   - Enter 4 to impute missing values with the mode.
#'   \item   - Enter 5 to keep NA values in the dataset.
#'}
#' @section Conclusion:
#' Handling missing values is crucial for data analysis and modeling.
#' By using the `handle_missing_values` function, you can choose the most suitable method for your dataset.
#'
#' @examples
#' # Load the package
#' library(simpleEnsembleGroup7)
#'X <- matrix(c(1, 2, NA, 4, 5, NA, 2, 3, 4, 5), ncol = 2)
#'y <- c(1, NA, 3, 4, 5)
#'data_handled <- handle_missing_values(X, y)
#'View(data_handled$X)
#'
#'
#' @export
#'
handle_missing_values <- function(X, y) {
  test_missing_proportion <- function(X, y) {
    missing_proportion_X <- colMeans(is.na(X))
    missing_proportion_y <- mean(is.na(y))
    return(list(missing_proportion_X = missing_proportion_X, missing_proportion_y = missing_proportion_y))
  }
  missing_proportions <- test_missing_proportion(X, y)
  missing_proportion_X <- missing_proportions$missing_proportion_X
  missing_proportion_y <- missing_proportions$missing_proportion_y

  if (anyNA(X) || anyNA(y)) {
    cat("Missing values found in the dataset.\n")
    cat("Proportion of missing values in each column of X:\n")
    cat(missing_proportion_X, "\n")
    cat("Proportion of missing values in y:\n")
    cat(missing_proportion_y, "\n")
    cat("In many cases, a small proportion of missing values (smaller than 5 - 10%) in a column is often considered manageable. ")
    cat("Recommend to choose choice one which using na.omit to remove rows with missing values. ")
    cat("For those dataset with high proportion of missing values (more than 10%) in a column, user should check and review the dataset carefully.\n")
    cat("Choose a method to handle missing values:\n")
    cat("1. Remove rows with missing values (na.omit)\n")
    cat("2. Impute missing values with the mean\n")
    cat("3. Impute missing values with the median\n")
    cat("4. Impute missing values with the mode\n")
    cat("5. Keep using NA values in the dataset for the next step calculation\n")

    choice <- as.integer(readline("Enter your choice (1, 2, 3, 4, or 5): "))

    if (choice == 1) {
      complete_cases <- complete.cases(X, y)
      X <- X[complete_cases, ]
      y <- y[complete_cases]
      cat("Missing values have been removed using na.omit.\n")
    } else if (choice == 2) {
      for (i in seq_len(ncol(X))) {
        X[, i][is.na(X[, i])] <- mean(X[, i], na.rm = TRUE)
      }
      y[is.na(y)] <- mean(y, na.rm = TRUE)
      cat("Missing values have been imputed using the mean.\n")
    } else if (choice == 3) {
      for (i in seq_len(ncol(X))) {
        X[, i][is.na(X[, i])] <- median(X[, i], na.rm = TRUE)
      }
      y[is.na(y)] <- median(y, na.rm = TRUE)
      cat("Missing values have been imputed using the median.\n")
    } else if (choice == 4) {
      calculate_mode <- function(x) {
        tbl <- table(x)
        if (length(tbl[tbl == max(tbl)]) == 1) {
          return(as.numeric(names(tbl)[which.max(tbl)]))
        } else {
          # Return NA if there are multiple modes
          return(NA)
        }
      }
      for (i in seq_len(ncol(X))) {
        col_values <- X[, i]
        if (any(is.na(col_values))) {
          mode_value <- calculate_mode(col_values)
          X[, i][is.na(X[, i])] <- mode_value
        }
      }
      if (any(is.na(y))) {
        mode_value <- calculate_mode(y)
        y[is.na(y)] <- mode_value
      }
      cat("Missing values have been imputed using the mode.\n")
    } else if (choice == 5) {
      cat("Warning: NA values will be used in the dataset for the next step calculation.\n")
    } else {
      cat("Invalid choice. Please enter 1, 2, 3, 4, or 5.\n")
    }
  } else {
    cat("No missing values found in the dataset.\n")
  }

  return(list(X = as.matrix(X), y = y))
}
