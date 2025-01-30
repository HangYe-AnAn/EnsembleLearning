#' Support Vector Machine (SVM) Model
#'
#' This function trains a support vector machine (SVM) model on the given dataset.
#'
#' @param X Input features (data frame or matrix).
#' @param y Response variable (numeric or factor).
#'
#' @return Trained SVM model.
#'
#' @references
#' This function uses the 'svm' function from the 'e1071' package.
#'
#' @examples
#' # Load example dataset
#' data(iris)
#' X <- iris[, -which(names(iris) == "Species")]
#' y <- iris$Species
#'
#' # Train an SVM model
#' svm_model <- svm_model(X, y)
#' svm_model
#'
#' @import e1071
#'
#' @export
svm_model <- function(X, y) {
  library(e1071)

  # Train SVM model
  model <- svm(x = X, y = y, kernel = "linear", cost = 1)

  return(model)
}


