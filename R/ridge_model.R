#' Ridge Regression Model
#'
#' The ridge_model function fits a ridge regression model to the given data using cross-validation to select the optimal regularization parameter (lambda).
#'
#' @param X Matrix of predictor variables.
#' @param y Response variable.

#' @return A fitted ridge regression model.
#'
#' @details
#' Ridge regression is a linear regression technique that introduces regularization to mitigate multicollinearity and overfitting. It adds a penalty term to the ordinary least squares objective function, controlled by the regularization parameter (lambda).
#'
#' @examples
#' # Load required libraries
#' library(glmnet)
#' # Generate sample data
#' n <- 100  # Number of observations
#' n_predictors <- 10  # Number of predictors
#' # Create predictor variables
#' predictors <- matrix(rnorm(n * n_predictors), ncol = n_predictors)
#'
#' # Create names for the predictors
#' predictor_names <- c("Predictor1", "Predictor2", "Predictor3", "Predictor4", "Predictor5", "Predictor6", "Predictor7", "Predictor8", "Predictor9", "Predictor10")
#' # Assign names to the columns of the predictors matrix
#' colnames(predictors) <- predictor_names
#'
#' # Generate response variable
#' response <- rnorm(n)
#'
#' result <- ridge_model(predictors, response)
#' print(result)
#' @export


ridge_model <- function(X, y) {

  # Perform cross-validation to find the best lambda
  cv_fit <- cv.glmnet(X, y, alpha = 0)

  # Extract the best lambda
  best_lambda <- cv_fit$lambda.min

  # Fit ridge regression model using the best lambda
  ridge_fit <- glmnet(X, y, alpha = 0, lambda = best_lambda)
}
