#' Elastic Net Regression Model
#'
#' The elastic_model function fits an elastic net regression model to the given data using cross-validation to select the optimal regularization parameter (lambda).
#'
#' @param x Matrix of predictor variables.
#' @param y Response variable.
#' @param alpha The mixing parameter for elastic net regularization.
#' @return A fitted elastic net regression model.
#'
#' @details
#' Elastic net regression is a hybrid of ridge and lasso regression techniques, offering a balance between the two. It introduces two regularization parameters: alpha (mixing parameter) and lambda (regularization parameter). The mixing parameter controls the balance between ridge and lasso penalties.
#' When alpha = 0, the model is equivalent to ridge regression, and when alpha = 1, it is equivalent to lasso regression.
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
#' result <- elastic_model(predictors, response, alpha = 0.5)
#' print(result)
#' @export
elastic_model <- function(X, y, alpha = alpha, nfolds = 5) {

  # Perform cross-validation to find the best lambda
  cv_fit <- cv.glmnet(X, y, alpha = alpha, nfolds = nfolds)

  # Extract the best lambda
  best_lambda <- cv_fit$lambda.min

  # Fit ridge regression model using the best lambda
  elastic_fit <- glmnet(X, y, alpha = alpha, lambda = best_lambda)
}
