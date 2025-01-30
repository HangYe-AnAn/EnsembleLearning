#' Linear Regression Model
#'
#' The linear_model function fits a linear regression model to the given data.
#'
#' @param X Matrix or data frame of predictor variables.
#' @param y Response variable.
#' @return A fitted linear regression model.
#'
#' @details
#' Linear regression is a statistical method used to model the relationship between one or more predictor variables (independent variables) and a response variable (dependent variable). The model assumes a linear relationship between the predictors and the response. The function uses lm function (For more information see \code{\link[base]{lm}}) to fit and compute the coefficients.
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
#' result <- linear_model(predictors, response)
#' print(result)
#' @export
linear_model <- function(X, y){
  data <- cbind(X, y)
  model <- lm(y ~ ., data = as.data.frame(data))
  return(model)
}
