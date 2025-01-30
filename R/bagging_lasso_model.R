#' Bagging Lasso Model
#'
#' The bagging_Lasso_model function takes predictor variables (X) and a response variable (y) as inputs, along with optional parameters for the number of bags (n_bags), the number of top predictors to consider (K). It then fits multiple Lasso regression models using bootstrap sampling and aggregates their predictions to improve model accuracy.
#'
#' @param X A data frame or matrix containing the predictor variables.
#' @param y A vector containing the response variable (Binary or continuous).
#' @param n_bags Number of bootstrap samples to create (default is 100).
#' @param K A positive integer specifying the number of top informative predictors to select; must be greater than the number of predictors. (Optional)
#'
#' @details
#' The function generates n_bags bootstrap samples from the original dataset.
#' \cr
#' \cr
#' \enumerate{For each bootstrap sample:
#' \item It fits a Lasso regression model (using \code{\link[glmnet]{glmnet}}) using cross-validation to find the optimal regularization parameter, or minimum lambda. It then extracts the coefficients of the fitted Lasso models.
#' \item If the K is specified, and if p >> n, it will pre-screen for top K most “informative” predictors (For more information see \code{\link[simpleEnsembleGroup7]{returnTopK}}). Then it fits a Lasso regression model(using \code{\link[glmnet]{glmnet}}) using the top K predictors and using cross-validation to find the optimal regularization parameter, or minimum lambda. It then extracts the coefficients of the fitted Lasso models.
#' }
#' \cr
#' \cr
#' Coefficient Aggregation: The coefficients from all bootstrap samples are combined and aggregated by taking the average, or row means, across all bootstrap samples with equal weights. This provides the final coefficient estimates of the final Lasso model.
#' \cr
#' \cr
#' Variable Importance: The function calculates the importance of each predictor variable by counting the number of times each variable appears in the non-missing coefficients across all bootstrap samples.
#' \cr
#' \cr
#' Output: The function returns a list containing the average coefficient estimates (predictions) and the variable importance scores (variable_importance).
#' \cr
#' \cr
#'  \eqn{\alpha=1} corresponds to the Lasso penalty.
#'
#' @return bagging_lasso_model returns an object of class "list". A list with:
#' \item{predictions}{The averaged coefficient estimates of the final fitted model.}
#' \item{variable_importance}{Importance of each predictor variable calculated by counting the number of times each variable appears in the non-missing coefficients across all bootstrap samples.}
#'
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
#' # Run bagging lasso regression
#' result <- bagging_lasso_model(predictors, response, K = 5)
#' # Print the average coefficient estimate
#' print(result$predictions)
#' # Print variable importance
#' print(result$variable_importance)
#' @export

bagging_lasso_model <- function(X, y, n_bags = 100, K = NULL) {
  library(glmnet)
  if(is.null(K)){
    K <- ncol(X) # K = number of total predictors
  }

  if(!is.numeric(n_bags) && n_bags > 0){
    stop("n_bags have to be numeric and greater than zero.")
  }


  # Initialize NaiveScore with zero counts
  NaiveScore <- matrix(0, nrow = 1, ncol = ncol(X))
  colnames(NaiveScore) <- colnames(X)
  all_predictors <- colnames(X)  # Store all possible predictors

  # Generate bootstrap indices
  indices_list <- generate_bootstrap_indices(X, n_bags)

  coefs_storage <- setNames(vector("list", length = n_bags), seq_len(n_bags))
  final_model <- NULL  # To store the last model


  for (i in seq_along(indices_list)) {
    boot_data <- X[indices_list[[i]], , drop = FALSE]
    boot_y <- y[indices_list[[i]]]

    # Get top K predictors
    selected_data <- returnTopK(boot_data, boot_y, K)

    top_predictors <- colnames(selected_data$X)  # Ensure top_predictors are correctly extracted

    if (!is.null(top_predictors) && length(top_predictors) > 0) {
      NaiveScore[1, top_predictors] <- NaiveScore[1, top_predictors] + 1
      fit_results <- fit_lasso_model(selected_data$X, boot_y)

      if (!is.null(fit_results$coefs)) {  # Ensure there's more than just an intercept
        coefs_storage[[i]] <- setNames(array(NA, dim = length(all_predictors) + 1), c("Intercept", all_predictors))
        coefs_storage[[i]][c("Intercept", top_predictors)] <- fit_results$coefs
        final_model <- fit_results$model  # Store the last successful model
      }
    }
  }

  # Combine and average coefficients across all bootstraps
  coefs_matrix <- do.call(rbind, coefs_storage)  # Bind rows to align each bootstrap's coefficients
  avg_coefs <- colMeans(coefs_matrix, na.rm = TRUE, dims = 1)  # Calculate mean ignoring NA

  # Remove NA columns from the final predictions
  valid_coefs <- !is.na(avg_coefs)
  predictions <- avg_coefs[valid_coefs]

  NaiveScore <- NaiveScore/n_bags
  percentage_scores <- sprintf("%.2f%%", NaiveScore * 100)
  formatted_NaiveScore <- matrix(percentage_scores, nrow = nrow(NaiveScore), ncol = ncol(NaiveScore), dimnames = list(NULL, colnames(NaiveScore)))
  #NaiveScore <- matrix(sprintf("%.2f%%",NaiveScore*100),nrow=nrow(NaiveScore))

  return(list(predictions = predictions, variable_importance = formatted_NaiveScore))
}

#' @keywords internal function not for user
generate_bootstrap_indices <- function(X, n_bags) {
  library(glmnet)
  n <- nrow(X)
  lapply(1:n_bags, function(i) sample(1:n, n, replace = TRUE))
}

fit_lasso_model <- function(X, y) {
  require(glmnet)
  X_matrix <- as.matrix(X)

  y_vector <- as.matrix(y)  # Ensure y is in the correct format

  # Perform cross-validation to determine the optimal lambda
  cv_model <- cv.glmnet(X_matrix, y_vector, alpha = 1, nfolds = 10)
  best_lambda <- cv_model$lambda.min

  # Fit the final model using the optimal lambda
  model <- glmnet(X_matrix, y_vector, alpha = 1, lambda = best_lambda)

  # Extract the full set of coefficients, including the intercept
  coefs <- coef(model, s = "lambda.min")  # Get coefficients at the best lambda
  return(list(coefs = as.numeric(coefs), model = model))
}

# Example usage
#data(mtcars)
#X <- mtcars[, -which(names(mtcars) == "mpg")]
#y <- mtcars$mpg

#results <- bagging_lasso_model(X, y, n_bags = 100, K = 3)
#print(results)
