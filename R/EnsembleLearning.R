#' Ensemble Learning Function
#'
#' This function implements an ensemble learning approach that allows users to train
#' multiple models on the same dataset and aggregate the predictions into a single output.
#' It supports both regression and classification tasks.
#'
#' @param X A matrix or data frame of predictor variables.
#' @param y A vector representing the response variable, which should be binary for classification tasks
#' and continuous for regression tasks.
#' @param model_types A character vector specifying the types of models to train.
#' Valid options include "linear", "ridge", "lasso", "elastic", and "svm". Ex. model_types = c("linear","ridge", "lasso", "elastic")
#' @param return_accuracy A logical flag that determines whether to calculate and return the
#' accuracy of the predictions (for classification tasks only). Default is FALSE.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{prediction}: The aggregated prediction from the ensemble. For classification,
#'   this is based on majority voting; for regression, it is the average of the predictions.
#'   \item \code{models}: Details of all the individual models trained during the ensemble process.
#'   This allows for further inspection and analysis.
#'   \item \code{accuracy} (only for svm): The accuracy of the ensemble predictions, provided if
#'   \code{return_accuracy} is TRUE and the task is classification.
#' }
#'
#' @details
#' The `EnsembleLearning` function trains each specified model using the provided data,
#' then aggregates their predictions into a single result. The aggregation method depends
#' on the type of task:
#' \itemize{
#'   \item \strong{Classification}: Uses majority voting to determine the final class.
#'   \item \strong{Regression}: Calculates the mean of all model predictions.
#' }
#' This method enhances prediction robustness by combining the unique strengths of
#' different modeling approaches and reducing the risk of overfitting.
#'
#' @examples
#' library(simpleEnsembleGroup7)
#' data(mtcars)
#' mtcars$high_mpg <- as.numeric(mtcars$mpg > median(mtcars$mpg))
#' X <- mtcars[, c("wt", "hp", "qsec", "drat")]
#' y <- mtcars$high_mpg
#' results <- EnsembleLearning(X, y, c("linear", "ridge", "svm"), return_accuracy=TRUE)
#' print(results$prediction)
#' if (!is.null(results$accuracy)) {
#'   print(paste("Accuracy:", results$accuracy))
#' }
#'
#'
#' @export
EnsembleLearning <- function(X, y, model_types, return_accuracy = FALSE) {
  # Determine if the task is classification or regression
  task <- ifelse(is_binary(y), "classification", "regression")




  # Prepare lists to store model predictions and models
  predictions_list <- list()
  models_list <- list()

  # Train each model specified by the user
  for (model_type in model_types) {
    if (model_type %in% c("linear", "ridge", "lasso", "elastic")) {
      # Set alpha for different regression types
      alpha <- switch(model_type,
                      "linear" = 0,
                      "ridge" = 0,
                      "lasso" = 1,
                      "elastic" = NULL)  # Placeholder for Elastic Net optimization

      if (model_type == "elastic") {
        # Optimize Elastic Net separately
        fit <- optimize_elastic_net(X, y, task)
      } else {
        # Fit model using glmnet with cross-validation
        fit <- cv.glmnet(as.matrix(X), y, alpha = alpha, family = ifelse(task == "classification", "binomial", "gaussian"))
      }

      # Use the lambda that gives the minimum mean cross-validated error
      optimal_lambda <- fit$lambda.min

      # Store the model
      models_list[[model_type]] <- fit

      # Predict using the model
      if (task == "classification") {
        # For classification, predict class probabilities and select class 2
        pred <- predict(fit, newx = as.matrix(X), type = "response", s = optimal_lambda)
        pred_class <- ifelse(pred > 0.5, 1, 0)
        predictions_list[[model_type]] <- pred_class
      } else {
        # For regression, predict the numeric response
        pred <- predict(fit, newx = as.matrix(X), type = "response", s = optimal_lambda)
        predictions_list[[model_type]] <- pred
      }
    } else if (model_type == "svm") {
      # Train SVM model
      svm_model <- svm(X, y, probability = task == "classification")

      # Store the model
      models_list[[model_type]] <- svm_model

      # Predict using the model
      if (task == "classification") {
        pred <- predict(svm_model, X, probability = TRUE)
        predictions_list[[model_type]] <- as.numeric(pred)
      } else {
        pred <- predict(svm_model, X)
        predictions_list[[model_type]] <- pred
      }
    }
  }

  # Aggregate predictions
  final_prediction <- if (task == "classification") {
    apply(do.call(cbind, predictions_list), 1, function(row) {
      names(which.max(table(row)))
    })
  } else {
    rowMeans(do.call(cbind, predictions_list))
  }

  # Calculate accuracy if requested and applicable
  accuracy <- if (return_accuracy && task == "classification") {
    calculate_accuracy(final_prediction, y)
  } else {
    NA  # Not applicable for regression or if not requested
  }

  # Return both the aggregated prediction and the models
  result <- list(prediction = final_prediction, models = models_list)
  if (return_accuracy) {
    result$accuracy <- accuracy
  }

  return(result)
}
#' @keywords internal function not for user
calculate_accuracy <- function(predictions, actual) {
  correct <- sum(predictions == actual)
  total <- length(actual)
  accuracy <- correct / total
  return(accuracy)
}

#' @keywords internal function not for user
optimize_elastic_net <- function(X, y, task) {
  library(glmnet)
  # Define a grid of alpha values (mixing parameter between ridge and lasso)
  alpha_grid <- seq(0.1, 0.9, by = 0.2)

  # Placeholder for the best model
  best_model <- NULL
  best_cv_score <- Inf

  # Loop over alpha grid
  for (alpha in alpha_grid) {
    # Fit model using cross-validation
    fit <- cv.glmnet(as.matrix(X), y, alpha = alpha, family = ifelse(task == "classification", "binomial", "gaussian"))

    # Check if this model is better
    if (fit$cvm[which.min(fit$cvm)] < best_cv_score) {
      best_cv_score <- fit$cvm[which.min(fit$cvm)]
      best_model <- fit
    }
  }

  return(best_model)
}

is_binary <- function(y) {
  return (length(unique(y)) == 2)
}

