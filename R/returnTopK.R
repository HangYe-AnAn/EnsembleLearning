#' Top K Informative Predictors and Response
#'
#' This function evaluates the informativeness of predictors in relation to a given response variable
#' using appropriate statistical tests based on their data types. It returns the top K informative
#' predictors along with the response variable for further analysis or modeling.
#'
#' @param X A dataframe containing the predictors. Each column should represent one predictor.
#' @param y A vector containing the response variable, which can be either continuous or categorical.
#' @param k An integer specifying the number of top informative predictors to return.
#'
#' @return A list of class 'list' containing the following components:
#'   \itemize{
#'     \item \code{X}: A dataframe containing the top K informative predictors.
#'     \item \code{y}: The response variable as provided in the input.
#'   }
#'
#' @details The function applies different statistical tests depending on the data type of the predictors and the response:
#'   \itemize{
#'     \item For continuous predictors and a continuous response, it uses the Pearson Correlation Coefficient.
#'     \item For categorical predictors and a categorical response, it applies the Chi-square Test of Independence.
#'     \item For categorical predictors and a continuous response, it uses the Kruskal-Wallis Test.
#'   }
#' Note: The function handles NA values by omitting rows with NA in any of the predictors or the response during the analysis.
#'
#' @examples
#' data(mtcars)
#' X <- mtcars[, -which(names(mtcars) == "mpg")]  # Predictor dataset excluding the response
#' y <- mtcars$mpg  # Response variable
#' result <- returnTopK(X, y, k = 3)
#' print(result$X)  # Print the dataframe of top K predictors
#' print(result$y)  # Print the response variable
#'
#' @export
returnTopK <- function(X, y, K) {

  response_type <- var_type(y)
  #print(response_type)
  scores <- setNames(numeric(ncol(X)), colnames(X))


  for (predictor in names(scores)) {
    predictor_data <- X[,predictor]
    predictor_type <- var_type(predictor_data)

    if (predictor_type == "continuous" && response_type == "continuous") {
      score <- abs(cor(predictor_data, y, use = "complete.obs", method = "pearson"))
      scores[predictor] <- score
    } else if (predictor_type == "categorical" && response_type == "categorical") {
      if (any(table(predictor_data, y) < 5)) {
        scores[predictor] <- NA  # Chi-square test not valid with expected counts < 5
      } else {
        test <- chisq.test(table(predictor_data, y))
        scores[predictor] <- 1 - pchisq(test$statistic, df = test$parameter)
      }
    } else if (predictor_type == "categorical" && response_type == "continuous") {
      if (length(unique(predictor_data)) > 1) {
        test <- KrusKal.test(y ~ predictor_data)
        scores[predictor] <- test$p.value
      } else {
        scores[predictor] <- NA  # Not enough groups for comparison
      }
    } else {
      scores[predictor] <- NA  # Inapplicable test configuration
    }
  }

  # Sort the scores and select top K predictors
  top_K_predictors <- names(sort(scores, decreasing = TRUE, na.last = TRUE)[1:K])

  # Return a list containing the subset of data for the top K predictors and the response
  return(list(X = X[, top_K_predictors, drop = FALSE], y = y))
}


# Ensure the var_type function is defined properly but not exported if it's only used internally
#' Determine the Type of a Variable
#'
#' Internal helper function to determine the data type of a variable.
#' Used internally by various functions within the pacKage.
#'
#' @param x The variable to type checK.
#' @return A character string describing the type of x.
#' @keywords internal

var_type <- function(x) {
if (is.numeric(x)) {
  if (all(x == round(x))) {  # ChecK if all values are integers
    if (length(unique(x)) == 2) "binary"
    else "integer"
  } else "continuous"
} else if (is.factor(x) || is.character(x)) {
  "categorical"
} else {
  "other"
}
}
