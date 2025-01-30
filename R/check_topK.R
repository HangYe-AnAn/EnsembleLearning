#' Check Top K Predictors
#'
#' This function checks the input of K value if it follow the rules and give different results due to conditions.
#'
#' @param X A matrix or data frame containing the predictors.
#' @param y A vector containing the response variable.
#' @param K A positive integer specifying the number of top informative predictors to select; must be greater than the number of predictors. (Optional)
#'
#' @return A list with the updated X matrix and y vector.
#'
#' @details This function checks if the input of K is NULL or not.
#' \cr If K is NULL, the function returns the original X and y without further processing.
#' \enumerate{ If K is not NULL, the function checks the following conditions before proceeding:
#' \item P > N: The number of predictors (P) must be less than the number of observations (N).
#' \item K is numeric: K must be a numeric value.
#' \item K is greater than zero: K must be greater than zero.
#' \item K does not exceed the number of predictors: K must not exceed the total number of predictors in X.
#' }
#' If all conditions are met, the function proceeds with model regression using the `returnTopK` function(For more information, see \code{\link{returnTopK}}.) to select the top K predictors.
#'
#' @examples
#' library(simpleEnsembleGroup7)
#'
#' # Example usage
#' X <- iris[, -5]  # predictors
#' y <- iris[, 5]   # response
#' result <- check_topK(X, y, 3)
#' print(result)
#'
#' @export
#'

check_topK <- function(X, y, K){
  p <- ncol(X)
  n <- nrow(X)
  # Only proceed with predictor selection if K or informative is not NULL

  if (!is.null(K)) {

    # Check for P > N
    if (p < n) {

      stop("Your P is smaller than N, not allowed to return top K predictors.")
    }

    # Ensure K is numeric
    if (!is.null(K) && !is.numeric(K)) {
      stop("K does not allow it to be a value other than numeric.")

    }
    if(K <= 0){
      stop("K has to be greater than zero.")
    }
    # Ensure K does not exceed the number of predictors
    if (!is.null(K) && K > p) {
      stop("Your request for top predictors exceeds the number of available predictors in the dataset.")

    }else{
      X2 <- returnTopK(X, y, K)$predictors
      return(list(X = X, y = y))
    }
  }else(is.null(K))
  {
    return(list(X = X, y = y))
  }

  }

