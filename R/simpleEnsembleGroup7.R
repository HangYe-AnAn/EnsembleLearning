#' AMS 597 Final Project - Simple Ensemble Group 7
#' @description
#' The function simpleEnsembleGroup7 implements various regression models and it is able to apply different tasks, including bagging, pre-screen for top informative predictors, and ensemble learning.
#'
#'
#' @param X A predictor matrix, which may contain categorical, continuous, or discrete data.
#' @param y The response variable vector, which can be either binary or continuous.
#' @param method A character string specifying the type of model to fit.
#'               Valid options are "linear", "ridge", "lasso", "elastic", and "svm".
#'               \cr
#'               This parameter must be a single model type when bagging is TRUE. Ex. method = "linear"
#'               \cr
#'               This parameter can be multiple model types when ensemble is TRUE. A list of character strings specifying the model types to be included in ensemble learning. Ex. method = c("linear", "lasso")
#' @param bagging A logical value indicating whether bagging should be used.
#'                Bagging is only applicable to a single model type at a time. Ex. method = "linear"
#'                If bagging is TRUE, ensemble must be FALSE.
#'                Default is FALSE.
#' @param ensemble A logical value indicating whether ensemble should be used.
#'                Ensemble is applicable to multiple model types at a time. Ex. method = c("linear", "lasso")
#'                If ensemble is TRUE, bagging must be FALSE.
#'                Default is FALSE.
#' @param alpha The alpha parameter(from 0 to 1 inclusive) for elastic net models, influencing the mix of ridge and lasso regularization. Default is 0.5.
#' @param n_bag The number of bagging iterations to perform. Default is 100.
#' @param K An optional positive integer specifying the number of top predictors to use.
#'          If NULL, all predictors in X are used to train the model specified.
#'          Default is FALSE.
#' @details
#' Here we will brifly introduce the basic \bold{workflow} of out simEnsemnbleGroup7 function: \cr \cr
#' 1.  Check and handle missing values(For more information see \code{\link[simpleEnsembleGroup7]{handle_missing_values}}) and, if response is binary and not numeric, it converts categorical variables of the response (y) to numeric binary(0 or 1).
#' \cr
#' \cr
#'  2. It then checks the validity of the parameters, including the number of top predictors (K) (For more information see \code{\link[simpleEnsembleGroup7]{check_topK}}), if the selected models are allowed (we only allow "linear", "ridge", "lasso", "elastic", and "svm"), number of bag samples (n_bag), alpha values (applies only if you are using elastic net method), and whether ensemble or bagging is TRUE or FALSE.
#' If the parameters is correctly entered, the function will proceeds to the next stage of the modeling process.
#' \cr
#' \cr
#' \enumerate{ 3. Based on these checks, the function fits a model using the specified method, either with or without bagging or ensemble learning and if pre-screening for top informative predictors is applied.
#' \item If bagging is TRUE, function will compute bagging and return the appropriate model fitting function based on the method chosen. (For more information see \code{\link[simpleEnsembleGroup7]{bagging_linear_model}}, \code{\link[simpleEnsembleGroup7]{bagging_ridge_model}}, \code{\link[simpleEnsembleGroup7]{bagging_lasso_model}}, \code{\link[simpleEnsembleGroup7]{bagging_elastic_model}})
#' \enumerate{
#' \item If bagging = FALSE, ensemble = FALSE, K = NULL, function will return the basic fitting model based on the single method that the user specify.
#' \item If bagging = TRUE, ensemble = FALSE, K = NULL, function will apply bagging method and will return the final model that averages the total bagged models based on the single method that the user specify.
#' \item If bagging = TRUE, ensemble = FALSE, K = 5, function will apply bagging method and will find the top 5 informative predictors for each bag sample, and will return the final model that averages the total bagged models based on the single method that the user specify.
#' }
#'
#'
#' \item Bagging and ensemble cannot be both TRUE.
#' \item If ensemble is TRUE, function will compute ensemble and return the appropriate model. (For more information see \code{\link[simpleEnsembleGroup7]{EnsembleLearning}}).
#'}
#'
#' If the number of predictors(p) is larger compared to the number of observations(n), a warning is issued for linear regression and function will stop.
#' \cr
#' \cr
#' 4. Finally, the function returns the fitted model based on the selected options.
#'
#' @return Depending on the settings, it returns a fitted model or models,
#'         potentially along with model performance metrics if specified.
#'
#' @examples
#' # Load required libraries
#' library(glmnet)
#' # Generate sample data
#' n <- 500  # Number of observations
#' n_predictors <- 50  # Number of predictors
#' # Create predictor variables
#' predictors <- matrix(rnorm(n * n_predictors), ncol = n_predictors)
#'
#' # Create names for the predictors
#' predictor_names <- paste0("Predictor", 1:50)
#' # Assign names to the columns of the predictors matrix
#' colnames(predictors) <- predictor_names
#'
#' # Generate response variable
#' response <- rnorm(n)
#' #bagging with no top K and elastic method
#' simpleEnsembleGroup7(predictors, response, method = "elastic", bagging = T)
#'
#' @examples
#' # example code
#'
#' # Load required libraries
#' library(glmnet)
#' # Generate sample data
#' n <- 100  # Number of observations
#' n_predictors <- 105  # Number of predictors
#' # Create predictor variables
#' predictors <- matrix(rnorm(n * n_predictors), ncol = n_predictors)
#'
#' # Create names for the predictors
#' predictor_names <- paste0("Predictor", 1:105)
#' # Assign names to the columns of the predictors matrix
#' colnames(predictors) <- predictor_names
#'
#' # Generate response variable
#' response <- rnorm(n)
#' #bagging with top K and elastic method
#' simpleEnsembleGroup7(predictors, response, method = "elastic", bagging = T, K = 3)
#'#' @examples
#' # example code
#'
#' # Load required libraries
#' library(glmnet)
#' # Generate sample data
#' n <- 100  # Number of observations
#' n_predictors <- 105  # Number of predictors
#' # Create predictor variables
#' predictors <- matrix(rnorm(n * n_predictors), ncol = n_predictors)
#'
#' # Create names for the predictors
#' predictor_names <- paste0("Predictor", 1:105)
#' # Assign names to the columns of the predictors matrix
#' colnames(predictors) <- predictor_names
#'
#' # Generate response variable
#' response <- rnorm(n)
#' #ensemble with elastic and lasso
#' simpleEnsembleGroup7(predictors, response, method = c("elastic", "lasso"), bagging = F, ensemble = T, K = 3)
#'
#' @author Hang Ye, Minghao Zhang, Lily Yuan, Jiang Yi
#' @export

simpleEnsembleGroup7 <- function(X, y, method = NULL, K=NULL, bagging = FALSE, ensemble = FALSE, alpha = 0.5, n_bag = 100) {

  #===Step 0:Unexpected Parameter Handle 2.0===
  if(is.null(X) || is.null(y) || is.null(method))
  {
    stop("One of these is NULL:X,y,method")
  }

  if( (!is.numeric(K) & !is.null(K)) || !is.numeric(alpha) || !is.numeric(n_bag))
  {
    stop("One of these is NOT numeric :K / alpha  / n_bag ")
  }

  if(!is.null(K)){
    if( K<=0  || alpha<0  || n_bag<=0 )
    {
      stop("One of these is smaller or equal than ZERO :K / alpha / n_bag ")
    }
  } else if(is.null(K)){
    if( alpha<0 || n_bag<=0 )
    {
      stop("One of these is smaller or equal than ZERO :K / alpha / n_bag ")
    }
  }

  #===Step 1: Handle missing value===#
  result <- handle_missing_values(X, y)
  X <- result$X
  y <- result$y


  # 1.1 When user forget to map the categorical data into a numeric
  for (i in seq_along(X)) {
    # Check if the column is a factor (categorical)
    if (is.factor(X[[i]]) || is.character(X[[i]])) {
      # Convert the column to a factor and then to numeric
      X[[i]] <- as.numeric(factor(X[[i]]))
    }
  }
  # Check and convert y if it is categorical and map it into numeric value
  if (is.factor(y) || is.character(y)) {
    y <- as.numeric(factor(y))
  }

  #===Step 2: Unexpected Parameter Handle===#

  # 2.1 Does User's K being reasonable ?
  X<-check_topK(X,y,K)$X
  y<-check_topK(X,y,K)$y

  # 2.2 K is not provided; We will fit the model with all predictors
  if (is.null(K)) {
    K <- ncol(X)
  }

  # 2.3 Ensemble and bagging cannot both be true.
  if (ensemble & bagging) {
    stop("Error: Both ensemble and bagging cannot be set to TRUE simultaneously.")
  }

  # 2.4 Allowed models
  allowed_models <- c("linear", "ridge", "lasso", "elastic", "svm")
  if (!all(method %in% allowed_models)) {
    stop("Error: We only support 'linear', 'ridge', 'lasso', 'elastic', and 'svm'.")
  }

  # 2.5 Bagging while method is a list and bagging is TRUE
  if ( (length(method) > 1) & bagging == TRUE) {
    stop("Error: Only one model type can be trained for the bagging process.")
  }

  #===Step 3: ALL parameters are reasonble, off you go!===#

  # Determine if K is provided
  if (!is.null(K)) {

    # K is provided; use it in the appropriate model based on the method and the bagging flag
    if (bagging) {
      if (method == "elastic") {
        model <- bagging_elastic_model(X, y, n_bags = n_bag, alpha = alpha, K = K)
      } else if (method == "lasso") {
        model <- bagging_lasso_model(X, y, n_bags = n_bag, K = K)
      } else if (method == "linear") {

        # 3.1 P >> n, we are not support linear regression when P >> n
        if (ncol(X) > nrow(X)) {
          warning("We are not support linear regression when P >> n,please pick a smaller K or use another dataset")
        }
        model <- bagging_linear_model(X, y, n_bags = n_bag, K = K)
      } else if (method == "ridge") {
        model <- bagging_ridge_model(X, y, n_bags = n_bag, K = K)
      }
    } else if (ensemble) {
      if (!all(method %in% allowed_models)) {
        stop("Error: Ensemble can only include 'linear', 'ridge', 'lasso', 'elastic', and 'svm'.")
      }
      X <- returnTopK(X,y,K)$X
      model <- EnsembleLearning(X, y, model_types = method, return_accuracy = FALSE)
    }
  }
  if(!ensemble & !bagging){
    if ( length(method) > 1) {
      stop("Error: Only one model type can be trained")
    }

    #ensemble and bagging are both FALSE
    if (method == "elastic") {
      X <- returnTopK(X,y,K)$X
      model <- elastic_model(X, y, alpha = alpha)
    } else if (method == "lasso") {
      X <- returnTopK(X,y,K)$X
      model <- lasso_model(X, y)
    } else if (method == "linear") {
      X <- returnTopK(X,y,K)$X
      model <- linear_model(X,y)
    } else if (method == "ridge") {
      X <- returnTopK(X,y,K)$X
      model <- ridge_model(X,y)
    }else if (method == "svm") {
      cat("Do you want to return the accuracy from the ensemble model? [yes/no]: ")
      response <- tolower(readline())
      if (response == "yes") return_accuracy <- TRUE
      X <- returnTopK(X,y,K)$X
      model <- svm(X,y)
    }
  }
  return(model)
}

install_and_load <- function(package_name) {
  options(repos = "https://cloud.r-project.org/")
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, repos = NULL, type = "source")
  }
  library(package_name, character.only = TRUE)
}


