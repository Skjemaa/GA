#' select randomly the covariates to include in a regression and returns 
#' the names, indices of the covariates and an 'lm' object
#' @param y Name of the response variable in the dataset
#' @param names Names of the covariates in the dataset
#' @param n Number of covariates to include in the regression
#' @return A list with :
#' \item{variable}{the name of the covariates selected in the regression}
#' \item{indices}{their indices in the original vector names}
#' \item{linear_model}{the 'lm' object resulting from the regression}

random_selection_regression <- function(y, names, dataset, n){
  
  indices <- sample(1:length(names), n)
  lin_model <- regression(y, names, indices, dataset)
  return(list("variables" = names[indices], "indices" = indices, 
              "linear_model" = lin_model$linear_model))
}