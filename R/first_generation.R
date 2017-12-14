## do the first iteration (at random, other initialization will be added)

#' Performs the first iteration of the Genetic algorithm
#' Generates the first individuals
#' @param y the response variable in the dataframe 'dataset'
#' @param dataset the data frame containing the variables in the model
#' @param population_size the number of individuals to generate
#' @param interaction a boolean indicating if we want to take into account
#' terms coming from interactions, False by default
#' @param most_sig a boolean indicating if we want to only consider the most
#' significant covariates
#' @param normalization a boolean indicating if we want to normalize the
#' covariates
#' @return a list of individuals each a list with fields:
#' \item{variable}{the covariates kept in the linear model}
#' \item{indices}{the indices of these covariates in the data frame}
#' \item{linear_model}{the linear model with these covariates}
first_generation <- function(y, dataset, population_size, interaction = F,
                             most_sig = F, objective_function = "AIC",
                             normalization = F){

  n <- ncol(dataset)-1
  names <- names(dataset)[names(dataset)!=y]
  if(interaction == T){
    ## add the interaction terms
  }
  if(most_sig == T){
    ## add the use of get_most_significant_var
  }
  
  n_var <- sample(1:n, population_size, replace = T)
  individuals <- lapply(n_var, function(x) random_selection_regression(y, names, dataset, x))
  return(individuals)
}