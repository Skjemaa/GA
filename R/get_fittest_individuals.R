#' Used to get the element in a list of elements like the result of
#' random_selection_regression
#' @title get_fittest_ind
#' @param individuals list with fields variables, indices, linear_model
#' @param objective name of the objective function to be maximized (as a string, inside quotes)
#' @param fitness only works with AIC, using a fitness function derived from the AIC
#' @return returns the element of individuals with that maximizes the objective function
get_fittest_ind <- function(individuals, objective = "AIC"){
  idx <- which.max(unlist(lapply(individuals, 
                                   function(x) - eval(as.name(objective))(x$linear_model))))
  return(individuals[[idx]])
}

#' Used to get the k elements from a list of (variables, indices, linear_model)
#' with the higgest objectives values
#' @title get_k_fittest_ind
#' @param individuals list with fields variables, indices, linear_model
#' @param objective name of the objective function to be maximized (as a string, inside quotes)
#' @param fitness only works with AIC, using a fitness function derived from the AIC
#' @param k number of variables we want to choose.
#' @return returns the k elements of individuals with that have the highest objectives

get_k_fittest_ind <- function(individuals, objective = "AIC", k){
  idx <- order(unlist(lapply(individuals, 
                               function(x) - eval(as.name(objective))(x$linear_model))),
                 decreasing = T)
  return(individuals[idx[1:k]])
}
