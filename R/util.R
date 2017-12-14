#' Get the value of the objective function
#' @title  get_objective_for_population
#' @param individuals list with fields variables, indices, linear_model
#' @param objective name of the objective function inside quotes
#' @return The value of the objective function of the linear model
get_objective_for_population <- function(individuals, objective = "AIC"){
  obj <- lapply(individuals, 
                function(x) - eval(as.name(objective))(x$linear_model))
  return(obj)
}

## get the loss
#get_fitness_function(lin.model, fitness){
# return
#}

#' utility function to encode indices in a binary vector
#' @title one_hot
#' @param p a list like the result of random_selection_regression
#' @param n_var the total number of indices
#' @return a binary vector of length n_var with elements equal to 1 for the
#' indices in p$indices
one_hot <- function(p, n_var){
  one_hot_vector <- rep(0, n_var)
  idx <- p$indices
  one_hot_vector[idx] = 1
  return(one_hot_vector)
}
                
#' Used to get the probabilities of each individual in a list
#' of (variables, indices, linear_model) in the next generation
#' proportionally to the value of their objective
#' @title get_prob_individuals
#' @param individuals list of (variables, indices, linear_model) see the
#' results section of random_selection_regression for more details
#' @param objective objective function
#' @return a list of probabilities that correspond to the objective value.
get_prob_individuals <- function(individuals, objective = "AIC"){
  
  objectives <- unlist(lapply(individuals, 
                              function(x) - eval(as.name(objective))(x$linear_model)))
  probs <- objectives / sum(objectives)
  return(probs)
}
                              
mutation <- function(offspring, mu){
  prob_mutation <- runif(length(offspring),0,1)
  offspring[which(prob_mutation < mu)] <- abs(offspring[which(prob_mutation < mu)] - 1)
  return(offspring)
}
