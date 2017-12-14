#' Used to get the element in a list of elements like the result of
#' random_selection_regression
#' @param individuals list with fields variables, indices, linear_model
#' @param objective name of the objective function to be maximized (as a string, inside quotes)
#' @param fitness only works with AIC, using a fitness function derived from the AIC
#' @return returns the element of individuals with that maximizes the objective function

get_fittest_ind <- function(individuals, objective = "AIC", fitness = F){
  
  if(fitness){
    ## case where the objective function is not the fitness function
    # @TODO
    return(0)
  } 
  else{
    idx <- which.max(unlist(lapply(individuals, 
                                   function(x) eval(as.name(objective))(x$linear_model))))
    return(individuals[[idx]])
  }
}