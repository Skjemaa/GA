#' Used to get the k elements from a list of (variables, indices, linear_model)
#' with the higgest objectives values
#' @param individuals list with fields variables, indices, linear_model
#' @param objective name of the objective function to be maximized (as a string, inside quotes)
#' @param fitness only works with AIC, using a fitness function derived from the AIC
#' @return returns the k elements of individuals with that have the highest objectives

get_k_fittest_ind <- function(individuals, objective = "AIC", k=1,
                              fitness = F){
  
  if(fitness){
    ## case where the objective function is not the fitness function
    # @TODO
    return(0)
  } 
  else{
    idx <- order(unlist(lapply(individuals, 
                               function(x) eval(as.name(objective))(x$linear_model))),
                 decreasing = T)
    return(individuals[idx[1:k]])
  }
}