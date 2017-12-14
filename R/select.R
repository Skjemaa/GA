#################### select function ##########################

## for now it does not deal with the case n_iter %% pop_size != 0
## nor pop_size %% 2 != 0
## I will add these later

#' Selects covariates for regression using a genetic algorithm
#'
#' @param dataset The dataset in matrix form with last column being the dependent variable.
#' @param regression The regression form to use (either lm or glm).
#' @param criterion The objective criterion to use (default AIC).
#' @param parent_selection The selection method for choosing parents in GA.
#' @param gene_selection The selection method for choosing genes in GA.
#' @param num_iter The maximum number of iterations for the algorithm
#' @return List containing the fitted regression, the optimal objective value,
#' and number of iterations used.
#' @examples
#'

select <- function(y, dataset, n_iter = 200, pop_size = 20, objective = "AIC",
                   interaction = F, most_sig = F, normalization = F, 
                   selection = "prop", nb_groups = 4, permutation = 1,
                   gene_selection = "crossover", nb_pts = 1, decay = "unif"){
  
  
  n <- ncol(dataset)-1
  
  first_ind <- first_generation(y, dataset, pop_size, interaction, 
                                objective_function = objective, most_sig, 
                                normalization)
  
  ind <- first_ind
  
  ## when to change the population size
  pop_sizes <- seq(pop_size, 2, by = -2)
  chge_pop <- n_iter/length(pop_sizes)
  
  ##changing the permutation gap
  permutation_gap <- rep(permutation, length(pop_sizes))
  for (i in 1:length(pop_sizes)){
    if(permutation_gap[i] > pop_sizes[i]/2)
      permutation_gap[i] <- floor(pop_sizes[i]/4)
  }
  
  ## generations for uniform decay of population size
  for (i in 1:n_iter){
    
    ind <- iterate_generations(y, dataset, ind, objective,
                               pop_sizes[ceiling(i/chge_pop)],
                               permutation_gap[ceiling(i/chge_pop)], 
                               selection, nb_groups,
                               gene_selection, nb_pts)
  }
  objectives <- get_objective_for_population(ind, objective)
  objectives <- unlist(objectives)
  
  return(ind[which.max(objectives)])
  
}