## tournament selection

#' A parents selection mechanism. The tournament selection splits the parents
#' population in k non overlapping subgroups and takes the fittest individual
#' in each group then takes n-k random subgroups (n the number of parents)
#' and takes the fittest individual in each of these random subgroups
#' @param y response variable
#' @param dataset the dataframe containing the data
#' @param k the number of subsequences that should not overlap
#' @param n_var number of parents
#' @param objective the objective function
#' @return a list of n_var parents that are the fittest ones in each subgroup

tournament_selection <- function(y, dataset, individuals, k = 4, 
                                 n_var, objective = "AIC"){
  
  ## shuffle the indices and split the resulting sequence
  shuffled_sample <- sample(n_var, n_var, replace = F)
  disjoint_parts <- split_sequence(shuffled_sample, k)
  
  ## get the fittest individuals among each group
  fit_ind <- sapply(1:k, 
                    function(x) get_fittest_ind(individuals[disjoint_parts[[x]]],
                                                objective = objective))
  
  pop_size = length(individuals)
  random_groups <- sapply(1:(pop_size-k), 
                          function(x) sample(1:pop_size, floor(pop_size/3)))
  
  fit_ind_rdm <- sapply(1:length(random_groups),
                        function(x) get_fittest_ind(individuals[random_groups[[x]]], 
                                                    objective = objective))
  
  fit_ind <- c(fit_ind, fit_ind_rdm)
  
  ## returns a list of parents for the next generation
  return(fit_ind)
  
}