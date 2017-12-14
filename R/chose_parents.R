####################### Choosing parents ######################

#' Used to chose 2 parents proportionally to their fitness with respect
#' to the objective function
#' @title chose_parents_prop
#' @param individuals list of parents of type (variables, indices, linear_model)
#' @param objective objective function that defines the criterion to evaluate the
#' fitness of each individual
#' @return the parent pairing - 2 parents samples with probabilities as defined in get_prob_individuals
## chose both parents proportionally to their fitness
chose_parents_prop <- function(individuals, objective = "AIC"){
  
  probs <- get_prob_individuals(individuals, objective = "AIC")
  parents <- sample(length(individuals), 2, prob = probs)
  return(parents)
}

## chose one parent proportionally to their fitness
## second at random
#' Used to chose one parent at random and the other proportionally to
#' their fitness with regard to their fitness function
#' @title chose_parents_prop_random
#' @param individuals list of (variables, indices, linear_model) 
#' see random_selection_regression
#' @param objective objective function
#' @return the parent pairing - 1 parent samples with probabilities and 1 parent is randomly selected.
chose_parents_prop_random <- function(individuals, objective = "AIC"){
  
  probs <- get_prob_individuals(individuals, objective = "AIC")
  parent1 <- sample(length(individuals), 1, prob = probs)
  parent2 <- sample(length(individuals), 1)
  return(c(parent1, parent2))
}

## chose both parents at random
#' @title chose_parents_random
#' @param individuals list of (variables, indices, linear_model) 
#' @return the parent pairing - 2 parents are randomly selected.
chose_parents_random <- function(individuals){
  return(sample(length(individuals), 2))
}

#'Utility function to split a sequence in k subsequences
#' that is used in the tournament selection
#' @title lit_sequence
#' @param shuffled 1:n shuffled
#' @param k number of k subsequences
#' @return a list of k subsequences
split_sequence <- function(shuffled, k){
  ## utility for tournament selection
  ## vies k disjoint subsequences from a sequence
  l_splits <- floor(length(shuffled)/k)
  disjoint_parts <- lapply(1:k, function (x) shuffled[((x-1)*l_splits+1):(x*l_splits)])
  if (length(shuffled) %% k > 0){
    disjoint_parts[[k]] <- c(disjoint_parts[[k]],
                             shuffled[(l_splits*k+1):length(shuffled)])
  }
  return(disjoint_parts)
}


#' A parents selection mechanism. The tournament selection splits the parents
#' population in k non overlapping subgroups and takes the fittest individual
#' in each group then takes n-k random subgroups (n the number of parents)
#' and takes the fittest individual in each of these random subgroups
#' @title tournament_selection
#' @param y response variable
#' @param dataset the dataframe containing the data
#' @param k the number of subsequences that should not overlap
#' @param n_var number of parents
#' @param objective the objective function
#' @return a list of n_var parents that are the fittest ones in each subgroup
## tournament selection
tournament_selection <- function(y, dataset, individuals, k = 4, 
                                 n_var, objective = "AIC"){
  
  
  
  ## shuffle the indices and split the resulting sequence
  shuffled_sample <- sample(n_var, n_var, replace = F)
  disjoint_parts <- split_sequence(shuffled_sample, k)
  
  ## get the fittest individuals among each group
  fit_ind <- lapply(1:k, 
                    function(x) get_fittest_ind(individuals[disjoint_parts[[x]]],
                                                objective = objective))
  pop_size = length(individuals)
  random_groups <- lapply(1:(pop_size-k), 
                          function(x) sample(1:pop_size, floor(pop_size/3)))
  
  fit_ind_rdm <- lapply(1:length(random_groups),
                        function(x) get_fittest_ind(individuals[random_groups[[x]]], 
                                                    objective = objective))
  fit_ind <- c(fit_ind, fit_ind_rdm)
  
  ## returns a list of parents for the next generation
  return(fit_ind)
  
}
