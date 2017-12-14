## chose both parents proportionally to their fitness
chose_parents_prop <- function(individuals, objective = "AIC"){
  #' Used to chose 2 parents proportionally to their fitness with respect
  #' to the objective function
  #' @param individuals list of parents of type (variables, indices, linear_model)
  #' @param objective objective function that defines the criterion to evaluate the
  #' fitness of each individual
  #' @return 2 parents samples with probabilities as defined in get_prob_individuals
  probs <- get_prob_individuals(individuals, objective = "AIC")
  parents <- sample(length(individuals), 2, prob = probs)
  return(parents)
}



## chose one parent proportionally to their fitness
## second at random
chose_parents_prop_random <- function(individuals, objective = "AIC"){
  #' Used to chose one parent at random and the other proportionally to
  #' their fitness with regard to their fitness function
  #' @param individuals list of (variables, indices, linear_model) 
  #' see random_selection_regression
  #' @param objective objective function
  probs <- get_prob_individuals(individuals, objective = "AIC")
  parent1 <- sample(length(individuals), 1, prob = probs)
  parent2 <- sample(length(individuals), 1)
  return(c(parent1, parent2))
}

## chose both parents at random
chose_parents_random <- function(individuals){
  return(sample(length(individuals), 2))
}



## utility for tournament selection
## vies k disjoint subsequences from a sequence
split_sequence <- function(shuffled, k){
  #'Utility function to split a sequence in k subsequences
  #' that is used in the tournament selection
  #' @param shuffled 1:n shuffled
  #' @param k number of k subsequences
  #' @return a list of k subsequences
  n_splits <- floor(length(shuffled)/k)
  disjoint_parts <- sapply(1:n_splits, shuffled[((x-1)*7+1):(x*7)])
  if (n %% k > 0){
    disjoint_parts[[n_splits]] <- c(disjoint_parts[[n_splits]],
                                    shuffled[(n_splits*k+1):n])
  }
  return(disjoint_parts)
}