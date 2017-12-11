setClass("GA",
         representation())

## perform a linear regression given y, a list of covariates and the
## indices of the covariates to take into account
regression <- function(y,names, indices, dataset){
  form <- noquote(paste(names[indices], collapse = "+"))
  print(form)
  form <- paste(y, form, sep = " ~ ")
  print(noquote(form))
  lin.reg <- lm(noquote(form), data = dataset)
  return(lin.reg)
}

## select randomly the covariates to include in the regression returns 
##the names, indices of the covariates and the result of lm
random_selection_regression <- function(y, names, dataset, n){
  indices <- sample(length(names), n)
  lin_model <- regression(y, names, indices, dataset)
  return(list("variables" = names[indices], "indices" = indices, 
              "linear_model" = lin_model))
}


## get the value of the objective function
get_objective_for_population <- function(lin.model, objective = "AIC"){
  return(as.name(objective)(lin.model))
}

@TODO
## get the loss
get_fitness_function(lin.model, fitness){
  return
}

## utility function one_hot to encode indices in a binary vector
one_hot <- function(p, n_var){
  one_hot_vector <- rep(0, n_var)
  idx <- p$indices
  one_hot_vector[idx] = 1
  return(one_hot_vector)
}




get_fittest_ind <- function(individuals, objective = "AIC", fitness = F){
  if(fitness){
    ## case where the objective function is not the fitness function
    # @TODO
    return(0)
  } 
  else{
    idx <- which.max(unlist(lapply(individuals, 
                                   function(x) as.name(objective)(x$linear_model))))
    return(individuals[[idx]])
  }
}

get_k_fittest_ind <- function(individuals, objective = "AIC", k=1,
                              fitness = F){
  if(fitness){
    ## case where the objective function is not the fitness function
    # @TODO
    return(0)
  } 
  else{
    idx <- order(unlist(lapply(individuals, 
                               function(x) as.name(objective)(x$linear_model))),
                 decreasing = T)
    return(individuals[idx[1:k]])
  }
}

## probabilities of individuals in the next generation
get_prob_individuals <- function(individuals, objective = "AIC"){
  objectives <- unlist(lapply(individuals, 
                              function(x) as.name(objective)(x$linear_model)))
  probs <- objectives / sum(objectives)
  return(probs)
}




####################### Choosing parents ######################


## chose both parents proportionally to their fitness
chose_parents_prop <- function(individuals, objective = "AIC"){
  probs <- get_prob_individuals(individuals, objective = "AIC")
  parents <- sample(length(individuals), 2, prob = probs)
  return(parents)
}



## chose one parent proportionally to their fitness
## second at random
chose_parents_prop_random <- function(individuals, objective = "AIC"){
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
split_sequence <- function(shuffled, k, n){
  n_splits <- floor(n/k)
  disjoint_parts <- sapply(1:n_splits, shuffled[((x-1)*7+1):(x*7)])
  if (n %% k > 0){
    disjoint_parts[[n_splits]] <- c(disjoint_parts[[n_splits]],
                                    shuffled[(n_splits*k+1):n])
  }
  return(disjoint_parts)
}



## tournament selection
tournament_selection <- function(y, dataset, individuals, k = 4, 
                                 n_var, objective = "AIC"){
  
  ## shuffle the indices and split the resulting sequence
  shuffled_sample <- sample(n_var, n_var, replace = F)
  disjoint_parts <- split_sequence(shuffled_sample, k, n_var)
  
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


####################### Gene operators ########################


## get indices of the variables taken into account in the children
## with crossover
get_indices_crossover_gm <- function(points, p1, p2, n_var){
  crossing_points <- c(1, points)
  ending_points <- c(points, length(n_var))
  
  from_p1_idx <- unlist(lapply(seq(1, length(crossing_points)-1, by = 2), 
                               function(i) crossing_points[i]:crossing_points[i+1]))
  from_p2_idx <- seq(1, n_var)[-child1_idx]
  
  one_hot_p1 <- one_hot(p1, n_var)
  one_hot_p2 <- one_hot(p2, n_var)
  
  child1_var <- rep(0, n_var)
  child2_var <- rep(0, n_var)
  
  cild1_var[from_p1_idx] <- one_hot_p1[from_p1_idx]
  cild1_var[from_p2_idx] <- one_hot_p2[from_p2_idx]
  
  cild2_var[from_p1_idx] <- one_hot_p2[from_p1_idx]
  cild2_var[from_p2_idx] <- one_hot_p1[from_p2_idx]
  
  child1_idx <- which(child1_var==1)
  child2_idx <- which(chidl2_idx)
  
  return(list("child_1" = child1_idx, "child2" = child2_idx))
}



## permutation gap : no need to implement a function for that, just include
## that case in the function iterate_generations

random_chromosomes_swap <- function(p1, p2, k, n_var){
  ## sample k chromosomes and swap these chromosomes among the 2 parents
  one_hot_p1 <- one_hot(p1, n_var)
  one_hot_p2 <- one_hot(p2, n_var)
  
  child1_var <- one_hot_p1
  child2_var <- one_hot_p2
  
  # sample the locis that will be swapped
  swapping_idx <- sample(n_var, k, replace = F)
  swapped_from_p1 <- one_hot_p1[swapping_idx]
  
  child1_var[swapping_idx] <- one_hot_p2[swapping_idx]
  child2_var[swapping_idx] <- swapped_from_p1
  
  child1_idx <- which(child1_var==1)
  child2_idx <- which(chidl2_idx)
  
  return(list("child_1" = child1_idx, "child_2" = child2_idx))
  
}

################ Options for the 1st iteration ################

get_most_significant_variables <- function(dataset, y, p_val = 0.05){
  removed_var <- c()
  iterate = T
  variables <- names(dataset)[names(dataset)!=y]
  while(iterate == T){
    formula <- cat(variables, sep="+")
    summary_lm <- summary(lm(y~formula))
    
    if (max(summary_lm)$coefficients[,4] > p){
      removed <- names(which.max(summary_lm)$coefficients[,4])
      removed_var <- c(removed_var, removed)
      variables <- variables[variables != removed]
    }
    
    if (max(summary_lm)$coefficients[,4] < p){
      iterate = F
    }
  }
  return(cat(variables, sep="+"))
}

get_largest_interactions <- function(dataset, y, nb.var, max = 0){
  var_dataset <- dataset[-which(names(dataset)==y)]
  correlation_matrix <- cor(var_dataset)
  
  
}

######################### Iterations ##########################

## do the first iteration (at random, other initialization will be added)
first_generation <- function(y, dataset, population_size, interaction = F,
                             objective_function = "AIC", most_sig = F,
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


iterate_generations <- function(y, dataset, individuals, objective = "AIC", 
                                pop_size, permutation_gap = 1,
                                selection = "prop", nb_groups = 4,
                                gene_selection = "crossover",
                                nb_pts = 1){
  n_var <- ncol(dataset)-1
  pop_size_last <- length(individuals)
  
  k_prev_gen <- get_k_fittest_ind(individuals, objective = objective,
                                  k = permutation_gap)
  
  ## parents selection
  
  if(selection=="tournament"){
    parents <- tournament_selection(y, dataset, individuals, 
                                    k = nb_groups, n_var,
                                    objective = objective)
  }
  
  if(selection=="prop"){
    parents_idx <- lapply(1:(pop_size_last/2),
                          function(x) chose_parents_prop(individuals,
                                                         objective))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  if(selection=="prop_random"){
    parents_idx <- lapply(1:(pop_size_last/2),
                          function(x) chose_parents_prop_random (individuals,
                                                                 objective))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  if(selection=="random"){
    parents_idx <- lapply(1:(pop_size_last/2),
                          function(x) chose_parents_random (individuals))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  ## gene selection
  
  if(gene_selection=="crossover"){
    chld_idx <- lapply(range(pop_size_last), function(x) 
      get_indices_crossover_gm(sample(1:n_var, k), parents[[(2*x-1)]],
                               parents[[(2*x)]],n_var))
    
    chld_1 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      chld_idx[[x]]$child_1))
    chld_2 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      chld_idx[[x]]$child_2))
    children <- c(chld_1, chld_2)
  }
  
  if(gene_selection=="random"){
    chld_idx <- lapply(range(pop_size_last), function(x) 
      random_chromosomes_swap(sample(1:n_var, k), parents[[(2*x-1)]],
                              parents[[(2*x)]],n_var))
    
    chld_1 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      chld_idx[[x]]$child_1))
    chld_2 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      chld_idx[[x]]$child_2))
    children <- c(chld_1, chld_2)
  }
  
  ## here we only keep the (new gener pop size - nb_parents_kept) children 
  ## with the best fitness
  nb_child_kept = pop_size - permutation_gap
  child_kept <- get_k_fittest_ind(children, objective, k = nb_child_kept)
  
  return(c(k_prev_gen, child_kept))
  
}



#################### select function ##########################

## for now it does not deal with the case n_iter %% pop_size != 0
## nor pop_size %% 2 != 0
## I will add these later

select <- function(y, dataset, n_iter = 200, pop_size = 20, objective = "AIC",
                   interaction = F, most_sig = F, normalization = F, 
                   selection = "prop", nb_groups = 4, permutation = 1,
                   gene_selection = "crossover", nb_pts = 1, decay = "unif"){
  
  n <- ncol(dataset)-1
  
  names <- names(dataset)[names(dataset)!=y]
  
  pop_sizes <- from(pop_size, 2, by = -2)
  
  first_ind <- first_generation(y, dataset, pop_size, interaction, 
                                objective_function = objective, most_sig, 
                                normalization)
  
  ind <- first_ind
  ## when to change the population size
  chge_pop <- n_iter/pop_size
  
  ## generations for uniform decay of population size
  for (i in 1:n_iter){
    ind <- iterate_generations(y, dataset, ind, objective,
                               pop_size[ceiling(i/chge_pop)],
                               permutation, selection, nb_groups,
                               gene_selection, nb_pts)
  }
  
  objectives <- lapply(ind, 
                       function(x) get_objective_for_population(x$linear_model,
                                                                objective))
  objectives <- unlist(objectives)
  
  return(ind[which.max(objectives)])
  
}




