#' Performs a linear regression given y, a list of covariates and the
#' indices of the covariates to take into account.
#' @param y Name of the response variable in the dataset
#' @param names Names of the covariates in the dataset
#' @param indices Indices of the covariates considered in the regression
#' @param dataset Name of the dataset
#' @return 'lm' object obtained by performing the regression with the
#' selected covariates
#' @examples 
regression <- function(y,names, indices, dataset, reg_method){
  if(length(indices)==0){
    indices <- sample(1:length(names), floor(length(names)/2))
  }
  form <- noquote(paste(names[indices], collapse = "+"))
  form <- paste(y, form, sep = " ~ ")
  types <- sapply(dataset, class)
  if(is.null(reg_method)) {
    nb_double <- sum(types == "numeric") + sum(types == "integer")
    if(nb_double == ncol(dataset)){
      lin.reg <- lm(noquote(form), data = dataset)
    } else {
      lin.reg <- glm(noquote(form), data = dataset)
    }
  } else if (reg_method == "lm") {
    lin.reg <- lm(noquote(form), data = dataset)
  } else {
    lin.reg <- glm(noquote(form), data = dataset)
  }
  return(list("variables" = names[indices], "indices" = indices, 
              "linear_model" = lin.reg))
}


#' select randomly the covariates to include in a regression and returns 
#' the names, indices of the covariates and an 'lm' object
#' @param y Name of the response variable in the dataset
#' @param names Names of the covariates in the dataset
#' @param n Number of covariates to include in the regression
#' @return A list with :
#' \item{variable}{the name of the covariates selected in the regression}
#' \item{indices}{their indices in the original vector names}
#' \item{linear_model}{the 'lm' object resulting from the regression}
random_selection_regression <- function(y, names, dataset, n, reg_method){
  indices <- sample(1:length(names), n)
  lin_model <- regression(y, names, indices, dataset, reg_method)
  return(list("variables" = names[indices], "indices" = indices, 
              "linear_model" = lin_model$linear_model))
}

#' Get the value of the objective function
#' @param individuals the population
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
                                   function(x) - eval(as.name(objective))(x$linear_model))))
    return(individuals[[idx]])
  }
}

#' Used to get the k elements from a list of (variables, indices, linear_model)
#' with the higgest objectives values
#' @param individuals list with fields variables, indices, linear_model
#' @param objective name of the objective function to be maximized (as a string, inside quotes)
#' @param fitness only works with AIC, using a fitness function derived from the AIC
#' @return returns the k elements of individuals with that have the highest objectives

get_k_fittest_ind <- function(individuals, objective = "AIC", fitness = F, k){
  if(fitness){
    ## case where the objective function is not the fitness function
    # @TODO
    return(0)
  } 
  else{
    idx <- order(unlist(lapply(individuals, 
                               function(x) - eval(as.name(objective))(x$linear_model))),
                 decreasing = T)
    return(individuals[idx[1:k]])
  }
}


#' Used to get the probabilities of each individual in a list
#' of (variables, indices, linear_model) in the next generation
#' proportionally to the value of their objective
#' @param individuals list of (variables, indices, linear_model) see the
#' results section of random_selection_regression for more details
#' @param objective objective function
#' @return a list of probabilities
get_prob_individuals <- function(individuals, objective = "AIC"){
  
  objectives <- unlist(lapply(individuals, 
                              function(x) - eval(as.name(objective))(x$linear_model)))
  probs <- objectives / sum(objectives)
  return(probs)
}




####################### Choosing parents ######################

#' Used to chose 2 parents proportionally to their fitness with respect
#' to the objective function
#' @param individuals list of parents of type (variables, indices, linear_model)
#' @param objective objective function that defines the criterion to evaluate the
#' fitness of each individual
#' @return 2 parents samples with probabilities as defined in get_prob_individuals

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
#' @param individuals list of (variables, indices, linear_model) 
#' see random_selection_regression
#' @param objective objective function
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

#'Utility function to split a sequence in k subsequences
#' that is used in the tournament selection
#' @param shuffled 1:n shuffled
#' @param k number of k subsequences
#' @return a list of k subsequences
lit_sequence <- function(shuffled, k){
## utility for tournament selection
## vies k disjoint subsequences from a sequence
sp
  n_splits <- floor(length(shuffled)/k)
  disjoint_parts <- sapply(1:n_splits, shuffled[((x-1)*7+1):(x*7)])
  if (n %% k > 0){
    disjoint_parts[[n_splits]] <- c(disjoint_parts[[n_splits]],
                                    shuffled[(n_splits*k+1):n])
  }
  return(disjoint_parts)
}


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
## tournament selection
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


####################### Gene operators ########################


#' Gene operator. Performs the k points crossover from 2 parents over
#' n_var genes at the indices given in the vector 'points'
#' @param points crossover points
#' @param p1,p2 parents as lists with fields variables, indices, linear_model
#' @param n_var, the total number of covariates in the data ncol(dataset)-1
#' @return a list containing the vectors of indices taken into account into
#' each of the 2 children given by the crossover operator
## get indices of the variables taken into account in the children
## with crossover
get_indices_crossover_gm <- function(points, p1, p2, n_var, mu){
  
  crossing_points <- c(1, points)
  ending_points <- c(points, n_var)
  
  from_p1_idx <- unlist(lapply(seq(1, length(crossing_points), by = 2), 
                               function(i) crossing_points[i]:ending_points[i]))
  from_p2_idx <- seq(1, n_var)[-from_p1_idx]
  
  one_hot_p1 <- one_hot(p1, n_var)
  one_hot_p2 <- one_hot(p2, n_var)
  
  child1_var <- rep(0, n_var)
  child2_var <- rep(0, n_var)
  
  child1_var[from_p1_idx] <- one_hot_p1[from_p1_idx]
  child1_var[from_p2_idx] <- one_hot_p2[from_p2_idx]
  
  child2_var[from_p1_idx] <- one_hot_p2[from_p1_idx]
  child2_var[from_p2_idx] <- one_hot_p1[from_p2_idx]
  
  child1_var <- mutation(child1_var, mu)
  child2_var <- mutation(child1_var, mu)
  
  child1_idx <- which(child1_var == 1)
  child2_idx <- which(child2_var == 1)
  
  return(list("child_1" = child1_idx, "child2" = child2_idx))
}



## permutation gap : no need to implement a function for that, just include
## that case in the function iterate_generations
## sample k locis and swap the alleles at these locis among the 2 parents
#' Function that executes alleles swap. It samples k indices in
#' range(number of genes) and swaps the alleles of the 2 parents
#' at these indices
#' @param p1,p2 the 2 parents: lists with fields (variables, indices, linear_model)
#' @param k number of locis to be swapped
#' @param n_var length of the chromosome
random_chromosomes_swap <- function(p1, p2, k, n_var, mu){
  
  one_hot_p1 <- one_hot(p1, n_var)
  one_hot_p2 <- one_hot(p2, n_var)
  
  child1_var <- one_hot_p1
  child2_var <- one_hot_p2
  
  # sample the locis that will be swapped
  swapping_idx <- sample(n_var, k, replace = F)
  swapped_from_p1 <- one_hot_p1[swapping_idx]
  
  child1_var[swapping_idx] <- one_hot_p2[swapping_idx]
  child2_var[swapping_idx] <- swapped_from_p1
  
  child1_var <- mutation(child1_var, mu)
  child2_var <- mutation(child1_var, mu)
  
  child1_idx <- which(child1_var==1)
  child2_idx <- which(chidl2_idx)
  
  return(list("child_1" = child1_idx, "child_2" = child2_idx))
  
}

################ Options for the 1st iteration ################

#' Used to get the most significant covariates by recursively eliminating
#' the one with a p-value > p_val. This function can be used for the 
#' first generation in order to decrease the number of covariates that will
#' be taken into account in the iterations
#' @param dataset the dataframe containing the data
#' @param y the response variable
#' @param p_val the threshold p-value, a covariate is kept if its p-value is
#' less than p_val
#' @return a vector containing the names of the covariates that have been kept
get_most_significant_variables <- function(dataset, y, p_val = 0.05){
  removed_var <- c()
  iterate = T
  variables <- names(dataset)[names(dataset)!=y]
  while(iterate == T){
    formula <- paste(variables, sep="+")
    formula <- paste(y, formula, sep = "~")
    summary_lm <- summary(lm(noquote(formula), dataset))
    
    if (max(summary_lm$coefficients[,4]) > p_val){
      removed <- names(which.max(summary_lm$coefficients[,4]))
      removed_var <- c(removed_var, removed)
      variables <- variables[variables != removed]
    }
    
    if (max(summary_lm$coefficients[,4]) < p_val){
      iterate = F
    }
  }
  return(variables)
}

get_largest_interactions <- function(dataset, y, nb.var, max = 0){
  #' Usded to get the c
  var_dataset <- dataset[-which(names(dataset)==y)]
  correlation_matrix <- cor(var_dataset)
  
   
}

######################### Iterations ##########################

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
#' @return a list of individuals each a list with fields:
#' \item{variable}{the covariates kept in the linear model}
#' \item{indices}{the indices of these covariates in the data frame}
#' \item{linear_model}{the linear model with these covariates}
first_generation <- function(y, dataset, population_size, interaction = F,
                             most_sig = F, objective_function = "AIC",
                             reg_method){
  if(interaction == T){
    ## add the interaction terms
  }
  if(most_sig == T) {
    names <- get_most_significant_variables(dataset, y)
  } else {
    names <- names(dataset)[names(dataset)!=y]
  }
  n <- length(names)
  n_var <- sample(1:n, population_size, replace = T)
  individuals <- lapply(n_var, function(x) random_selection_regression(y, names, dataset, x, reg_method))
  return(individuals)
}


## Get a new generation from a former one
#' Used to perform an iteration of the Genetic Algorithm and return the
#' next generation from the generation 'individuals'
#' @param y the name of the response variable in the data frame 'dataset'
#' @param dataset the data frame containing the variables in the model
#' @param pop_size the number of individuals in the next generation
#' @param generation_gap the proportion of individuals to bereplaced
#' generation to keep in the next one
#' @param selection the name of the parents selection mechanism, the user
#' can provide the name of his own function defining a parent selection
#' mechanism
#' @param nb_groups the number of subgroups if the tournament selection
#' is the parent selection mechanism
#' @param gene_selection the gene operator, chose between "crossover",
#' "random" for random locis swap or provide the name of your own function
#' inside quotation marks
#' @param nb_points the number of crossover points if gene_selection="crossover"
#' @return a list of pop_size new models each a list with fields
#' \item{variables}{the covariates kept in the model}
#' \item{indices}{the indices of the covariates kept in the model}
#' \item{linear_model}{the linear model for these covariates}
iterate_generations <- function(y, dataset, individuals, objective, 
                                pop_size, generation_gap,
                                selection = "prop", nb_groups,
                                gene_selection = "crossover",
                                nb_pts = 1, reg_method, mu){
  n_var <- ncol(dataset)-1
  names <- names(dataset)[which(names(dataset)!=y)]
  
  k_prev_gen <- get_k_fittest_ind(individuals, objective,
                                  k = (1 - generation_gap) * pop_size)
  
  ## parents selection
  
  if(selection =="tournament"){
    parents <- tournament_selection(y, dataset, individuals, 
                                    k = nb_groups, n_var,
                                    objective = objective)
  }
  
  if(selection =="prop"){
    parents_idx <- lapply(1:pop_size,
                          function(x) chose_parents_prop(individuals,
                                                         objective))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  if(selection =="prop_random"){
    parents_idx <- lapply(1:pop_size,
                          function(x) chose_parents_prop_random (individuals,
                                                                 objective))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  if(selection =="random"){
    parents_idx <- lapply(1:pop_size,
                          function(x) chose_parents_random (individuals))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  ## gene selection
  
  if(gene_selection == "crossover"){
    chld_idx <- lapply(1:pop_size, function(x) 
      get_indices_crossover_gm(order(sample(1:n_var, nb_pts)), 
                               parents[[(2*x-1)]],
                               parents[[(2*x)]], n_var, mu))
    
    chld_1 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      x$child_1, dataset, reg_method))
    chld_2 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      x$child2, dataset, reg_method))
    children <- c(chld_1, chld_2)
  }
  
  if(gene_selection == "random"){
    chld_idx <- lapply(1:pop_size, function(x) 
      random_chromosomes_swap(sample(1:n_var, k), parents[[(2*x-1)]],
                              parents[[(2*x)]],n_var, mu))
    
    chld_1 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      x$child_1, dataset, reg_method))
    chld_2 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      x$child2, dataset, reg_method))
    children <- c(chld_1, chld_2)
  }
  
  ## here we only keep the (new gener pop size - nb_parents_kept) children 
  ## with the best fitness
  nb_child_kept = generation_gap * pop_size
  child_kept <- get_k_fittest_ind(children, objective, k = nb_child_kept)
  
  return(c(k_prev_gen, child_kept))
  
}


mutation <- function(offspring, mu){
  prob_mutation <- runif(length(offspring),0,1)
  offspring[which(prob_mutation < mu)] <- abs(offspring[which(prob_mutation < mu)] - 1)
  return(offspring)
}

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
#' @param generation_gap The number of the individuals to be replaced
#' @param mu The mutation rate
#' @return List containing the fitted regression, the optimal objective value,
#' and number of iterations used.
#' @examples
#'

select <- function(y, dataset, reg_method = NULL, n_iter = 200, pop_size = 2 * n, objective = "AIC",
                   interaction = F, most_sig = F, selection = "prop", nb_groups = 4, generation_gap = 0.25,
                   gene_selection = "crossover", nb_pts = 1, mu = 0.3){
  n <- ncol(dataset)-1
  first_ind <- first_generation(y, dataset, pop_size, interaction, 
                                objective_function = objective, most_sig, reg_method)
  ind <- first_ind
  if_changesize <- T
  if(pop_size < n) {
    if_changesize <- F
  } else {
    pop_sizes <- ceiling(seq(pop_size, n, by = - n/n_iter))
  }
 
  for (i in 1:n_iter){
    if(if_changesize) {
      popsize <- pop_sizes[i]
    }
    ind <- iterate_generations(y, dataset, ind, objective,
                               pop_size,
                               generation_gap,
                               selection, nb_groups,
                               gene_selection, nb_pts, reg_method, mu)
  }
  objectives <- get_objective_for_population(ind, objective)
  objectives <- unlist(objectives)
  
  return(ind[which.max(objectives)])
  
}




