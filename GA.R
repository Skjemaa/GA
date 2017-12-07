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


## do the first iteration (at random, other initialization will be added)
first_generation <- function(y, dataset, population_size, interaction = F,
                             objective_function = "AIC"){
  n <- ncol(dataset)
  names <- names(dataset)[names(dataset)!=y]
  n_var <- sample(1:(n-1), population_size, replace = T)
  individuals <- lapply(n_var, function(x) random_selection_regression(y, names, dataset, x))
  return(individuals)
}

get_fittest_individual <- function(individuals, objective = "AIC", fitness = F){
  if(fitness){
    ## case where the objective function is not the fitness function
    # @TODO
    return
  } 
  else{
    idx <- which.max(unlist(lapply(individuals, 
                                   function(x) as.name(objective)(x$linear_model))))
    return(individuals[[idx]])
  }
}

## probabilities of individuals in the next generation
get_prob_individuals <- function(individuals, objective = "AIC"){
  objectives <- unlist(lapply(individuals, 
                              function(x) as.name(objective)(x$linear_model)))
  probs <- objectives / sum(objectives)
  return(probs)
}

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


## utility function one_hot to encode indices in a binary vector
one_hot <- function(p, n_var){
  one_hot_vector <- rep(0, n_var)
  one_hot_vector[p$indices] = 1
  return(one_hot_vector)
}

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
  
  cild1_var[from_p1_idx] <- one_hot_p1[from_p1_idx]
  cild1_var[from_p2_idx] <- one_hot_p2[from_p2_idx]
  
  cild2_var[from_p1_idx] <- one_hot_p2[from_p1_idx]
  cild2_var[from_p2_idx] <- one_hot_p1[from_p2_idx]
  
  child1_idx <- which(child1_var==1)
  child2_idx <- which(chidl2_idx)
  
  return(list("child_1" = child1_idx, "child2" = child2_idx))
}

## tournament selection
tournament_selection(individuals, k = 2){
  ## implement the tournament selection
}

## permutation gap : no need to implement a function for that, just include
## that case in the function iterate_generations

random_chromosomes_swap <- function(p1, p2, k){
  ## sample k chromosomes and swap these chromosomes among the 2 parents
}



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


iterate_generations <- function(n, p){
  
}




