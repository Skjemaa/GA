######################### Iterations ##########################
## do the first iteration or with the most significant variables
#' Performs the first iteration of the Genetic algorithm
#' Generates the first individuals
#' @title first_generation
#' @param y the response variable in the dataframe 'dataset'
#' @param dataset the data frame containing the variables in the model
#' @param population_size the number of individuals to generate
#' @param interaction a boolean indicating if we want to take into account
#' terms coming from interactions, False by default
#' @param most_sig a boolean indicating if we want to only consider the most
#' significant covariates
#' @param objective_function The objective criterion to use (default AIC).
#' @param reg_method regression method
#' @return a list of individuals each a list with fields:
#' \item{variable}{the covariates kept in the linear model}
#' \item{indices}{the indices of these covariates in the data frame}
#' \item{linear_model}{the linear model with these covariates}
first_generation <- function(y, dataset, population_size, interaction = F,
                             most_sig = F, objective_function = "AIC",
                             reg_method){

  if(most_sig == T) {
    names <- get_most_significant_variables(dataset, y)
    dataset <- dataset[c(y, names)]
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
update_generations <- function(y, dataset, individuals, objective, 
                               pop_size, generation_gap,
                               parent_selection, nb_groups,
                               gene_selection, gene_operator,
                               nb_pts, reg_method, mu){
  n_var <- ncol(dataset) - 1
  names <- names(dataset)[which(names(dataset)!=y)]
  
  k_prev_gen <- get_k_fittest_ind(individuals, objective,
                                  k = floor((1 - generation_gap) * pop_size))
  
  ## parents selection
  
  if(parent_selection == "tournament"){
    parents <- tournament_selection(y, dataset, individuals, 
                                    k = nb_groups, n_var,
                                    objective = objective)
    #print(parents)
  }
  
  if(parent_selection == "prop"){
    parents_idx <- lapply(1:pop_size,
                          function(x) chose_parents_prop(individuals,
                                                         objective))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  if(parent_selection == "prop_random"){
    parents_idx <- lapply(1:pop_size,
                          function(x) chose_parents_prop_random (individuals,
                                                                 objective))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  if(parent_selection == "random"){
    parents_idx <- lapply(1:pop_size,
                          function(x) chose_parents_random (individuals))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  ## gene selection
  chld_idx <- lapply(1:(length(parents)/2), function(x) 
    gene_selection(gene_operator,order(sample(1:n_var, nb_pts)), 
                   parents[[(2*x-1)]],parents[[(2*x)]], 
                   nb_pts, n_var, mu))
  
  chld_1 <- lapply(chld_idx, function(x) regression(y, names, 
                                                    x$child_1, dataset, reg_method))
  chld_2 <- lapply(chld_idx, function(x) regression(y, names, 
                                                    x$child_2, dataset, reg_method))
  children <- c(chld_1, chld_2)
  
  ## here we only keep the (new gener pop size - nb_parents_kept) children 
  ## with the best fitness
  nb_child_kept = round(generation_gap * pop_size)
  child_kept <- get_k_fittest_ind(children, objective, k = nb_child_kept)
  return(c(k_prev_gen, child_kept))
}
