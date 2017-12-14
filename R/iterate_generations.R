#' Used to perform an iteration of the Genetic Algorithm and return the
#' next generation from the generation 'individuals'
#' @param y the name of the response variable in the data frame 'dataset'
#' @param dataset the data frame containing the variables in the model
#' @param pop_size the number of individuals in the next generation
#' @param permutation_gap the number of individuals of the previous
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

iterate_generations <- function(y, dataset, individuals, objective = "AIC", 
                                pop_size, permutation_gap = 1,
                                selection = "prop", nb_groups = 4,
                                gene_selection = "crossover",
                                nb_pts = 1){

  n_var <- ncol(dataset)-1
  pop_size_last <- length(individuals)
  names <- names(dataset)[which(names(dataset)!=y)]
  
  k_prev_gen <- get_k_fittest_ind(individuals, objective,
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
  
  if(sum(selection == c("tournament", "prop", "random", "prop_random"))==0){
    parents_idx <- lapply(1:(pop_size_last/2),
                          function(x) eval(as.name(selection))(individuals))
    parents_idx <- unlist(parents_idx)
    parents <- individuals[parents_idx]
  }
  
  ## gene selection
  
  if(gene_selection=="crossover"){
    chld_idx <- lapply(1:(pop_size_last/2), function(x) 
      get_indices_crossover_gm(order(sample(1:n_var, nb_pts)), 
                               parents[[(2*x-1)]],
                               parents[[(2*x)]],n_var))
    
    chld_1 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      x$child_1, dataset))
    chld_2 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      x$child2, dataset))
    children <- c(chld_1, chld_2)
  }
  
  if(gene_selection=="random"){
    chld_idx <- lapply(range(pop_size_last/2), function(x) 
      random_chromosomes_swap(sample(1:n_var, k), parents[[(2*x-1)]],
                              parents[[(2*x)]],n_var))
    
    chld_1 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      x$child_1, dataset))
    chld_2 <- lapply(chld_idx, function(x) regression(y, names, 
                                                      x$child2, dataset))
    children <- c(chld_1, chld_2)
  }
  
  if(sum(gene_selection==c("crossover", "random")) == 0){
    children <- eval(as.name(gene_selection))(parents)
  }
  
  ## here we only keep the (new gener pop size - nb_parents_kept) children 
  ## with the best fitness
  nb_child_kept = pop_size - permutation_gap
  child_kept <- get_k_fittest_ind(children, objective, k = nb_child_kept)
  
  return(c(k_prev_gen, child_kept))
  
}
