# This file calls the functions from utilities.R to fit a model using genetic algorithm
#' @name select
#' @title Genetic Algorithm for Model Selection
#' @export
#' @description This is main call function to run package GA.  This package is comprised of
#' a main execution file (\code{select.R}) and a R file comtaining all utilities functions
#' for execution (\code{utilities.R}).  The user can enter enter a dependent variable and  a
#' dataset to execute this function.
#' @usage select(y, dataset, reg_method = NULL, n_iter = 200, pop_size = 2 * n, objective = "AIC",
#' interaction = F, most_sig = F, parent_selection = "prop", nb_groups = 4, generation_gap = 0.25,
#' gene_selection = "crossover", nb_pts = 1, mu = 0.3, err = 1e-6)
#' @details Contained in the list below are the invdividual functions that are called during the
#' execution of the genetic algorithm.
#' \itemize{
#'  \item{first_generation()}: {Generates the first generation}
#'  \item{update_generation()}: {Updates the generation}
#'  \item{get_objective_for_population()}: {Gets objective values for each model}
#' @param y (character) Column name of the dependent variable
#' @param dataset (data frame)The dataset in matrix form with last column being the dependent variable.
#' @param reg_method (character) "lm" or "glm". methods for fitting the data
#' @param n_iter (int) The maximum number of iterations allowed when running GA
#' @param pop_size (int) The number of individuals per generation.
#' @param objective (character) The objective criterion to use (default AIC).
#' @param interaction (logical) Whether to add the interaction terms to the independent variables.
#' @param most_sig (logical) Whether to use the most significant variables inside the first_generation function.
#' @param parent_selection (character) The mechanism to select parents. Selection mechanisms are "prop","prop_random", "random" or "tournament".
#' @param nb_groups (int) The number of groups chosen to do using the tournament selection.
#' @param generation_gap ( numeric) The proportion of the individuals to be replaced by offspring.
#' @param gene_selection (function) The additional selection method for choosing genes in GA.
#' refer to gene_selection to see the required inputs and the desired form of output
#' @param gene_operator If the user doesn't provide his own gene_selection method. Then the gene_operator is used.
#' @param nb_pts (int) The number of points that used in crossover
#' @param mu (numeric) The mutation rate
#' @param err (numeric) The convergence threshold (if the difference between last iteration and current is
#' less than err, the algorithm stops)
#' @return \code{select} returns a list with elements:
#'\itemize{
#'  \item{\code{count}}: {number of iterations until getting the selection}
#'  \item{\code{variables}}: {The names of variables that selected}
#'  \item{\code{model}}: {a \code{lm} or \code{glm} object}
#'  \item{\code{fitness_value}}: {the value of fitness function of the returned model}
#'  }
#' @examples
#' select("mpg", mtcars)
#' select("crim", Boston)
#'

select <- function(y, dataset, reg_method = NULL, n_iter = 200, pop_size = 2 * n, 
                   objective = "AIC", interaction = F, most_sig = F, 
                   parent_selection = "prop", nb_groups = 4, generation_gap = 0.25,
                   gene_selection = NULL, gene_operator = "crossover", 
                   nb_pts = 1, mu = 0.3, err = 1e-6){
  
  if(interaction == T){
    l <- get_largest_interactions(y, dataset)
    s <- sapply(l, function(x) strsplit(x, split = ":"))
    interactions <- dataset
    for(i in 1:length(s)){
      col_1 <- dataset[which(names(dataset)==s[[i]][1])]
      col_2 <- dataset[which(names(dataset)==s[[i]][2])]
      interactions <- cbind.data.frame(interactions, 
                                       col_1/col_2)
    }
    names(interactions) <- c(names(dataset), l)
    dataset <- interactions
  }
  
  if(most_sig == T) {
    names <- get_most_significant_variables(dataset, y)
    dataset <- dataset[c(y, names)]
  }
  
  n <- ncol(dataset) - 1
  
  # Input check
  if(is.null(dataset)) {
    stop("Dataset shouldn't be null.")
  }
  if(length(which(names(dataset) == y)) == 0) {
    stop("Y can't be found in dataset.")
  }
  if(length(which(c("prop", "random", "tournament", 
                    "prop_random") == parent_selection)) == 0) {
    stop("The parent selection method can only be chosen from the four in help documentation.")
  }
  if(length(which(c("crossover", "swap") == gene_operator)) == 0) {
    stop("Only crossover and swap can be chosen.")
  }
  if(nb_pts >= n) {
    stop("The number of crossover points should be smaller than the number of variables.")
  } else if(nb_pts < 1) {
    stop("The number of crossover points should at least be one.")
  }
  if(generation_gap <= 0 | generation_gap >= 1) {
    stop("The generation gap should be in range (0,1)")
  }
  if(mu <= 0 | mu >= 1) {
    stop("The mutation rate should be in range (0,1)")
  }
  
  
  first_ind <- first_generation(y, dataset, pop_size, interaction, 
                                objective_function = objective, most_sig, reg_method)
  ind <- first_ind
  
  # changing the population size
  if(pop_size < n) {
    pop_sizes <- rep(pop_size, n_iter)
  } else {
    pop_sizes <- ceiling(seq(pop_size, n, by = - n/n_iter))
  }
  objectives <- unlist(get_objective_for_population(first_ind, objective))
  oldoptim <- max(objectives)
  iter <- 0
  if(is.null(gene_selection)) {
    gene_selection <- gene_selection
  }
  for (i in 1:n_iter){
    iter <- iter + 1
    popsize <- pop_sizes[i]
    ind <- update_generations(y, dataset, ind, objective, pop_size, 
                              generation_gap, parent_selection, nb_groups, 
                              gene_selection, gene_operator, nb_pts, 
                              reg_method, mu)
    objectives <- unlist(get_objective_for_population(ind, objective))
    newoptim <- max(objectives)
    if(abs(newoptim - oldoptim) < err & parent_selection != "random" & i > n_iter/4) {
      break;
    }
  }
  iter <- as.list(iter)
  names(iter) <- "iterations"
  objective_value <- as.list(-max(objectives))
  return(c(ind[which.max(objectives)], iter, objective_value))
}




