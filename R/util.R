#' Get the value of the objective function
#' @title  get_objective_for_population
#' @param individuals list with fields variables, indices, linear_model
#' @param objective name of the objective function inside quotes
#' @return The value of the objective function of the linear model
get_objective_for_population <- function(individuals, objective = "AIC"){
  obj <- lapply(individuals, 
                function(x) - eval(as.name(objective))(x$linear_model))
  return(obj)
}

                
################ Options for the 1st iteration ################

#' Used to get the most significant covariates by recursively eliminating
#' the one with a p-value > p_val. This function can be used for the 
#' first generation in order to decrease the number of covariates that will
#' be taken into account in the iterations
#' @title get_most_significant_variables
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
                
                
#' Used to get the largest interaction terms.
#' @title get_largest_interactions
#' @param dataset the dataframe containing the data
#' @param y the response variable
#' @param nb.var the threshold p-value, a covariate is kept if its p-value is
#' less than p_val
#' @return a vector containing the names of the covariates that have been kept

get_largest_interactions <- function(y,dataset){
  var_dataset <- dataset[-which(names(dataset)==y)]
  names <- names(var_dataset)
  corr_matrix <- cor(var_dataset)
  largest_int <- c()
  for (i in 2:ncol(var_dataset)){
    for (j in 1:(i-1)){
      if(abs(corr_matrix[i,j])> 0.75){
        largest_int <- c(largest_int, paste(names[i], names[j], sep=":"))
      }
    }
  }
  return(largest_int)
}

#' utility function to encode indices in a binary vector
#' @title one_hot
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
                
#' Used to get the probabilities of each individual in a list
#' of (variables, indices, linear_model) in the next generation
#' proportionally to the value of their objective
#' @title get_prob_individuals
#' @param individuals list of (variables, indices, linear_model) see the
#' results section of random_selection_regression for more details
#' @param objective objective function
#' @return a list of probabilities that correspond to the objective value.
get_prob_individuals <- function(individuals, objective = "AIC"){
  
  objectives <- unlist(lapply(individuals, 
                              function(x) - eval(as.name(objective))(x$linear_model))) 
  fitness_rank <- rank(objectives)
  probs <- fitness_rank/sum(fitness_rank)                            
  return(probs)
}
                              
mutation <- function(offspring, mu){
  prob_mutation <- runif(length(offspring),0,1)
  offspring[which(prob_mutation < mu)] <- abs(offspring[which(prob_mutation < mu)] - 1)
  return(offspring)
}

                              
