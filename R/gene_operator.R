####################### Genetic operators ########################

#' Genetic operator: 1. Performs the k points crossover from 2 parents over
#' n_var genes at the indices given in the vector 'points'
#' 2. Sample k locis and swap the alleles at these locis among the 2 parents.
#' 3. Gene mutation at a specific rate
#' @param method a string inside quotation marks either "crossover", "random"
#' @param points crossover points
#' @param p1,p2 the 2 parents: lists with fields (variables, indices, linear_model)
#' @param k number of locis to be swapped
#' @param n_var length of the chromosome
gene_selection <- function(method, points, p1, p2, k, n_var, mu){
  if(method=="crossover"){
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
  }
  
  
  if(method=="random"){
    swapping_idx <- sample(1:n_var, size = k, replace = F)
    swapped_from_p1 <- one_hot_p1[swapping_idx]
    
    child1_var[swapping_idx] <- one_hot_p2[swapping_idx]
    child2_var[swapping_idx] <- swapped_from_p1
  }
  
  
  child1_var <- mutation(child1_var, mu)
  child2_var <- mutation(child1_var, mu)
  
  child1_idx <- which(child1_var == 1)
  child2_idx <- which(child2_var == 1)
  
  return(list("child_1" = child1_idx, "child_2" = child2_idx))
}
