## get indices of the variables taken into account in the children
## with crossover

#' Gene operator. Performs the k points crossover from 2 parents over
#' n_var genes at the indices given in the vector 'points'
#' @param points crossover points
#' @param p1,p2 parents as lists with fields variables, indices, linear_model
#' @param n_var, the total number of covariates in the data ncol(dataset)-1
#' @return a list containing the vectors of indices taken into account into
#' each of the 2 children given by the crossover operator
get_indices_crossover_gm <- function(points, p1, p2, n_var){

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
  
  child1_idx <- which(child1_var == 1)
  child2_idx <- which(child2_var == 1)
  
  return(list("child_1" = child1_idx, "child2" = child2_idx))
}