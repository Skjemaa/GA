#' Function that executes alleles swap. It samples k indices in
#' range(number of genes) and swaps the alleles of the 2 parents
#' at these indices
#' @param p1,p2 the 2 parents: lists with fields (variables, indices, linear_model)
#' @param k number of locis to be swapped
#' @param n_var length of the chromosome

random_chromosomes_swap <- function(p1, p2, k, n_var){
  ## sample k locis and swap the alleles at these locis among the 2 parents
  
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