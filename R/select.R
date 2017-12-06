#' Selects covariates for regression using a genetic algorithm
#'
#' @param dataset The dataset in matrix form with last column being the dependent variable.
#' @param regression The regression form to use (either lm or glm).
#' @param criterion The objective criterion to use (default AIC).
#' @param parent_selection The selection method for choosing parents in GA.
#' @param gene_selection The selection method for choosing genes in GA.
#' @param num_iter The maximum number of iterations for the algorithm
#' @return List containing the fitted regression, the optimal objective value,
#' and number of iterations used.
#' @examples
#'
select <- function(dataset, regression, criterion='AIC', parent_selection='fitness',
                   gene_selection='default', num_iter=20) {
  print('hello')
  return(1)
}
