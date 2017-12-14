#' Performs an lm or glm given y, a list of covariates and the
#' indices of the covariates to take into account.
#' @param y Name of the response variable in the dataset
#' @param names Names of the covariates in the dataset
#' @param indices Indices of the covariates considered in the regression
#' @param dataset Name of the dataset
#' @return 'lm' object obtained by performing the regression with the
#' selected covariates
#' @examples 
regression <- function(y,names, indices, dataset){
  if(length(indices)==0){
    indices <- sample(1:length(names), floor(length(names)/2))
  }
  form <- noquote(paste(names[indices], collapse = "+"))
  form <- paste(y, form, sep = " ~ ")
  types <- sapply(dataset, class)
  nb_double <- sum(types == "numeric") + sum(types == "integer")
  if(nb_double == ncol(dataset)){
    lin.reg <- lm(noquote(form), data = dataset)
  }
  else{
    lin.reg <- glm(noquote(form), data = dataset)
  }
  return(list("variables" = names[indices], "indices" = indices, 
              "linear_model" = lin.reg))
}