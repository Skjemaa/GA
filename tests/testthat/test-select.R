library(testthat)
############################################################
######### integration test for select() function ###########
context("Integration test for select() function")


test_that("Input errors", {
  expect_error(select(dataset=Boston),
               'argument "y" is missing, with no default')

  expect_error(select(y="crim"),
               'argument "dataset" is missing, with no default')

  expect_error(select(y="crim", dataset= as.matrix(Boston)),
               "Y can't be found in dataset")


})


test_that("Check Output object is a list", {
  result1 <- select(y="crim", dataset=Boston, reg_method = NULL, n_iter = 200, objective = "AIC",
                    interaction = F, most_sig = F, parent_selection = "prop", nb_groups = 4, generation_gap = 0.25,
                    gene_selection = NULL, nb_pts = 1, mu = 0.3, err = 1e-6)
  expect_equal(class(result1), "list")
  expect_equal(length(result1), 3)

})

test_that("Check that in some case, maximum number of iterations is useful",{
  result1 <-select(y="crim", dataset=Boston, reg_method = NULL, n_iter = 20, objective = "AIC",
                  interaction = F, most_sig = F, parent_selection = "prop", nb_groups = 4, generation_gap = 0.25,
                  gene_selection = NULL, nb_pts = 1, mu = 0.3, err = 1e-6)
  expect_true(result1$iterations ==20)
})

test_that("With different mechanism, our algorithm can converge to same result",{
  result11 <- select(y="crim", dataset=Boston, parent_selection = "prop")
  result12 <- select(y="crim", dataset=Boston, parent_selection = "random")
  result13 <- select(y="crim", dataset=Boston, parent_selection = "prop_random")
  result14 <- select(y="crim", dataset=Boston, parent_selection = "tournament")

  expect_true(result12$objective == result11$objective)
})

test_that("The variables are correctly chosen", {
  simulation <- function(c,n,k=3){
      X <- matrix(rep(0,n*k),n,k)
      for (i in 1:k){
         X[,i]<- rnorm(n, runif(1,-50,50), runif(1,0,1))
     }
     X <- as.data.frame(X)
     X[,((k+1):c)] <- rnorm(n*(c-k), 0, 0.2)   
     X$Y <- 5*X[,1]+3*X[,2]- 2*X[,3]
     X
     colnames(X)<- c(paste0("X", 1:c),"Y")
     return(X)
 }
  X <- simulation(10,100,3)  
  result <- select("Y", X, most_sig = T, n_iter = 500)
  
  expect_equal(result[[1]]$indices, c(1,2,3))
})
