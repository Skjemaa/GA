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
               "The dataset is not a data frame.")


})


test_that("Check Output object is a list", {
  result1 <- select(y="crim", dataset=Boston, reg_method = NULL, n_iter = 200, pop_size = 2 * n, objective = "AIC",
                    interaction = F, most_sig = F, parent_selection = "prop", nb_groups = 4, generation_gap = 0.25,
                    gene_selection = NULL, nb_pts = 1, mu = 0.3, err = 1e-6)
  expect_equal(class(result1), "list")
  expect_equal(length(result1), 4)

})

test_that("Check that in some case, maximum number of iterations is useful",{
  result1 <-select(y="crim", dataset=Boston, reg_method = NULL, n_iter = 20, pop_size = 2 * n, objective = "AIC",
                  interaction = F, most_sig = F, parent_selection = "prop", nb_groups = 4, generation_gap = 0.25,
                  gene_selection = NULL, nb_pts = 1, mu = 0.3, err = 1e-6)
  expect_true(result1$count ==20)
})

test_that("With different mechanism, out algorithm converges to different result",{
  result11 <- select(y="crim", dataset=Boston, parent_selection = "prop")
  result12 <- select(y="crim", dataset=Boston, parent_selection = "random")
  result13 <- select(y="crim", dataset=Boston, parent_selection = "prop_random")
  result14 <- select(y="crim", dataset=Boston, parent_selection = "tournament")

  expect_false(result12 == result11)
  print(result11$model)
  print(result12$model)
})

