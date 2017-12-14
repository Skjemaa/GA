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

test_that("With different mechanism, out algorithm converges to different result",{
  result11 <- select(y="crim", dataset=Boston, parent_selection = "prop")
  result12 <- select(y="crim", dataset=Boston, parent_selection = "random")
  result13 <- select(y="crim", dataset=Boston, parent_selection = "prop_random")
  result14 <- select(y="crim", dataset=Boston, parent_selection = "tournament")

  expect_false(result12$objective == result11$objective)
})

