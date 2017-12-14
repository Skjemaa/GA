#################################################################################
# Test the file Utilities.R
#################################################################################
# Unit Test
###########
library(MASS)
# Boston
###########
# test for regression()
regression1 <- regression("crim", c("zn", "indus", "chas", "nox",
                     "rm","age","dis","rad","tax",
                     "ptratio","black", "lstat", "medv"),c(1:3), Boston,"glm")

test_that(" Select the right variables to do regression", {
  expect_equal(regression1$variables[-1],c("zn", "indus", "chas", "nox",
                                      "rm","age","dis","rad","tax",
                                      "ptratio","black", "lstat", "medv")[c(2:3)])
})

###########
# test for one_hot()
test_that("correct encoding of chromosomes", {
  list <- random_selection_regression("crim", c("zn", "indus", "chas", "nox",
                                     "rm","age","dis","rad","tax",
                                     "ptratio","black", "lstat", "medv"), Boston, 5, "glm")
  n <- length(c("zn", "indus", "chas", "nox",
                "rm","age","dis","rad","tax",
                "ptratio","black", "lstat", "medv"))
  my_chromosome <- rep(0,n)
  my_chromosome[list$indices]=1
  expect_equal(my_chromosome, one_hot(list, 13))
})

####################### First Generation ######################
# test for first_generation()
test_that("Repeated initialization should not return the same chromosomes",{
  init1 <-  first_generation("crim", Boston, reg_method = NULL)
  init2 <-  first_generation("crim", Boston, reg_method = NULL)
  expect_false(identical(init1,init2))
})

test_that("Output is a list", {
  init3 <- first_generation("crim", Boston, reg_method = NULL)
  expect_equal(class(init3),"list")
})

####################### Choosing parents ######################
# test for chose_parents_prop()
test_that("Output is length 2",{
  init4 <- first_generation("crim", Boston, reg_method = NULL)
  expect_equal(length(chose_parents_prop(init4)),2)
})
test_that("Output is of class integer",{
  init4 <- first_generation("crim", Boston, reg_method = NULL)
  expect_equal(class(chose_parents_prop(init4)),"integer")
})

# test for chose_parents_prop_random()
test_that("Output is length 2",{
  init4 <- first_generation("crim", Boston, reg_method = NULL)
  expect_equal(length(chose_parents_prop_random(init4)),2)
})
test_that("Output is of class integer",{
  init4 <- first_generation("crim", Boston, reg_method = NULL)
  expect_equal(class(chose_parents_prop_random(init4)),"integer")
})

# test for chose_parents_random()
test_that("Output is length 2",{
  init4 <- first_generation("crim", Boston, reg_method = NULL)
  expect_equal(length(chose_parents_random(init4)),2)
})
test_that("Output is of class integer",{
  init4 <- first_generation("crim", Boston, reg_method = NULL)
  expect_equal(class(chose_parents_random(init4)),"integer")
})

# test that the results from different methods are different.
test_that(" Parents generated from different methods are different",{
  init4 <- first_generation("crim", Boston, reg_method = NULL)
  expect_false(identical(chose_parents_prop(init4), chose_parents_random(init4)))
  expect_false(identical(chose_parents_prop(init4), chose_parents_prop_random(init4)))
  expect_false(identical(chose_parents_prop_random(init4), chose_parents_random(init4)))
})

# test for tournament_selection()
test_that("Output is a list",{
  init4 <- first_generation("crim", Boston, reg_method = NULL)
  expect_equal(class(tournament_selection("crim", Boston, init4, k = 4,
                                           6, objective = "AIC")),"list")
})

####################### Genetic operators #####################
mutation()
gene_selection()






###############################################################
# test for get_prob_individuals()
test_that("Test that the selection between AIC and BIC are different",{
  init4 <- first_generation("crim", Boston, reg_method = NULL)
  prob1 <- get_prob_individuals(init4, "AIC")
  prob2 <- get_prob_individuals(init4, "BIC")
  expect_false(identical(prob1,prob2))
})


