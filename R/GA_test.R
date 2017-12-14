## test for regression
library(MASS)
## test with Boston (lm)
names <- names(Boston)
y <- "crim"
names <- names[which(names != y)]
indices <- c(2,5,7)
lin <- regression(y, names, indices, dataset = Boston)

library(ISLR)
## test with Smarket (glm)
y_smarket <- "Volume"
names_smarket <- names(Smarket)
names_smarket <- names_smarket[which(names_smarket != y_smarket)]
indices_smarket <- c(1,4,8)
regression(y_smarket, names_smarket, indices_smarket, Smarket)

## test for random_selection_regression

## with Boston
random_selection_regression(y, names, Boston, 5)

##with Smarket
random_selection_regression(y_smarket, names_smarket, Smarket, 4)


## test first_generation
## produces a generation at random

## Boston
ind_Boston <- first_generation(y, Boston, population_size = 20)
## Smarket
ind_Smarket <- first_generation(y_smarket, Smarket, population_size = 20)

##test get_objective_for_population()

get_objective_for_population(ind_Boston)
get_objective_for_population(ind_Boston, objective = "BIC")
get_objective_for_population(ind_Smarket)



##test get_fittest_ind
get_fittest_ind(ind_Boston)
get_fittest_ind(ind_Smarket)

## test get_k_fittest_ind
get_k_fittest_ind(ind_Boston, objective = "AIC", k = 2)
get_k_fittest_ind(ind_Boston, objective = "BIC", k = 2)

get_k_fittest_ind(ind_Smarket, objective = "AIC", k = 2)
get_k_fittest_ind(ind_Smarket, objective = "BIC", k = 2)


## test the select function
select(y, dataset = Boston)


