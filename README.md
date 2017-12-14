# Genetic algorithm for lm and glm

## Members

- Skander Jemaa
- River Jenkins
- Yachuan Liu
- Yu Wang

## Main function : select

The main function select can be used with 2 arguments:
- the name of the response variable y
- the name of the dataset

example: select("crim", Boston)

## Use of the most significant covariates and the largest interaction terms

We added options and utility functions to be able to eliminate the covariates with large p-values and to take into account the interactions var1:var2 in lm if the absolute value of the correlation between the 2 covariates are large.

## User defined selection methods:

The user can provide function names as arguments to select the parent selection mechanism, the gene operator and the objective function.
