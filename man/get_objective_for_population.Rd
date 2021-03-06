% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{get_objective_for_population}
\alias{get_objective_for_population}
\title{get_objective_for_population}
\usage{
get_objective_for_population(individuals, objective = "AIC")
}
\arguments{
\item{individuals}{list with fields variables, indices, linear_model}

\item{objective}{name of the objective function inside quotes}
}
\value{
The value of the objective function of the linear model
}
\description{
Get the value of the objective function
}
