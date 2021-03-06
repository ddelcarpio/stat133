% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_variable.R
\name{bin_variable}
\alias{bin_variable}
\title{Binomial Variable}
\usage{
bin_variable(trials, prob)
}
\arguments{
\item{trials}{Number of trials}

\item{prob}{Probability of success}
}
\value{
An object of binomial variable
}
\description{
Creates an object of binomial variable
}
\examples{
var <- bin_variable(trials = 5, prob = 0.5)
sum_var <- summary(var)
sum_var

}
