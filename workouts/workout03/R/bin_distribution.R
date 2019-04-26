#Daniel del Carpio
#Binomial Distribution


library(ggplot2)

#'@title Binomial Distribution
#'@description Binomial Distribution as a dataframe with successes and probabliyu
#'@param trials Number of trials
#'@param prob Probablity of success
#'@return dataframe with probability distribition: successes in first column and probablity in second
#'@export
#'@examples
#'bin_dis <- bin_distribution(trials = 5,prob=0.5)
#'plot(bin_dis)
#'

bin_distribution <- function(trials,prob){
  success <- 0:trials
  probability <- bin_probability(success,trials,prob)
  dat <- data.frame(success,probability)
  class(dat) <- c("bindis","data.frame")
  return(dat)
}

#'@export
plot.bindis <- function(bindis){
  ggplot(data=bindis, aes(x=success, y=probability)) +
    geom_bar(stat="identity") +ggtitle("binomial distribution")
}