#Daniel del Carpio
#Functions of Measures
#stat133 Workout03



#'@title Binomial Mean
#'@description Calculates the mean of a binomial variable
#'@param trials Number of trials
#'@param prob Probability of success
#'@return The mean of a binomial variable
#'@export
#'@examples
#' bin_mean(trials = 5, prob = 0.5)
#'
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title Binomial Variance
#' @description Calculates the variance of a binomial variable
#' @param trials Number of trials
#' @param prob Probability of success
#' @return Calculates the variance of a binomial variable
#' @export
#' @examples
#' bin_variance(trials = 5, prob = 0.5)
#'
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#'@title Binomial Mode
#'@description Calculates the mode of a binomial variable
#'@param trials Number of trials
#'@param prob Probability of success
#'@return Mode of a binomial variable
#'@export
#'@examples
#' bin_mode(trials = 5, prob = 0.5)
#'
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#'@title Binomial Skewness
#'@description Calculates the skewness of a binomial variable
#'@param trials Number of trials
#'@param prob Probability of success
#'@return Skewness of a binomial variable
#'@export
#'@examples
#'bin_skewness(trials = 5, prob = 0.5)
#'
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#'@title Binomial Kurtosis
#'@description Calculates the kurtosis of a binomial variable
#'@param trials Number of trials
#'@param prob Probability of success
#'@return Kurtosis of a binomial variable
#'@export
#'@examples
#'bin_kurtosis(trials = 5, prob = 0.5)
#'
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}