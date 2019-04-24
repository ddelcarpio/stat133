#Bionmial 
#Daniel del Carpio
#Stat 133 Workout03

#Private Checker Functions

#checks if in input is an appropiate probablity number
check_prob <- function(prob){
  if (prob >= 0 & prob <= 1){
    return(TRUE)
  }
  stop("invalid prob value")
}

#checks if input value trials is positive 
check_trials <- function(trials){
  if (trials > 0){
    return(TRUE)
  }
  stop("invalid trials value")
}

#check if input value sucess is a valid number of sucessses
check_success <- function(success,trials){
  for i in success{
    if (i < 0){
      stop("invalid sucess value")
    }
    if(i > trials){
      stop("success cannot be greater than trials")
    }
  }
}

#Private Auxiliary Functions

#Compute the expected value of a binomial distribution
aux_mean <- function(trials, prob) {
  return(trials * prob)
}

#Compute the variance of a binomial distribution
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

#Compute the mode of a binomial distribution
aux_mode <- function(trials, prob) {
  m <- (trials * prob) + prob
  if (m == floor(m)) {
    return(c(m, m - 1))
  }
  return(floor(m))
}

#Compute the skewness of a binomial distribution
aux_skewness <- function(trials, prob) {
  top <- (1 - 2 * prob)
  bot <- sqrt(trials * prob * (1 - prob))
  return(top/bot)
}

#Compute the kurtosis of a binomial distribution
aux_kurtosis <- function(trials, prob) {
  top <- (1 - 6 * prob * (1 - prob))
  bot <- (trials * prob * (1 - prob))
  return(top/bot)
}

#' @title Binomial Choose
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param n Number of trials
#' @param k Number of Sucessesfull Trials
#' @return number of combinations in which k sucesses can occur in n trials
#' @export
#' @examples 
#' #How many ways to choose 2 successes from 5 trials?
#' example1 <- bin_choose(n=5,k=2)
#' #How many ways to choose 3 successes from 6 trials?
#' example2 <- bin_choose(n=6,k=3)
#' 

bin_choose <- function(n,k){
  for(i in k){
    if (i > n){
      stop("k (sucesses) cannot be greater than n (trials)")
    }
  }
  return(factorial(n)/(factorial(k)*factorial(n-k)))
}

#'@title Binomial Probabality
#'@description Calculate the probablity of sucesses in trials with a probablity
#'@param success Number of Successes
#'@param trials Number of Trials
#'@param prob Probabality of Success
#'@return Probablity of sucesses in trials with a probablity
#'@export
#'@examples
#'#Probability of getting 2 successes in 5 trials
#'bin_probability(success = 2, trials = 5, prob = 0.5)
#'#Probability of getting 2 or less sucesses in 5 trials
#'bin_probability(success = 0:2, trials = 5, prob = 0.5)
#'

bin_probability <- function(success,trials,prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success)
  return(bin_choose(trials,success)*(prob**success)*((1-prob)**(trials-success)))
}


