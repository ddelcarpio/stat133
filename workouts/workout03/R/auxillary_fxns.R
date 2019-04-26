#Daniel del Carpio
#Private Checker Functions


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
  mode <- (trials * prob) + prob
  if (mode == floor(mode)) {
    return(c(m, m - 1))
  }
  return(floor(mode))
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