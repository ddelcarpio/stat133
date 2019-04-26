#Bionmial 
#Daniel del Carpio
#Stat 133 Workout03


library("ggplot2")

#Private Checker Functions

#checks if in input is an appropiate probablity number
check_prob <- function(prob){
  if (prob >= 0 & prob <= 1){
    return(TRUE)
  }
  stop("\ninvalid prob value")
}

#checks if input value trials is positive 
check_trials <- function(trials){
  if (trials > 0){
    return(TRUE)
  }
  stop("\ninvalid trials value")
}

#check if input value sucess is a valid number of sucessses
check_success <- function(success,trials){
  for (i in success){
    if (i < 0){
      stop("\ninvalid sucess value")
    }
    if(i > trials){
      stop("\nsuccess cannot be greater than trials")
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
      stop("\nk (sucesses) cannot be greater than n (trials)")
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
  check_success(success,trials)
  return(bin_choose(trials,success)*(prob**success)*((1-prob)**(trials-success)))
}

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

#'@title Binomial Cumulative Distribution
#'@description Creates an object of cumulative binomial distribution
#'@param trials Number of trials
#'@param prob Probability of success
#'@return Dataframe with both the probabilty distribution and the cumulative probabilities
#'@export
#'@examples
#'bin_cum <- bin_cumulative(trials = 5, prob = 0.5)
#'plot(bin_cum)
#'

bin_cumulative <- function(trials,prob){
  dat <- bin_distribution(trials,prob)
  cum <- c()
  for (i in 1:(trials+1)){
    cum[i] <- sum(dat$p[1:i])
  }
  dat$cumulative <- cum
  class(dat) <- c("bincum","data.frame")
  return(dat)
}
 
#' @export
plot.bincum <- function(x) {
  plot(x$success, x$cumulative, type = "p",
       xlab = "successes", ylab = "probability")
  lines(x$success, x$cumulative)
}

#' @title Binomial Variable
#' @description Creates an object of binomial variable
#' @param trials Number of trials
#' @param prob Probability of success
#' @return An object of binomial variable
#' @export
#' @examples
#' var <- bin_variable(trials = 5, prob = 0.5)
#' sum_var <- summary(var)
#' sum_var
#'
bin_variable <- function(trials, prob) {
  check_prob(prob)
  check_trials(trials)
  object <- list(trials,prob)
  class(object) <- "binvar"
  return(object)
}

#' @export
print.binvar <- function(binvar)
{
  cat('"Binomial variable"\n\n')
  cat("Paramaters\n")
  cat("- number of trials :", binvar[[1]])
  cat("\n- prob of success:", binvar[[2]])
  
  
}

#' @export
summary.binvar <- function(binvar)
{
  x <- list(binvar[[1]],binvar[[2]],aux_mean(binvar[[1]],binvar[[2]]),
            aux_variance(binvar[[1]],binvar[[2]]),aux_mode(binvar[[1]],binvar[[2]]),
            aux_skewness(binvar[[1]],binvar[[2]]),aux_kurtosis(binvar[[1]],binvar[[2]]))
  class(x) <- "summary.binvar"
  return(x)
}

#' @export
print.summary.binvar <- function(summary.binvar)
{
  cat('"Summary Binomial"\n\n')
  cat("Paramaters\n")
  cat("- number of trials  :", summary.binvar[[1]])
  cat("\n- prob of success :", summary.binvar[[2]])
  cat("\n\nMeasures\n")
  cat("- mean       :", summary.binvar[[3]])
  cat("\n- variance :", summary.binvar[[4]])
  cat("\n- mode     :", summary.binvar[[5]])
  cat("\n- skewness :", summary.binvar[[6]])
  cat("\n- kurtosis :", summary.binvar[[7]])
}

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