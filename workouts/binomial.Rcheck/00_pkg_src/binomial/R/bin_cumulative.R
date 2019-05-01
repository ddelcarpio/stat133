#Daniel del Carpio 
#Binomial Cumulative Distribution
#stat133 Workout03


#'@title Binomial Cumulative Distribution
#'@description Creates an object of cumulative binomial distribution
#'@param trials Number of trials
#'@param prob Probability of success
#'@return Dataframe with both the probabilty distribution and the cumulative probabilities
#'@export
#'@examples
#'bin_cum <- bin_cumulative(trials = 5, prob = 0.5)
#'plot(bin_cum)

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