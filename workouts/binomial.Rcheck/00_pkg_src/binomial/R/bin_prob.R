#Daniel del Carpio
#Binomial Probabilty Function
#stat133 Workout03


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

bin_probability <- function(success,trials,prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success,trials)
  return(bin_choose(trials,success)*(prob**success)*((1-prob)**(trials-success)))
}