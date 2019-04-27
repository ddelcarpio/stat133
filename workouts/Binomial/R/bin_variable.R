#Daniel del Carpio
#Binomial Variable
#stat133 Workout03


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