#Daniel del Carpio
#Function Bionmial Choose



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