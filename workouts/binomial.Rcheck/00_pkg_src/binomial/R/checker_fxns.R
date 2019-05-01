#Daniel del Carpio
#Private Checker Functions
#Stat 133 Workout03


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

