steph <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/stephen-curry.csv", stringsAsFactors = FALSE)
kd <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/kevin-durant.csv", stringsAsFactors = FALSE)
klay <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/klay-thompson.csv", stringsAsFactors = FALSE)
draymond <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/draymond-green.csv", stringsAsFactors = FALSE)
iggy <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/andre-iguodala.csv", stringsAsFactors = FALSE)

steph$name <- "Stephen Curry"
kd$name <- "Kevin Durant"
klay$name <- "Klay Thompson"
draymond$name <- "Draymond Green"
iggy$name <- "Andre Iguodala"


steph$shot_made_flag[steph$shot_made_flag == 'n'] <- 'shot_no'
steph$shot_made_flag[steph$shot_made_flag == 'y'] <- 'shot_yes'

kd$shot_made_flag[kd$shot_made_flag == 'n'] <- 'shot_no'
kd$shot_made_flag[kd$shot_made_flag == 'y'] <- 'shot_yes'

klay$shot_made_flag[klay$shot_made_flag == 'n'] <- 'shot_no'
klay$shot_made_flag[klay$shot_made_flag == 'y'] <- 'shot_yes'

draymond$shot_made_flag[draymond$shot_made_flag == 'n'] <- 'shot_no'
draymond$shot_made_flag[draymond$shot_made_flag == 'y'] <- 'shot_yes'

iggy$shot_made_flag[iggy$shot_made_flag == 'n'] <- 'shot_no'
iggy$shot_made_flag[iggy$shot_made_flag == 'y'] <- 'shot_yes'

steph$minute <- (steph$period * 12) - steph$minutes_remaining
kd$minute <- (kd$period * 12) - kd$minutes_remaining 
klay$minute <- (klay$period * 12) - klay$minutes_remaining
draymond$minute <- (draymond$period * 12) - draymond$minutes_remaining
iggy$minute <- (iggy$period * 12) - iggy$minutes_remaining


sink("andre-iguodala-summary.txt")
summary(iggy)
sink()
summary(kd)
summary(klay)
summary(draymond)
summary(iggy)

sink()


full_team <- rbind(steph,kd,klay,draymond,iggy)

head(full_team,5)



write.csv(full_team,'shots-data.csv')



dim(full_team)
