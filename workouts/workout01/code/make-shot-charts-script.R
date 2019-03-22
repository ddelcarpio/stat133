library(ggplot2)
library(jpeg)
library(grid)
steph <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/stephen-curry.csv", stringsAsFactors = FALSE)
kd <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/kevin-durant.csv", stringsAsFactors = FALSE)
klay <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/klay-thompson.csv", stringsAsFactors = FALSE)
draymond <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/draymond-green.csv", stringsAsFactors = FALSE)
iggy <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/andre-iguodala.csv", stringsAsFactors = FALSE)
shots_data <- read.csv("C:/Users/danie/OneDrive/Documents/GitHub/stat133workout01/workout01/data/shots-data.csv", stringsAsFactors = FALSE)

klay_scatter <- ggplot(data = klay) + geom_point(aes(x=x,y=y,color=shot_made_flag))

klay_scatter

court_file <- "C:/Users/danie/OneDrive/Documents/Github/stat133workout01/workout01/images/nba-court.jpg"

court_image <- rasterGrob(readJPEG(court_file),width=unit(1,"npc"),height=unit(1,"npc"))

klay_shot_chart <- ggplot(data = klay) + annotation_custom(court_image, -250,250,-50,420) + geom_point(aes(x = x, y=y, color = shot_made_flag)) + ylim(-50,420) + ggtitle('Shot Chart: Klay Thompson (2016 season)') + theme_minimal()
klay_shot_chart

steph_shot_chart <- ggplot(data = steph) + annotation_custom(court_image, -250,250,-50,420) + geom_point(aes(x = x, y=y, color = shot_made_flag)) + ylim(-50,420) + ggtitle('Shot Chart: Stephen Curry (2016 season)') + theme_minimal()
steph_shot_chart

draymond__shot_chart <- ggplot(data = draymond) + annotation_custom(court_image, -250,250,-50,420) + geom_point(aes(x = x, y=y, color = shot_made_flag)) + ylim(-50,420) + ggtitle('Shot Chart: Draymond Green (2016 season)') + theme_minimal()
draymond__shot_chart

kd_shot_chart <- ggplot(data = kd) + annotation_custom(court_image, -250,250,-50,420) + geom_point(aes(x = x, y=y, color = shot_made_flag)) + ylim(-50,420) + ggtitle('Shot Chart: Kevin Durant (2016 season)') + theme_minimal()
kd_shot_chart

iggy_shot_chart <- ggplot(data = iggy) + annotation_custom(court_image, -250,250,-50,420) + geom_point(aes(x = x, y=y, color = shot_made_flag)) + ylim(-50,420) + ggtitle('Shot Chart: Andre Iguodala (2016 season)') + theme_minimal()
iggy_shot_chart

gsw_shot_chart <- ggplot(data = shots_data)+annotation_custom(court_image, -250, 250, -50, 420) +geom_point(aes(x = x, y = y, color = shot_made_flag)) +ylim(-50, 420) + ggtitle('Shot Charts: GSW (2016 season)') +theme_minimal() + facet_wrap(~ name)
gsw_shot_chart

