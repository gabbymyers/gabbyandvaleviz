install.packages("tidytuesdayR")  #package where data is stored 
install.packages('tidyverse')

install.packages("devtools")
library('ggplot2')
tuesdata <- tidytuesdayR::tt_load(2022, week = 2) #starting with 2022 week 2 data about bee colonies


colony <- tuesdata$colony
colony

colony$state

#filter data to only Iowa 
iowa <- subset(colony, state %in% c("Iowa"))

iowa$months

months <- iowa$months
year <- iowa$year
lostcol <- iowa$colony_lost

foriowachart <- data.frame(months, year, lostcol)
foriowachart$year <- as.character(foriowachart$year)


ggplot(foriowachart, aes(x=year, y=lostcol, fill = months))+
  geom_bar(stat = "identity", position = 'dodge')

ggplot(foriowachart, aes(x=months, y=lostcol, fill = year))+
  geom_bar(stat = "identity", position = 'dodge')


