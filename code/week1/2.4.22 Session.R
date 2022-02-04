## Gabby and Vale 


install.packages("tidytuesdayR")  #package where data is stored 
install.packages('tidyverse')


install.packages("devtools")
library('ggplot2')
library('dplyr')
tuesdata <- tidytuesdayR::tt_load(2022, week = 2) #starting with 2022 week 2 data about bee colonies


colony <- tuesdata$colony


colony %>%
filter(months == "October-December") %>%
  filter(state, != "United States")

  ggplot() +
  geom_bar(aes(x= year, y= colony_lost, fill = ), 
           stat = "identity", 
           position = 'dodge') +
  theme_bw() 


  

help('filter')




