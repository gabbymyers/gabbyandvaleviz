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


#Iowa Plots

#plotting with year on x axis and grouped by months
ggplot(foriowachart, aes(x=year, y=lostcol, fill = months))+
  geom_bar(stat = "identity", position = 'dodge', scale_fill_manual(values =cbbPalette))



#months on x axis and grouped by year


g <- ggplot(foriowachart, aes(x=months, y=lostcol, fill = year))+
  geom_bar(stat = "identity", position = 'dodge') + 
  scale_fill_brewer(palette = "Accent") +
  theme_minimal()

g

p <- g + ggtitle("Lost Bee Colonies in Iowa") +
  xlab("Year") + ylab("Lost Colonies") +
  theme(plot.title = element_text(face="bold", 
                                  size = 18, 
                                  hjust = 0.5))
p



#----------------------------------------------
colony

winter <- subset(colony, months %in% c("October-December"))
winter

new_winter <- subset(colony, !(state %in% c("United States")))

new_winter

g <- ggplot(new_winter, aes(x=year, y=colony_lost))+
  geom_bar(stat = "identity", position = 'dodge') + 
  
  scale_fill_brewer(palette = "Accent") +
 
  theme_minimal()+ 
  
  ggtitle("Lost Bee Colonies in All States (Oct - Dec)") +
  
  xlab("Year") + ylab("Lost Colonies") +
  
  theme(plot.title = element_text(face="bold", 
                                  size = 18, 
                                  hjust = 0.5))
g



#-----------------------------------------------------------------
NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)

new_new_winter <- new_winter
new_new_winter$regions <- sapply(new_winter$state, 
                     function(x) names(region.list)[grep(x,region.list)])
new_new_winter

new_new_winter$regions <- as.character(new_new_winter$regions)

g <- ggplot(new_new_winter, aes(x=year, y=colony_lost, fill = regions))+
  geom_bar(stat = "identity") + 
  
  scale_fill_brewer(palette = "Accent") +
  
  theme_minimal()+ 
  
  ggtitle("Lost Bee Colonies in US (Oct - Dec)") +
  
  xlab("Year") + ylab("Lost Colonies") +
  
  theme(plot.title = element_text(face="bold", 
                                  size = 18, 
                                  hjust = 0.5))
g
