library(tidyverse)
library(tidyr)
library(emmeans)
library(lme4)
library(lmerTest)
library(tidyverse)
library(readxl)
library(maps)
library(ggplot2)
library(viridis)
library(ggrepel)

county <- map_data("county") #map_data is within the maps package 

iowa_map_data <- subset(county, region == "iowa")

##the subregion column in the iowa_map_data dataframe contains county names, 
##so i am renaming that column 
iowa_map_data  <- iowa_map_data  %>%
  rename(County = subregion)

iowamap <- ggplot(iowa_map_data, aes(x = long, y = lat, group=group))+
  geom_polygon(color = "black")
iowamap

county_rent_df <- read.csv("Tidy Tuesday Adventures/B8E03690-66DF-3A0E-8510-36A40C679A07.csv") #rent price data from USDA ARS

#because the county column in in lowercase in the map data, 
#i need to change the rent county column to lowrcase

county_rent_df$County <- tolower(county_rent_df$County)

#merging county map data and land rent data 
combined_df <- left_join(iowa_map_data, county_rent_df, by = "County")

## Make a map!
map1 <- ggplot(combined_df, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = Value), color = "black", size = .5)
map1

# make the map a little prettier
map2 <- map1 + scale_fill_gradient(name = "2021 Land Rent", low = '#132B43', high = "#56B1F7", na.value = "gray50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())

map2 + ggtitle("2021 Land Rent")+
  theme(plot.title = element_text(face="bold", 
                                  size = 50, 
                                  hjust = 0.3,
                                  vjust = .1))

##O'brien county is coded differently in each dataset so i need to deal with that. 
## in iowa map data it iis"obrien"
## in land rent data it is "o brien"
## I will replace it in the iowa rent data

county_rent_df$County[county_rent_df$County == "o brien"] <- "obrien" #replace

combined_df <- left_join(iowa_map_data, county_rent_df, by = "County") #rejoin

map1 <- ggplot(combined_df, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = Value), color = "black", size = 1)
map1


map2 <- map1 + scale_fill_gradient(name = "2021 Land Rent", low = '#132B43', high = "#56B1F7", na.value = "gray50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())

map2 + ggtitle("2021 Land Rent")+
  theme(plot.title = element_text(face="bold", 
                                  size = 50, 
                                  hjust = 0.3,
                                  vjust = .1))
#now all the counties have data!

##messing with the colors
map4 <- map1 + scale_fill_viridis(option = "rocket") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
map4

map4 <- map4 + ggtitle("2021 Land Rent")+
  theme(plot.title = element_text(face="bold", 
                                  size = 40, 
                                  hjust = 0.3,
                                  vjust = .1))


## attemt to label grundy county bc it was the highest

grundy <- subset(combined_df, County == "grundy")


grundy <- subset(grundy, order == 25692)

map4 + geom_label_repel(data = grundy,
                        aes(label = Value, size = NULL, color = NULL),
                        nudge_y = 1,
                        nudge_x = -.2,
                        segment.size = 0.2,
                        segment.color = "black",
                        direction = "x")
