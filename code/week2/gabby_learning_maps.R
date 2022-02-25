
library(sf)
library(ggspatial)
library(raster)
library(janitor)
library(showtext)
library(colorspace)
library(ggbeeswarm)
library(ggrepel)
library(tidyverse)
library(patchwork)

library(ggplot2)
library(cowplot)

df <- read.csv("Tidy Tuesday Adventures/EUvaccine.csv")
view(df)

mapdata <- map_data("world")
view(mapdata)

mapdata <- left_join(mapdata, df, by = "region")
view(mapdata)

mapdata1 <- mapdata %>% filter(!is.na(mapdata$Perc_vaccinated))
view(mapdata1)

map1 <- ggplot(mapdata1, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = Perc_vaccinated), color = "black")
map1


map2 <- map1 + scale_fill_gradient(name = "% vaccinated", low = 'yellow', high = "red", na.value = "gray50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
map2
### Freestyle

county <- map_data("county")
view(county)

iowa_county <- subset(county, region == "iowa")
view(iowa_county)

iowamap <- ggplot(iowa_county, aes(x = long, y = lat, group=group))+
  geom_polygon(color = "black")
iowamap

countypop_df <- read.csv("Tidy Tuesday Adventures/County_Population_in_Iowa_by_Year.csv")
view(countypop_df)

str(countypop_df)

library(tidyr)
library(emmeans)
library(lme4)
library(lmerTest)
library(tidyverse)
library(readxl)

countypop_df1 <- countypop_df %>%
  separate(Year, c("Month", "Day", "Year"), " ")

iowa_county_2010 <- subset(countypop_df1, Year == "2010")

iowa_county  <- iowa_county  %>%
  rename(County = subregion)

iowa_county_2010$County <- tolower(iowa_county_2010$County)

iowa_county_2010 <- iowa_county_2010 %>%
  separate(County, c("County", "Label"), " ")

iowa_county_2010$County <- gsub("'","", iowa_county_2010$County)

iowa_county <- iowa_county %>%
  separate(County, c("County", "Label"), " ")

popmapdata <- left_join(iowa_county, iowa_county_2010, by = "County")
view(popmapdata)

popmap1 <- ggplot(popmapdata, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = Population), color = "black", size = 1)
popmap1

popmap2 <- popmap1 + scale_fill_gradient(name = "Population", low = '#132B43', high = "#56B1F7", na.value = "gray50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
popmap2

popmap3 <- popmap1 + scale_fill_viridis_b(option = 'H') +
        theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank())
popmap3

popmap3 + ggtitle("2010 Iowa County Populations")+
  theme(plot.title = element_text(face="bold", 
                                  size = 18, 
                                  hjust = 0.3))
