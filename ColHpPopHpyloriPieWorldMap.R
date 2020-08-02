############################################################################
#
#                          H.pylori Phylogenomic Analysis Map
#
############################################################################

  
# Load libraries
rm(list=ls())
library(memisc)
library(assertthat)
library(sqldf)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(oz)
library(scatterpie)
library(rgdal)
library(maptools)
library(maps)
library(viridis)

# Check all available geospatial objects:
# help(package='maps')

# Map of the world:
world <- map('world',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80))

#Load your dataset
data <-read.csv(file.choose())
head(data)
str(data$Country)

#Convert several columns to the kind of variable that you need
data[,c(1:3)] <- lapply(data[,c(1:3)], as.character)
data$Group <- as.numeric(data$Group)
data[,c(4:33)] <- lapply(data[,c(4:33)], as.numeric)
class(data$hpAfrica1)

#graph the map
g <- data %>%
  arrange(NumberofIsolate) %>% 
  mutate( name=factor(Country, unique(Country))) %>% 
  ggplot() +
  geom_polygon(data = world, aes(x=long, y =lat, group=group), fill="lightgrey", color="white",alpha=1) +
  geom_scatterpie(aes(x=long, y=lat, group= Country), data = data, cols = colnames(data[,c(7:33)]))+
  scale_color_viridis_c (option = "plasma", name="", trans="log",alpha=0.5) + 
  geom_text(aes(x=long, y=lat, group = Country, label = Country), data = data, stat = "identity",
            position = position_dodge(width = 0.75), hjust = 1.5, vjust = -2, size = 3,
            check_overlap = FALSE, na.rm = FALSE, show.legend = NA, 
            inherit.aes = TRUE) +
  theme(legend.position = "right") +
  coord_equal() +
  theme_void() 
  

# Print the map
print(g)

#Para guardar el grafico
ggsave("~/Documents/PieWorldMap.pdf")
ggsave("~/Documents/PieWorldMap.jpeg")