####################################################################################################################################################################
#
#                                                              H.pylori Phylogenomic Analysis Map
#
###################################################################################################################################################################


# Load libraries
library(magrittr)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(scatterpie)
library(maps)


# Map of the world:
world <- map('world',col="grey", fill=TRUE, bg="white", lwd=0.3, mar=rep(0,4), border=0, ylim=c(-60,80))

#Load your dataset
data <-read.csv(file.choose())
head(data)
str(data)

#Convert several columns to the kind of variable that you need
Data1 <- data%>%
  map_if(is.factor, as.character)

str(Data1)

#if you want to convert only some columns, for example since 2 to the last, use:
#Data1 <- lapply (data[,c(2:length(data))], as.character)

#Prepare your data
Data1 <- data.frame(data) %>%
  group_by(Country,Assignedpopulation, value) %>%
  summarize(Count = sum(value))

str(Data1)

#Separate the variable AssignedPopulation into columns
tablaconvertida <-Data1%>%
  spread(Assignedpopulation, Count)

#Replace the na with Cero
tablaconvertida[is.na(tablaconvertida)] <-0

#Create a new Column with the total of Isolates by Country
tablaconvertida$NumberofIsolates <- rowSums(tablaconvertida[2:26])

# Load the coordinates of each country
Country_Coords <- read.csv(file.choose())

#Combining tables into a Finaldataset
Finaldata <- merge(x=tablaconvertida, y=Country_Coords[ , c(2:4)], by = "Country")
str(Finaldata)


#graph the map
g <- ggplot() +
  geom_polygon(data = world, aes(x=long, y =lat, group=group), fill="lightgrey", color="white",alpha=1) +
  geom_scatterpie(aes(x=longitude, y=latitude, group= Country, r= log(NumberofIsolates*12)), data = Finaldata, cols = colnames(Finaldata[,c(3:26)]), 
                  position= position_jitter(width = NULL, height = NULL, seed = NA), legend_name = "Population")+
  theme(legend.position = "right") +
  coord_equal() +
  theme_void() 

# Print the map
print(g)


#Para guardar el grafico
ggsave("~/Documents/PieWorldMap.pdf")
ggsave("~/Documents/PieWorldMap.jpeg")

