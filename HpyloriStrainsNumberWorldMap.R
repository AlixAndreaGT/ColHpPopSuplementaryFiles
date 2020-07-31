############################################################################
#
#                          H.pylori Phylogenomic Analysis Map
#
############################################################################

  
# Load libraries
library(ggplot2)
library(maps)
library(viridis)

# Check all available geospatial objects:
# help(package='maps')

# Load the map of the world
world <- map('world',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80) )

#Load your dataset
data <-read.csv(file.choose())
head(data)
str(data$Country)

data$Country <- as.character(data$Country)
data$Continent <- as.character(data$Continent)
data$Group <- as.numeric(data$Group)
data$long <- as.numeric(data$long)
data$lat <- as.numeric(data$lat)
data$NumberofIsolate <-as.numeric(data$NumberofIsolate)

#Graph the map
g <- data %>%
  arrange(NumberofIsolate) %>% 
  mutate( name=factor(Country, unique(Country))) %>% 
  ggplot() +
  geom_polygon(data = world, aes(x=long, y =lat, group=group), fill="lightgrey", color="white",alpha=1) +
  theme_void() +
  geom_point( aes(x=long, y=lat, size=NumberofIsolate, colour=Group)) +
  scale_color_viridis_c (option = "plasma", name="", trans="log",alpha=0.5) + 
  scale_size_continuous(name="Number of Isolates",range=c(1,10)) + 
  theme_void() +
  geom_text(aes(x=long, y=lat,label=NumberofIsolate), size=2)+
  ggtitle("")  + theme(plot.title =
                                          element_text(size = 10, face =
                                                         "bold"), legend.title =
                                          element_text(size = 15), legend.text
                                        = element_text(size = 10))
# With mercator projection 
 g + coord_map()
 
# Print the map
print(g)

#Save the graph
ggsave("~/Documents/WorldMap.pdf")
ggsave("~/Documents/WorldMap.jpeg")




