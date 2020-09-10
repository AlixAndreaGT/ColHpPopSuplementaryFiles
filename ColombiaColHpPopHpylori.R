############################################################################
#
#                          H.pylori Phylogenomic Analysis Map
#
############################################################################

#Remove previous elements of your enviroment
rm(list=ls())

# Install and load packages if you don't have
packages <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

packages( c("magrittr", "tidyr", "dplyr", "purrr", "ggplot2", "scatterpie", "rgdal") )

# Colombia map:
Colombia <- readOGR("/Users/alix/Downloads/COL_adm", layer = "COL_adm1")
#Converts an s3 objet (objets whit diferent class) to a data fram for ggplot2
 Colombia <- fortify(Colombia, region="NAME_1")

#Load your metadata file
data <-read.csv(file.choose()); print("Load the file with the metadata")
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
  group_by(Department,Assignedpopulation) %>%
  count(Assignedpopulation)%>%
  spread(Assignedpopulation, n)

Data1[is.na(Data1)] <-0
str(Data1)
#Create a new Column with the total of Isolates by Country
Data1$NumberofIsolates <- rowSums(Data1[2:ncol(Data1)])

# Load the coordinates of each country
Country_Coords <- read.csv(file.choose()); print("Load the file with the coords of each country")

#Combining tables into a Finaldataset
Finaldata <- merge(x=Data1, y=Country_Coords[ , c(1:3)], by = "Department") 
str(Finaldata)

#Define the set of color
#allcolors <- read.csv(file.choose()); print("Load the Colour palette")
#allcolors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

#graph the map
g <- ggplot() +
  geom_polygon(data = Colombia, aes(x=long, y =lat, group=group), fill="lightgrey", color="white",alpha=1)+
  geom_scatterpie(aes(x=Long, y=Lat, group= Department, r= 0.38), data = Finaldata, cols = colnames(Finaldata[,c(2:(ncol(Finaldata)-3))]), color=NA, 
                  legend_name = "Population") +
  scale_fill_manual(values= c(hpAfrica2= "#000000",
                              hpAsia2= "#FFD966",
                              hspAfrica1MiscAmerica= "#C59900",
                              hspAfrica1NAmerica= "#B08958",
                              hspAfrica1Nicaragua= "#C1A98A",
                              hspAfrica1SAfrica= "#D9D9D9",
                              hspAfrica1WAfrica= "#7F7F7F",
                              hspEAsia= "#F4A460",
                              hspIndigenousNAmerica= "#7b4ba0",
                              hspNEurope= "#E662C5",
                              hspIndigenousSAmerica= "#aa8ac3",
                              hspSEurope= "#FFACDF",
                              hspSWEurope= "#FFBBFF",
                              hspSWEuropeColombia= "#DBF1FF",
                              hspSWEuropeHonduras= "#7EC0EE",
                              hspSWEuropeMexico= "#0096FF",
                              Unk= "#7FFFD4")) +
  theme(
    legend.position = "right",
    legend.title = element_text(color = "black"))+
  coord_equal() +
  theme_void() 

# Print the map
print(g)


#Save the graph
ggsave("~/Documents/PieWorldMap.pdf")
print("The graph was save in pdf")
ggsave("~/Documents/PieWorldMap.jpeg")
print("The graph was save in jpeg")
graphics.off()
print("This is the last line")


#geom_scatterpie_legend((Finaldata$NumberofIsolates/70), x=-20, y=5, n=5) +
#coord_equal()= Equal scale cartesian coordinates
