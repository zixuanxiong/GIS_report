
##library a bunch of packages
library(spatstat)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(geojson)
library(geojsonio)
library(tmaptools)
library(rgdal)
library(tidyverse)
library(plotly)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)

#Set up some spatial data
#getwd()

#download a zip file containing some boundaries we want to use
#download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", destfile="pstatistical-gis-boundaries-london.zip")
#unzip it
#unzip("statistical-gis-boundaries-london.zip", exdir="prac9_data")
#look at the subfolders in the directory to work out where we want to get our shapefile from
list.dirs("statistical-gis-boundaries-london")
#read in the shapefile
LondonBoroughs <- readOGR("statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp", layer="London_Borough_Excluding_MHW")
#convert it to SF object (for data merge)
LondonBoroughsSF <- st_as_sf(LondonBoroughs)
#Convert to British National Grid
BNG = "+init=epsg:27700"
LondonBoroughsSFBNG <- st_transform(LondonBoroughsSF, BNG)
# in the london datastor, atrribute data is in excel, so i downloaded it first and coverted it to csv.
# the data excel could be downloaded from this website (https://data.london.gov.uk/dataset/ratio-house-prices-earnings-borough)
#read in some attribute data
heratio <- read_csv("1-ratio-house-price-earnings-residence-based.csv")
#check all of the columns have been read in OK
str(heratio)
#merge boundaries and data
LondonBoroughsSFBNG <- left_join(LondonBoroughsSFBNG, heratio, by = c("GSS_CODE" = "New Code"))
# SF to SP (because the sprial polygon dataframe will be applied to make a spatial weight matrix)
LondonBoroughs <- as(LondonBoroughsSFBNG, "Spatial")

##########################################################################
#plot house price/earnings ratio at each borough (change the year to see the difference.)

tm_shape(LondonBoroughs) +tmap_options(max.categories = 7)+
  tm_polygons("X2006",
              style="jenks",
              palette="PuOr",
              title="House price/earnings ratio at 2006")

##########################################################################
#Before being able to calculate Moran’s I and any similar statistics, 
#we need to first define a spatial weights matrix
#First calculate the centroids of all boroughs in London
coordsW <- coordinates(LondonBoroughs)
plot(coordsW)

#binary matrix of queen’s case neighbours (otherwise known as Contiguity edges corners)
#polygons with a shared edge or a corner will be included in computations for the target polygon
#create a neighbours list
nb <- poly2nb(LondonBoroughs, queen=T)
#plot them
plot(nb, coordinates(coordsW), col="red")
#add a map underneath
plot(LondonBoroughs, add=T)
#create a spatial weights object from these weights
swo <- nb2listw(nb, style="C")
head(swo$neighbours)

##########################################################################
#Global Moran’s I :
I_Global_Density <- moran.test(as.numeric(factor(LondonBoroughs@data$X2016)), swo)
I_Global_Density
#all global I at different year will be summaried 
#the summarized table of glocal i index could be found on the report

##########################################################################
#use the localmoran function to generate I for each borough

#I_local: the outcome will be recorded first
I_Local_Density <- localmoran(as.numeric(factor(LondonBoroughs@data$X2016)), swo)
head(I_Local_Density)

# I_local: copy the colume of z-score back into the london boroughs spatial polugons dataframe
I_Local_M2016 <- localmoran(as.numeric(factor(LondonBoroughs@data$X2016)), swo)
LondonBoroughs@data$LocIz_2016 <- I_Local_M2016[,4]

# I_local: map the outputs

#We’ll set the breaks manually based on the rule that data points 
#>2.58 or <-2.58 standard deviations
#away from the mean are significant at the 99% level 
#(<1% chance that autocorrelation not present); 
#>1.96 - <2.58 or <-1.96 to >-2.58 standard deviations are significant 
#at the 95% level (<5% change that autocorrelation not present). >1.65 = 90% etc.
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

#Now create a new diverging colour brewer palette and reverse the order 
#using rev so higher values correspond to red
MoranColours<- rev(brewer.pal(8, "RdGy"))

#I_local: The static map for local moran's I (z-score)
tmap_mode("plot")

tm_shape(LondonBoroughs)+
  tm_polygons("LocIz_2016",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I (z-score)-2016" )+
  tm_legend(show=TRUE,text.size=0.8)+
  tm_compass(north=0,type="arrow", text.size=0.7, size=3, position=c("left", "top"), show.labels = 1)+
  tm_layout(frame=FALSE)

# I_local: plot on an interactive map?

#tm_shape(LondonBoroughs) +
#tm_polygons("LocIz_2006",
#style="fixed",
# breaks=breaks1,
#  palette=MoranColours,
#  midpoint=NA,
#             title="Local Moran's I, House price/earnings ratio at 2006")


##########################################################################

#use the local G function to generate Gi for each borough

# Gi_local: the outcome will be recorded first
Gi_Local_Density <- localG(as.numeric(factor(LondonBoroughs@data$X2016)), swo)
head(Gi_Local_Density)

# Gi_local: copy the row back into the london boroughs spatial polugons dataframe
Gi_Local_Density <- localG(as.numeric(factor(LondonBoroughs@data$X2016)), swo)
LondonBoroughs@data$Gi_2016  <- Gi_Local_Density

# Gi_local: map the outputs

GIColours<- rev(brewer.pal(8, "RdBu"))

# Gi_local: The static map 
tmap_mode("plot")

tm_shape(LondonBoroughs)+
  tm_polygons("Gi_2016",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Getis-Ord Gi*-2016" )+
  tm_legend(show=TRUE,text.size=0.8)+
  tm_compass(north=0,type="arrow", text.size=0.7, size=3, position=c("left", "top"), show.labels = 1)+
  tm_layout(frame=FALSE)






# Gi_local:plot on an interactive map?


