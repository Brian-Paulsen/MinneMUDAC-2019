library(tidyverse)
library(janitor)
library(sp)
library(maps)
library(maptools)


# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
# https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}


combined <- data.frame()
files <- list.files(path = "../../data/raw/weather/", full.names = TRUE, recursive = TRUE)
dfs <- list()
dfs[length(files)] <- NULL
for(i in 1:length(files)) {
  print(i)
  newDf <- read.csv(files[i], stringsAsFactors = FALSE) %>% 
    clean_names('upper_camel')
  dfs[[i]] <- newDf
}

combined <- bind_rows(dfs)

stations <- distinct(combined, Station, x=Longitude, y=Latitude)

stations$County <- unlist(latlong2county(select(stations, x, y)))

allData <- stations %>% 
  select(Station, County) %>% 
  right_join(combined, 'Station')

write.csv(allData, '../../data/processed/weather-counties-2007-2019.csv', row.names = FALSE)
