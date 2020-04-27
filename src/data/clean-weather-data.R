# This script cleans a subset of the weather data from NOAA
#
# Author(s): Brian
#
# Created: 2019-10-22
# Last Updated: 2019-10-22


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


weather2007 <- read.csv('../../data/raw/weather-2007-2010.csv', stringsAsFactors = FALSE)
weather2010 <- read.csv('../../data/raw/weather-2010-2014.csv', stringsAsFactors = FALSE)
weather2015 <- read.csv('../../data/raw/weather-2015-2019.csv', stringsAsFactors = FALSE)

weatherMonthly2007 <- read.csv('../../data/raw/weather-monthly-2007-2010.csv', stringsAsFactors = FALSE)
weatherMonthly2010 <- read.csv('../../data/raw/weather-monthly-2010-2014.csv', stringsAsFactors = FALSE)
weatherMonthly2015 <- read.csv('../../data/raw/weather-month-2015-2019.csv', stringsAsFactors = FALSE)

allWeatherData <- bind_rows(weather2007, weather2010, weather2015) %>% 
  clean_names('upper_camel')

# Get a list of stations
stations <- allWeatherData %>% 
  distinct(Station, y=Latitude, x=Longitude) # change to x,y to make compatible with latlong2county

stations$County <- latlong2county(select(stations, x, y))

# daily weather data
allWeatherDataCounty <- stations %>% 
  select(Station, County) %>% 
  right_join(allWeatherData, 'Station') %>% 
  select(
    County,
    Date,
    Station,
    Precipitation = Prcp,
    SnowFall = Snow,
    SnowDepth = Snwd,
    TempMax = Tmax,
    TempMin = Tmin
  ) %>% 
  arrange(County,  Date, Station)


allMonthData <- bind_rows(weatherMonthly2007, weatherMonthly2010, weatherMonthly2015) %>% 
  clean_names('upper_camel')

# monthly weather data
allMonthDataCounty <- stations %>% 
  select(Station, County) %>% 
  right_join(allMonthData, 'Station') %>% 
  select(
    County,
    Date,
    Station,
    ExtremeMaximumPrecip = Emxp,
    SnowFall = Snow,
    Precipitation = Prcp,
    PrecipOverInch = Dp10,
    PrecipOverTenthInch = Dp01,
    DaysOver90F = Dx90,
    DaysOver70F = Dx70,
    DaysUnder32F = Dx32
  )

write.csv(allWeatherDataCounty, '../../data/processed/weather-data-daily-subset.csv', row.names=FALSE)
write.csv(allMonthDataCounty, '../../data/processed/weather-data-monthly-subset.csv', row.names=FALSE)

