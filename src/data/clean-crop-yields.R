# A script for cleaning soybean yield data
#
# Author(s): Brian
#
# Created: 2019-10-11
# Last Updated: 2019-10-11


library(tidyverse)


yields1950 <- read.csv('../../data/raw/yields-1950-1974.csv', stringsAsFactors = FALSE)
yields1975 <- read.csv('../../data/raw/yields-1975-1999.csv', stringsAsFactors = FALSE)
yields2000 <- read.csv('../../data/raw/yields-2000-2018.csv', stringsAsFactors = FALSE)

allYields <- bind_rows(yields1950, yields1975, yields2000) %>% 
  arrange(Year, State, County) %>% 
  select(Year, State, County, Ag.District, Value) %>% 
  rename(
    AgDistrict = Ag.District,
    Yield = Value
  )
      
write.csv(allYields, '../../data/processed/yields-by-country.csv', row.names=FALSE)
