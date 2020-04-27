# This script cleans the USDA corn and soybean yearly data
#
# Author(s): Brian
#
# Created: 2019-10-13
# Last Updated: 2019-10-13  

library(tidyverse)


gdpDeflator <- read.csv('../../data/processed/gdp-deflactor.csv', stringsAsFactors = FALSE) %>% 
  mutate(Year = as.numeric(str_sub(Date, 1, 4))) %>% 
  group_by(Year) %>% 
  summarise(
    Deflator = mean(GDPDef)
  )

cornPlanted <- read.csv('../../data/raw/corn-soybean-usda/planted-corn.csv', stringsAsFactors = FALSE) %>% 
  mutate(Value = as.numeric(str_remove_all(Value, ',')))
soybeanPlanted <- read.csv('../../data/raw/corn-soybean-usda/planted-soybean.csv', stringsAsFactors = FALSE) %>% 
  mutate(Value = as.numeric(str_remove_all(Value, ',')))

cornPrice <- read.csv('../../data/raw/corn-soybean-usda/price-corn.csv', stringsAsFactors = FALSE) %>% 
  # We want to keep the marketing year column, so change those to year to fix the filter below
  mutate(
    Period = ifelse(Period == 'YEAR', 'YEAR2', Period),
    Period = ifelse(Period == 'MARKETING YEAR', 'YEAR', Period)
  )
soybeanPrice <- read.csv('../../data/raw/corn-soybean-usda/price-soybean.csv', stringsAsFactors = FALSE) %>% 
  mutate(
    Period = ifelse(Period == 'YEAR', 'YEAR2', Period),
    Period = ifelse(Period == 'MARKETING YEAR', 'YEAR', Period)
  )

cornProduction <- read.csv('../../data/raw/corn-soybean-usda/production-corn.csv', stringsAsFactors = FALSE) %>% 
  mutate(Value = as.numeric(str_remove_all(Value, ',')))
soybeanProduction <- read.csv('../../data/raw/corn-soybean-usda/production-soybean.csv', stringsAsFactors =FALSE) %>% 
  mutate(Value = as.numeric(str_remove_all(Value, ',')))

cornYield <- read.csv('../../data/raw/corn-soybean-usda/yield-corn.csv', stringsAsFactors = FALSE)
soybeanYield <- read.csv('../../data/raw/corn-soybean-usda/yield-soybean.csv', stringsAsFactors = FALSE)

allData <- bind_rows(cornPlanted, soybeanPlanted, cornPrice, soybeanPrice, 
                     cornProduction, soybeanProduction, cornYield, soybeanYield) %>% 
  filter(Period == 'YEAR') %>% 
  mutate(
    Data.Item = ifelse(Data.Item == 'CORN - ACRES PLANTED', 'CornPlanted', Data.Item),
    Data.Item = ifelse(Data.Item == 'SOYBEANS - ACRES PLANTED', 'SoybeanPlanted', Data.Item),
    Data.Item = ifelse(Data.Item == 'CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU', 'CornPrice', Data.Item),
    Data.Item = ifelse(Data.Item == 'SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU', 'SoybeanPrice', Data.Item),
    Data.Item = ifelse(Data.Item == 'CORN, GRAIN - PRODUCTION, MEASURED IN BU', 'CornProduction', Data.Item),
    Data.Item = ifelse(Data.Item == 'SOYBEANS - PRODUCTION, MEASURED IN BU', 'SoybeanProduction', Data.Item),
    Data.Item = ifelse(Data.Item == 'CORN, GRAIN - YIELD, MEASURED IN BU / ACRE', 'CornYield', Data.Item),
    Data.Item = ifelse(Data.Item == 'SOYBEANS - YIELD, MEASURED IN BU / ACRE', 'SoybeanYield', Data.Item)
  ) %>% 
  select(Year, Data.Item, Value) %>% 
  spread(Data.Item, Value) %>% 
  left_join(gdpDeflator, 'Year') %>% 
  mutate(
    SoybeanRealPrice = SoybeanPrice / Deflator,
    CornRealPrice = CornPrice / Deflator
  ) %>% 
  select(-Deflator) %>% 
  filter(Year >= 1960, Year <= 2018)

write.csv(allData, '../../data/processed/misc-corn-soybean.csv', row.names = FALSE)
  