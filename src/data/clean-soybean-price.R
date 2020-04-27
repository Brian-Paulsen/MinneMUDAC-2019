# A script for cleaning the US soybean price
#
# Author(s): Brian
#
# Created: 2019-10-11
# Last Updated: 2019-10-11

library(tidyverse)
library(xlsx)


gdpDeflator <- read.csv('../../data/processed/gdp-deflactor.csv', stringsAsFactors = FALSE)

read.csv('../../data/raw/soybean-prices-historical-chart-data.csv', stringsAsFactors = FALSE) %>% 
  slice(-(1:15)) %>% 
  rename(
    Date = Macrotrends.Data.Download,
    Price = X
  ) %>% 
  left_join(gdpDeflator, 'Date') %>% 
  mutate(
    Date = as.Date(Date),
    Price = as.numeric(Price),
    RealPrice = Price / GDPDef
  ) %>% 
  select(-GDPDef) %>% 
  write.csv('../../data/processed/us-soybean-price.csv', row.names=FALSE)
  
