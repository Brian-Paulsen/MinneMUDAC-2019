# This script cleans US soybean future indices
  # Datasource: https://www.investing.com/commodities/us-soybeans-historical-data
#
# Author(s): Brian
#
# Created: 2019-10-08
# Last Updated: 2019-10-09

library(tidyverse)
library(janitor)

gdpDeflator <- read.csv('../../data/processed/gdp-deflactor.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(Date))

usSoybeanFutures <- read.csv('../../data/raw/US Soybeans Futures Historical Data.csv',
                             stringsAsFactors = FALSE) %>% 
  mutate(
    Close = as.numeric(gsub(',', '', Price)),
    Open = as.numeric(gsub(',', '', Open)),
    High = as.numeric(gsub(',', '', High)),
    Low = as.numeric(gsub(',', '', Low)),
    Date = as.Date(Date, '%b %d, %Y')
  ) %>% 
  select(Date, Open, High, Low, Close) %>% 
  left_join(gdpDeflator, 'Date') %>% 
  mutate(
    CloseReal = Close / GDPDef,
    OpenReal = Open / GDPDef,
    HighReal = High / GDPDef,
    LowReal = Low / GDPDef
  ) %>% 
  select(-GDPDef)

write.csv(usSoybeanFutures, '../../data/processed/us-soybean-futures.csv', row.names=FALSE)
