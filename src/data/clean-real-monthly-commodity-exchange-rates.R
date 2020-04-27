# This script cleans the monthly commodity data, and breaks it into 3 parts
#
# Author(s): Brian
#
# Created: 2019-10-06
# Last Updated: 2019-10-08

library(tidyverse)
library(xlsx)
library(janitor)

commodities <- read.xlsx('../../data/raw/realmonthlycommodityexchangerates_1_.xls', 1,
                         startRow=12) %>% 
  mutate(
    Date = as.Date(paste0(Year, '-', Month, '-01'))
  )

usMarkets <- commodities %>% 
  select(c(79, 2, 1, 3:29)) %>% 
  clean_names(case = "upper_camel")

usCompetitors <- commodities %>% 
  select(c(79, 2, 1, 31:55)) %>% 
  clean_names(case = "upper_camel")
  
usSuppliers <- commodities %>% 
  select(c(79, 2, 1, 57:78)) %>% 
  clean_names(case = "upper_camel")

write.csv(usMarkets, '../../data/processed/us-market-commodity-exchange-rates.csv', row.names = FALSE)
write.csv(usCompetitors, '../../data/processed/us-competitor-commodity-exchange-rates.csv', row.names = FALSE)
write.csv(usSuppliers, '../../data/processed/us-supplier-commodity-exchange-rates.csv', row.names=FALSE)
