# This script reads in the initial contract data for March 2020, May 2020, and July 2020, and
# combines it into one data set.
#
# Author(s): Brian
#
# Created: 2019-10-04
# Last Updated: 2019-10-08

library(xlsx)
library(tidyverse)


contractMarch <- read.xlsx('../../data/raw/ActiveSoybeanContractsForMarch2020.CSV.xlsx', 1,
                          stringsAsFactors = FALSE, startRow=4) %>% 
  filter(!is.na(Date)) %>% 
  rename(
    MarchOpen = Open,
    MarchHigh = High,
    MarchLow = Low,
    MarchClose = Close
  )

contractMay <- read.xlsx('../../data/raw/ActiveSoybeanContractsForMay2020.CSV.xlsx', 1,
                          stringsAsFactors = FALSE, startRow=4) %>% 
  filter(!is.na(Date)) %>% 
  rename(
    MayOpen = Open,
    MayHigh = High,
    MayLow = Low,
    MayClose = Close
  )

contractJuly <- read.xlsx('../../data/raw/ActiveSoybeanContractsforJuly2020.CSV.xlsx', 1,
                          stringsAsFactors = FALSE, startRow=4) %>% 
  filter(!is.na(Date)) %>% 
  rename(
    JulyOpen = Open,
    JulyHigh = High,
    JulyLow = Low,
    JulyClose = Close
  )
  
allData <- contractMarch %>% 
  full_join(contractMay, 'Date') %>% 
  full_join(contractJuly, 'Date') %>% 
  arrange(Date)

write.csv(allData, '../../data/processed/contract-price.csv', row.names = FALSE)
