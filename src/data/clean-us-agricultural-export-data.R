# This script cleans the top agricultural exports data set (fytop25hvpexp.xls)
#
# Author(s): Brian
#
# Created: 2019-10-09
# Last Updated: 2019-10-09

    
library(tidyverse)
library(xlsx)

rawDf <- read.xlsx('../../data/raw/fytop25hvpexp.xls', 1, stringsAsFactors=FALSE, startRow=2, endRow=27)

dfs <- as.list(rep(NA, ncol(rawDf) / 4))
for(i in seq(1, ncol(rawDf) - 1, 4)) {
  newDf <- rawDf %>% 
    select(i:(i+2))
  dfs[[(i+3) / 4]] <- newDf
}

fullDf <- data.frame()
for(df in dfs) {
  year <- as.numeric(str_sub(colnames(df)[[3]], 2, 5))
  commodity <- df[[1]]
  processType <- df[[2]]
  amount <- df[[3]]
  newDf <- data.frame(
    Year = year,
    Commodity = commodity,
    LevelOfProcessing = processType,
    Amount = amount
  )
  fullDf <- bind_rows(fullDf, newDf)
}

write.csv(fullDf, '../../data/processed/top-agricultural-exports.csv', row.names = FALSE)