library(tidyverse)
library(gridExtra)

yields <- read.csv('../../data/processed/yields-by-country.csv', stringsAsFactors = FALSE)

allYearsCountyList <- yields %>% 
  count(State, County) %>% 
  filter(n == 69) %>% 
  select(State, County)

subYields <- yields %>% 
  inner_join(allYearsCountyList, c('State', 'County'))

residualDf <- data.frame()
for(i in 1:nrow(allYearsCountyList)) {
  currState <- allYearsCountyList[[i, 'State']]
  currCounty <- allYearsCountyList[[i, 'County']]
  currYields <- yields %>% 
    filter(State == currState, County == currCounty)
  
  prevModel <- lm(Yield~1, data=currYields)
  models <- list()
  adjRSqs <- c()
  for(i in 1:3) {
    models[[i]] <- lm(Yield~poly(Year, i), data=currYields)
    adjRSqs <- c(adjRSqs, summary(models[[i]])$adj.r.squared)
  }
  
  bestModelIndex <- which.max(adjRSqs)
  bestModel <- models[[bestModelIndex]]
  
  currYields$ModelResidual <- bestModel$residuals / bestModel$fitted.values
  
  residualDf <- bind_rows(residualDf, currYields)
}

