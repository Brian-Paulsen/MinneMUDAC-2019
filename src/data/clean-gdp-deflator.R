# This script cleans the GDP deflactor data. The data is given quarterly, so it fills in the data
# daily, so we can properly adjust daily data. It assumes a linear increase from one quarter to the
# next.
#
# The data only goes to April, so a regression model on the log transformed data after 2010 is used.
#
# Author(s): Brian
#
# Created: 2019-10-09
# Last Updated: 2019-10-09


library(tidyverse)

gdpDeflator <- read.csv('../../data/raw/GDPDEF.csv', stringsAsFactors = FALSE) %>% 
  mutate(DATE = as.Date(DATE))



fillInDateRange <- function(startDate, endDate, startValue, endValue) {
  if(is.na(endDate) | is.na(startDate)) {
    return(data.frame())
  }

  dates <- seq(startDate, endDate, by='days')
  newValues <- ((length(dates)-1):0 * startValue + 0:(length(dates)-1) * endValue) / 
    (length(dates) - 1)
  
  df <- data.frame(
    Date = dates,
    Value = newValues
  ) %>% 
    slice(1:(n()-1))
  return(df)
}


filledIn <- mapply(fillInDateRange, 
       gdpDeflator$DATE, lead(gdpDeflator$DATE), 
       gdpDeflator$GDPDEF, lead(gdpDeflator$GDPDEF)) %>% 
  bind_rows()
  
centeredDf <- filledIn %>% 
  mutate(
    GDPDef = Value / 100
  ) %>% 
  select(Date, GDPDef)

smallDf <- centeredDf %>% 
  mutate(LogDef = log(GDPDef)) %>% 
  filter(Date >= '2010-01-01') %>% 
  mutate(DaysAfter = row_number())

# slope: 0.00004486077
coef(lm(LogDef~DaysAfter, data=smallDf))

lastLogValue <- log(gdpDeflator$GDPDEF[nrow(gdpDeflator)] / 100)
startDate <- gdpDeflator$DATE[nrow(gdpDeflator)]
finishDate <- as.Date('2019-12-01')
days <- as.numeric(finishDate - startDate)

newLogValues <- lastLogValue + 0.00004486077 * 0:days
newValues <- exp(newLogValues)

projectedDf <- data.frame(
  Date = seq(startDate, finishDate, by='days'),
  GDPDef = newValues
)

finalDf <- bind_rows(centeredDf, projectedDf)

finalDf %>% 
  filter(Date >= '2010-01-01') %>% 
ggplot() +
  geom_line(aes(Date, GDPDef))

write.csv(finalDf, '../../data/processed/gdp-deflactor.csv', row.names = FALSE)

