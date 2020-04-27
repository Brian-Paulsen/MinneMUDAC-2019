library(tidyverse)


gdpDeflator <- read.csv('../../data/processed/gdp-deflactor.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(Date))

wheat <- read.csv('../../data/raw/commdity-futures/US Wheat Futures Historical Data.csv',
                  stringsAsFactors = FALSE) %>% 
  select(
    Date,
    WheatPrice = Price
  )

soybeanOil <- read.csv('../../data/raw/commdity-futures/US Soybean Oil Futures Historical Data.csv',
                       stringsAsFactors = FALSE) %>% 
  select(
    Date,
    SoybeanOilPrice = Price
  )

soybeanMeal <- read.csv('../../data/raw/commdity-futures/US Soybean Meal Futures Historical Data.csv',
                        stringsAsFactors = FALSE) %>% 
  select(
    Date,
    SoybeanMealPrice = Price
  )

oilPrice <- read.csv('../../data/raw/commdity-futures/STOXX North America 600 Oil & Gas USD Price Historical Data.csv',
                     stringsAsFactors = FALSE) %>% 
  select(
    Date,
    OilPrice = Price
  )

corn <- read.csv('../../data/raw/commdity-futures/US Corn Futures Historical Data.csv',
             stringsAsFactors = FALSE) %>% 
  select(
    Date,
    CornPrice = Price
  )

canola <- read.csv('../../data/raw/commdity-futures/Canola Futures Historical Data.csv',
         stringsAsFactors = FALSE) %>% 
  select(
    Date,
    CanolaPrice = Price
  )

soybean <- read.csv('../../data/raw/US Soybeans Futures Historical Data.csv', 
                    stringsAsFactors = FALSE) %>% 
  select(
    Date,
    SoybeanPrice = Price
  )

allCommodities <- soybeanOil %>% 
  full_join(soybeanMeal, 'Date') %>% 
  full_join(corn, 'Date') %>% 
  full_join(canola, 'Date') %>% 
  full_join(wheat, 'Date') %>% 
  full_join(oilPrice, 'Date') %>% 
  full_join(soybean, 'Date') %>%
  mutate(
    Date = as.Date(Date, '%b %d, %Y'),
    Day = weekdays(Date),
    WheatPrice = as.numeric(gsub(",", "", WheatPrice)),
    SoybeanPrice = as.numeric(gsub(",", "", SoybeanPrice))
  ) %>% 
  filter(Day != 'Sunday', Day != 'Saturday') %>% 
  select(-Day) %>% 
  filter(!is.na(SoybeanPrice) | Date >= '2019-09-03') %>% 
  arrange(Date) %>% 
  left_join(gdpDeflator, 'Date') %>% 
  mutate(
    SoybeanOilRealPrice = SoybeanOilPrice / GDPDef,
    SoybeanMealRealPrice = SoybeanMealPrice / GDPDef,
    CornRealPrice = CornPrice / GDPDef,
    CanolaRealPrice = CanolaPrice / GDPDef,
    WheatRealPrice = WheatPrice / GDPDef,
    OilRealPrice = OilPrice / GDPDef,
    SoybeanRealPrice = SoybeanPrice / GDPDef,
    SoybeanOilDiff = SoybeanOilRealPrice - lag(SoybeanOilRealPrice),
    SoybeanMealDiff = SoybeanMealRealPrice - lag(SoybeanOilRealPrice),
    CornDiff = CornRealPrice - lag(CornRealPrice),
    CanolaDiff = CanolaRealPrice - lag(CanolaRealPrice),
    WheatDiff = WheatRealPrice - lag(WheatRealPrice),
    OilDiff = OilRealPrice - lag(OilPrice),
    SoybeanDiff = SoybeanRealPrice - lag(SoybeanRealPrice),
    SoybeanOilGrowth = SoybeanOilDiff / lag(SoybeanOilRealPrice),
    SoybeanMealGrowth = SoybeanMealDiff / lag(SoybeanMealRealPrice),
    CornGrowth = CornDiff / lag(CornRealPrice),
    CanolaGrowth = CanolaDiff / lag(CanolaRealPrice),
    WheatGrowth = WheatDiff / lag(WheatRealPrice),
    OilGrowth = OilDiff / lag(OilRealPrice),
    SoybeanGrowth = SoybeanDiff / lag(SoybeanRealPrice)
  )

write.csv(allCommodities, '../../data/processed/commodities.csv', row.names=FALSE)