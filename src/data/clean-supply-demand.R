# This script processes Soybeans_historicalstocks.xlsx and oil crops outlook_August19_relevanttables.xlsx.
# It outputs data by year, quarter, and month
#
# Author(s): Brian
#
# Date Created: 2019-10-10
# Last Updated: 2019-10-10  


library(tidyverse)
library(xlsx)


monthToNum <- function(month) {
  which(tolower(month) == tolower(month.name))
}


#### Historical Stocks ####


tab1 <- read.xlsx('../../data/raw/Soybeans_historicalstocks.xlsx', 3,
                  stringsAsFactors = FALSE, colIndex = 1:4,
                  startRow = 2, endRow = 100) %>% 
  slice(-1) %>% 
  mutate(
    OnFarmSoybeanStocks = 1000 * as.numeric(On.farm),
    OffFarmSoybeanStocks = 1000 * as.numeric(Off.farm),
    ProducedSoybeanStocks = OnFarmSoybeanStocks + OffFarmSoybeanStocks
  ) %>% 
  select(Date, OnFarmSoybeanStocks, OffFarmSoybeanStocks, ProducedSoybeanStocks)

soybeanStocks <- data.frame()
for(i in seq(1, nrow(tab1), 5)) {
  yearStr <- tab1[i,1]
  year1 <- as.numeric(str_sub(yearStr, 1, 4))
  year2 <- as.numeric(str_sub(yearStr, 6, 7)) + 2000
  subTab <- tab1[(i+1):(i+4),] %>% 
    rename(Month = Date) %>% 
    mutate(
      Month = as.numeric(sapply(str_sub(Month, start = 3, end=-3), monthToNum)),
      Year = c(year1, rep(year2, 3))
    ) %>% 
    select(Year, Month, OffFarmSoybeanStocks, OnFarmSoybeanStocks, ProducedSoybeanStocks) %>% 
    filter(!is.na(OffFarmSoybeanStocks)) # To deal with last table
  soybeanStocks <- bind_rows(soybeanStocks, subTab)
}



tab2 <- read.xlsx('../../data/raw/Soybeans_historicalstocks.xlsx', 4,
                  stringsAsFactors = FALSE, colIndex = 1:6,
                  startRow = 2, endRow = 64) %>% 
  slice(-(1:3)) %>% 
  mutate(
    Year = as.numeric(Year),
    SoybeanPlanted = 1000 * as.numeric(Planted),
    SoybeanHarvested = 1000 * as.numeric(Harvested),
    SoybeanProduction = 1000 * as.numeric(Production),
    SoybeanValue = 1000 * as.numeric(Value)
  ) %>% 
  select(Year, SoybeanPlanted, SoybeanHarvested, SoybeanProduction, SoybeanValue)

tab3 <- read.xlsx('../../data/raw/Soybeans_historicalstocks.xlsx', 5,
                  stringsAsFactors = FALSE, colIndex = 1:12,
                  startRow = 7, endRow = 46) %>% 
  transmute(
    Year = NA.,
    SoybeanBeginStocks = NA..1 * 10^6,
    SoybeanProduction = NA..2 * 10^6,
    SoybeanImports = NA..3 * 10^6,
    SoybeanTotalQuanity = NA..4 * 10^6,
    SoybeanCrush = NA..6 * 10^6,
    SoybeanExports = NA..7 * 10^6,
    SoybeanSeedsFeedResidual = NA..8 * 10^6,
    SoybeanUsed = NA..9 * 10^6,
    SoybeanEndingStocks = NA..10 * 10^6,
    SoybeanPrice = NA..11
  ) %>% 
  mutate(
    Year = ifelse(Year == '2018 2/', 2018, Year),
    Year = as.numeric(ifelse(Year == '2017 2/', 2017, Year)),
    SoybeanPrice = as.numeric(ifelse(SoybeanPrice == '8.10-9.10', 8.60, SoybeanPrice)),
  )

tab4 <- read.xlsx('../../data/raw/Soybeans_historicalstocks.xlsx', 6,
                  stringsAsFactors = FALSE, colIndex = 1:11,
                  startRow = 7, endRow = 46) %>% 
  transmute(
    Year = NA.,
    SoybeanMealBeginStocks = NA..1 * 1000,
    SoybeanMealProduction = NA..2 * 1000,
    SoybeanMealImports = NA..3 * 1000,
    SoybeanMealTotalQuanity = NA..4 * 1000,
    SoybeanMealDomesticConsumption = NA..6 * 1000,
    SoybeanMealExports = NA..7 * 1000,
    SoybeanMealTotalUsed = NA..8 * 1000,
    SoybeanMealEndingStocks = NA..9 * 1000,
    SoybeanMealPrice = NA..10
  ) %>% 
  mutate(
    Year = ifelse(Year == '2018  2/', 2018, Year),
    Year = as.numeric(ifelse(Year == '2017  2/', 2017, Year)),
    SoybeanMealPrice = as.numeric(ifelse(SoybeanMealPrice == '295-335', 310, SoybeanMealPrice))
  )

tab5 <- read.xlsx('../../data/raw/Soybeans_historicalstocks.xlsx', 7,
                  stringsAsFactors = FALSE,
                  startRow = 6, endRow = 45) %>% 
  transmute(
    Year = NA.,
    SoybeanOilBeginStocks = NA..1 * 10^6,
    SoybeanOilProduction = NA..2 * 10^6,
    SoybeanOilImports = NA..3 * 10^6,
    SoybeanOilTotalQuanity = NA..4 * 10^6,
    SoybeanOilDomesticUsed = NA..6 * 10^6,
    MethylEsterProduced = NA..7,
    SoybeanOilExports = NA..8 * 10^6,
    SoybeanOilTotalUsed = NA..9 * 10^6,
    SoybeanOilEndingStocks = NA..10 * 10^6,
    SoybeanOilPrice = NA..11
  ) %>% 
  mutate(
    Year = ifelse(Year == '2018  1/', 2018, Year),
    Year = as.numeric(ifelse(Year == '2017  1/', 2017, Year)),
    SoybeanOilPrice = as.numeric(ifelse(SoybeanOilPrice == '28.5-31.5', 28, SoybeanOilPrice))
  ) %>% 
  mutate(
    MethylEsterProduced = as.numeric(ifelse(MethylEsterProduced == '---', 0, MethylEsterProduced)) * 10^6
  )

tab6 <- read.xlsx('../../data/raw/Soybeans_historicalstocks.xlsx', 8,
                  stringsAsFactors = FALSE, colIndex = 1:10,
                  startRow = 6, endRow = 173) %>% 
  transmute(Year = NA.,
         SoybeanBeginStocks = NA..1 * 1000,
         SoybeanProduction = NA..2,
         SoybeanImports = NA..3 * 1000,
         SoybeanTotalQuanity = NA..4 * 1000,
         SoybeanCrush = as.numeric(X1.000.bushels) * 1000,
         SoybeanExports = NA..5 * 1000,
         SoybeanSeedsFeedResidual = as.numeric(NA..6) * 1000,
         SoybeanTotalUsed = NA..7 * 1000,
         SoybeanEndStocks = NA..8 * 1000
         )

quarterlySoybeanSupply <- data.frame()
monthlySoybeanSupply <- data.frame()
for(i in seq(1, 105, 7)) {
  yearStr <- tab6[i,1]
  year1 <- as.numeric(str_sub(yearStr, end=4))
  year2 <- as.numeric(str_sub(yearStr, 6, 7)) + 2000
  subTab <- tab6[(i+1):(i+4),] %>% 
    mutate(
      Year = c(year1, year1, year2, year2),
      Month = c(9, 12, 3, 6),
      SoybeanProduction = as.numeric(ifelse(SoybeanProduction == '---', 0, SoybeanProduction)) * 1000
    )
  quarterlySoybeanSupply <- bind_rows(quarterlySoybeanSupply, subTab)
}

for(i in seq(106, nrow(tab6), 19)) {
  yearStr <- tab6[i, 1]
  year1 <- as.numeric(str_sub(yearStr, end=4))
  year2 <- as.numeric(str_sub(yearStr, 6, 7)) + 2000
  fullSubTab <- tab6[(i+1):(i+16),]
  quarterlySubTab <- fullSubTab %>% 
    filter(grepl('-', Year)) %>% 
  mutate(
    Year = c(year1, year1, year2, year2)[1:n()],
    Month = c(9, 12, 3, 6)[1:n()],
    SoybeanProduction = as.numeric(ifelse(SoybeanProduction == '---', 0, SoybeanProduction))
  )
  monthlySubTab <- fullSubTab %>% 
    filter(!grepl('-', Year), !is.na(Year)) %>% 
    mutate(
      Month = sapply(trimws(Year), monthToNum),
      Year = c(rep(year1, 4), rep(year2, 8))[1:n()]
    ) %>% 
    select(Year, Month, SoybeanImports, SoybeanCrush, SoybeanExports)
  
  quarterlySoybeanSupply <- bind_rows(quarterlySoybeanSupply, quarterlySubTab)
  monthlySoybeanSupply <- bind_rows(monthlySoybeanSupply, monthlySubTab)
}


tab7 <- read.xlsx('../../data/raw/Soybeans_historicalstocks.xlsx', 9,
                  stringsAsFactors = FALSE,
                  startRow = 6, endRow = 104) %>% 
  transmute(Year = NA.,
         SoybeanMealBeginStocks = NA..1 * 1000,
         SoybeanMealProduction = NA..2 * 1000,
         SoybeanMealImports = NA..3 * 1000,
         SoybeanMealTotalQuanity = X1.000.short.tons * 1000,
         SoybeanMealDomesticUsed = NA..5 * 1000,
         SoybeanMealExports = NA..6 * 1000,
         SoybeanMealTotalUsed = NA..7 * 1000,
         SoybeanMealEndStocks = NA..8 * 1000
  )

monthlyMealSupply <- data.frame()
for(i in seq(1, nrow(tab7), 14)) {
  yearstr <- tab7[i, 1]
  year1 <- as.numeric(str_sub(yearstr, 1, 4))
  year2 <- as.numeric(str_sub(yearstr, 6, 7)) + 2000
  
  subTab <- tab7[(i+1):(i+12),] %>% 
    mutate(
      Month = sapply(trimws(Year), monthToNum),
      Year = c(rep(year1, 3), rep(year2, 9))
    )
  
  monthlyMealSupply <- bind_rows(monthlyMealSupply, subTab)
}


tab8 <- read.xlsx('../../data/raw/Soybeans_historicalstocks.xlsx', 10,
                  stringsAsFactors = FALSE,
                  startRow = 6, endRow = 104) %>% 
  transmute(
    Year = NA.,
    SoyOilBeginStocks = NA..1 * 1000,
    SoyOilProduction = NA..2 * 1000,
    SoyOilImports = NA..3 * 1000,
    SoyOilTotalQuanity = NA..4 * 1000,
    SoyOilDomesticUsed = X1.000.pounds * 1000,
    MethylEster = NA..6 * 1000,
    SoyOilExports = NA..7 * 1000,
    SoyOilTotalUsed = NA..8 * 1000,
    SoyOilEndStocks = NA..9 * 1000
  )

monthlySoyOilSupply <- data.frame()
for(i in seq(1, nrow(tab8), 14)) {
  yearStr <- tab8[i, 1]
  year1 <- as.numeric(str_sub(yearStr, 1, 4))
  year2 <- as.numeric(str_sub(yearStr, 6, 7)) + 2000
  
  subTab <- tab8[(i+1):(i+12),] %>% 
    mutate(
      Month = sapply(trimws(Year), monthToNum),
      Year = c(rep(year1, 3), rep(year2, 9))
    )
  
  monthlySoyOilSupply <- bind_rows(monthlySoyOilSupply, subTab)
}

tab9 <- read.xlsx('../../data/raw/Soybeans_historicalstocks.xlsx', 11,
                  stringsAsFactors = FALSE, colIndex = 1:15,
                  startRow = 8, endRow = 137) %>% 
  transmute(
    Year = NA.,
    OilYield = NA..1,
    OilPrice = NA..2 / 100,
    OilValue = NA..3,
    MealYield = NA..4,
    MealPrice = NA..5,
    MealValue = NA..6,
    HullYield = NA..7,
    HullPrice = NA..8,
    HullValue = NA..9,
    TotalValue = NA..10,
    OilValueProp = NA..11,
    MealHullValueProp = NA..12,
    Processor = NA..13,
    ValuePriceSpread = NA..14
  )

yearlyProductivity <- tab9 %>% 
  slice(1:16) %>% 
  mutate(
    HullYield = as.numeric(ifelse(HullYield == '---', 0, HullYield)),
    HullPrice = as.numeric(ifelse(HullPrice == '---', 0, HullPrice)),
    HullValue = as.numeric(ifelse(HullValue == '---', 0, HullValue)),
    Year = as.numeric(str_sub(Year, 1, 4))
  )

monthlyProductivity <- data.frame()
for(i in seq(18, nrow(tab9), 14)) {
  yearStr <- tab9[i, 1]
  year1 <- as.numeric(str_sub(yearStr, 1, 4))
  year2 <- as.numeric(str_sub(yearStr, 6, 7)) + 2000
  
  subMonth <- tab9[(i+1):(i+12),] %>% 
    mutate(
      Month = sapply(trimws(Year), monthToNum),
      Year = c(rep(year1, 4), rep(year2, 8)),
      HullYield = as.numeric(HullYield),
      HullPrice = as.numeric(HullPrice),
      HullValue = as.numeric(HullValue)
    )
  
  subYear <- tab9[i+13,] %>% 
    mutate(
      Year = year1,
      HullYield = as.numeric(HullYield),
      HullPrice = as.numeric(HullPrice),
      HullValue = as.numeric(HullValue)
    )
  
  
  monthlyProductivity <- bind_rows(monthlyProductivity, subMonth)
  yearlyProductivity <- bind_rows(yearlyProductivity, subYear)
}

#### Oil Crops Outlook ####


table8Outlook <- read.xlsx('../../data/raw/oil crops outlook_August19_relevanttables.xlsx', 5,
                    stringsAsFactors = FALSE, colIndex = 1:7,
                    startRow = 5, endRow = 42) %>% 
  transmute(
    Year = NA.,
    SoybeanSeedPrice = NA..1,
    CottenseedSeedPrice = NA..2,
    SunflowerSeedPrice = NA..3,
    CanolaSeedPrice = NA..4,
    PeanutSeedPrice = NA..5 / 100,
    FlaxseedSeedPrice = NA..6
  )

yearlySeedPrice <- table8Outlook %>% 
  slice(1:11) %>% 
  mutate(
    Year = as.numeric(str_sub(Year, 1, 4))
  )

seedPrice2017 <- table8Outlook %>% 
  slice(14:25) %>% 
  mutate(
    Month = sapply(trimws(Year), monthToNum),
    Year = c(rep(2017, 4), rep(2018, 8))
  )

seedPrice2018 <- table8Outlook %>% 
  slice(28:37) %>% 
  mutate(
    Month = sapply(trimws(Year), monthToNum),
    Year = c(rep(2018, 4), rep(2019, 6))
  )

monthlySeedPrice <- bind_rows(seedPrice2017, seedPrice2018)

table9Outlook <- read.xlsx('../../data/raw/oil crops outlook_August19_relevanttables.xlsx', 6,
                    stringsAsFactors = FALSE, colIndex = 1:9,
                    startRow = 5, endRow = 42) %>% 
  rename(
    Year = NA.,
    SoybeanOilPrice = NA..1,
    CottonseedOilPrice = NA..2,
    SunflowerSeedOilPrice = NA..3,
    CanolaSeedOilPrice = NA..4,
    PeanutOilPrice = NA..5,
    CornOilPrice = NA..6,
    LardPrice = NA..7,
    EdibleTallowPrice = NA..8
  )
  
yearlyOilPrice <- table9Outlook %>% 
  slice(1:11) %>% 
  mutate(Year = as.numeric(str_sub(Year, 1, 4)))

oilPrice2017 <- table9Outlook %>% 
  slice(14:25) %>% 
  mutate(
    Month = sapply(trimws(Year), monthToNum),
    Year = c(rep(2017, 3), rep(2018, 9))
  )

oilPrice2018 <- table9Outlook %>% 
  slice(28:37) %>% 
  mutate(
    Month = sapply(trimws(Year), monthToNum),
    Year = c(rep(2018, 3), rep(2019, 7))
  )

monthlyOilPrice <- bind_rows(oilPrice2017, oilPrice2018)

table10Outlook <- read.xlsx('../../data/raw/oil crops outlook_August19_relevanttables.xlsx', 7,
                     stringsAsFactors = FALSE, colIndex = 1:7,
                     startRow = 5, endRow = 42) %>% 
  rename(
    Year = NA.,
    SoybeanMealPrice = NA..1,
    CottonseedMealPrice = NA..2,
    SunflowerseedMealPrice = NA..3,
    PeanutMealPrice = NA..4,
    CanolaMealPrice = NA..5,
    LinseedMealPrice = NA..6
  ) %>% 
  select(-PeanutMealPrice)

yearlyMealPrice <- table10Outlook %>% 
  slice(1:11) %>% 
  mutate(
    Year = as.numeric(str_sub(Year, 1, 4))
  )

mealPrice2017 <- table10Outlook %>% 
  slice(14:25) %>% 
  mutate(
    Month = sapply(trimws(Year), monthToNum),
    Year = c(rep(2017, 3), rep(2018, 9))
  )

mealPrice2018 <- table10Outlook %>% 
  slice(28:37) %>% 
  mutate(
    Month = sapply(trimws(Year), monthToNum),
    Year = c(rep(2018, 3), rep(2019, 7))
  )

monthlyMealPrice <- bind_rows(mealPrice2017, mealPrice2018)


#### Join datasets ####

yearlyDf <- tab2 %>% 
  select(-SoybeanProduction) %>% # in tab3, don't duplicate
  full_join(tab3, 'Year') %>% 
  full_join(tab4, 'Year') %>% 
  full_join(tab5, 'Year') %>% 
  full_join(yearlyProductivity, 'Year') %>% 
  full_join(yearlySeedPrice, 'Year') %>% 
  full_join(yearlyOilPrice, 'Year') %>% 
  full_join(yearlyMealPrice, 'Year') %>% 
  select(-SoybeanMealPrice.y, -SoybeanOilPrice.y) %>% 
  rename(
    SoybeanMealPrice = SoybeanMealPrice.x, 
    SoybeanOilPrice = SoybeanOilPrice.x
  ) %>% 
  arrange(Year)

quarterlyDf <- soybeanStocks %>% 
  full_join(quarterlySoybeanSupply, c('Year', 'Month')) %>% 
  arrange(Year, Month)

monthlyDf <- monthlySoybeanSupply %>% 
  full_join(monthlyMealSupply, c('Year', 'Month')) %>% 
  full_join(monthlySoyOilSupply, c('Year', 'Month')) %>% 
  full_join(monthlyProductivity, c('Year', 'Month')) %>% 
  full_join(monthlySeedPrice, c('Year', 'Month')) %>% 
  full_join(monthlyOilPrice, c('Year', 'Month')) %>% 
  full_join(monthlyMealPrice, c('Year', 'Month')) %>% 
  arrange(Year, Month)


#### Write data to files ####


write.csv(yearlyDf, '../../data/processed/yearly-supply-demand.csv', row.names = FALSE)
write.csv(quarterlyDf, '../../data/processed/quarterly-supply-demand.csv', row.names = FALSE)
write.csv(monthlyDf, '../../data/processed/monthly-supply-demand.csv', row.names = FALSE)
