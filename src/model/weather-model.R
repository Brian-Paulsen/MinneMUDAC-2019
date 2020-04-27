# Ideas for feature engineering:
# k-day max during growing season
# rain frequency (number of days with rain)

library(tidyverse)
library(janitor)
library(lme4)
library(lubridate)
library(R2OpenBUGS)
library(openintro)

dailyWeather <- read.csv('../../data/processed/weather-counties-2007-2019.csv', stringsAsFactors = FALSE)
yields <- read.csv('../../data/processed/yields-by-country.csv', stringsAsFactors = FALSE)
fipsCodes <- read.csv('../../data/raw/state_and_county_fips_master.csv') %>% 
  filter(!is.na(state)) %>% 
  mutate(
    County = toupper(str_sub(name, 1, -8)),
    State = toupper(abbr2state(state)),
    FullCounty = paste0(County, ', ', State),
    Fips = fips
  ) %>% 
  select(FullCounty, Fips)

stationMonthly<- dailyWeather %>%
  transmute(
      Date=ymd(Date),
      Station=Station,
      County=County,
      Prcp=Prcp
    ) %>%
  mutate(
    Year=year(Date),
    Month=month(Date),
    Prcp=replace_na(Prcp,replace=0)
  ) %>%
  group_by(Year, Month, County, Station) %>%
  summarise(
    PrcpAvg=mean(Prcp),
    PrcpSum=sum(Prcp),
    Freqency = sum(Prcp > 0)
  ) %>% 
  ungroup()

rainMonthly<- stationMonthly %>%
  group_by(Year, Month, County) %>%
  summarise(
    PrcpSum=mean(PrcpSum),
    PrcpAvg=mean(PrcpAvg),
    PrcpFrequency = mean(Freqency)
  )

preSpreadData <- rainMonthly %>%
  separate(County, c("State", "County"), ",") %>%
  select(-PrcpAvg) %>% 
  mutate(
    State=toupper(State),
    County=toupper(County)
  ) %>% 
  unite(Year_County_State, Year, County, State) 

prcpSum <- preSpreadData %>% 
  select(-PrcpFrequency) %>% 
  spread(Month, PrcpSum) %>% 
  rename(
    JanPrcp=`1`, FebPrcp=`2`, MarPrcp=`3`, AprPrcp=`4`, MayPrcp=`5`, JunPrcp=`6`,
    JulPrcp=`7`, AugPrcp=`8`, SepPrcp=`9`, OctPrcp=`10`, NovPrcp=`11`, DecPrcp=`12`
  )

prcpFreq <- preSpreadData %>% 
  select(-PrcpSum) %>% 
  spread(Month, PrcpFrequency) %>% 
  rename(
    JanFreq=`1`, FebFreq=`2`, MarFreq=`3`, AprFreq=`4`, MayFreq=`5`, JunFreq=`6`,
    JulFreq=`7`, AugFreq=`8`, SepFreq=`9`, OctFreq=`10`, NovFreq=`11`, DecFreq=`12`
  )

rainMonthlyCln <- prcpSum %>% 
  left_join(prcpFreq, 'Year_County_State') %>% 
  separate(Year_County_State, c('Year', 'County', 'State'), '_') %>% 
  mutate(Year = as.numeric(Year))

rainMonthly2019 <- rainMonthlyCln %>% 
  filter(Year == 2019)

countyRainYield <- left_join(rainMonthlyCln, yields, by=c("Year", "County", "State")) %>% 
  filter(!is.na(Yield)) %>% 
  filter(!is.na(MarPrcp), !is.na(JulPrcp), !is.na(AugPrcp), !is.na(SepPrcp)) %>% 
  mutate(
    FullCounty = paste0(County, ', ', State)
  )

modelData <- countyRainYield %>% 
  distinct(FullCounty) %>% 
  mutate(
    CountyId = row_number()
  ) %>% 
  select(FullCounty, CountyId) %>% 
  left_join(countyRainYield, 'FullCounty')

stateAndCounties <- modelData %>% 
  distinct(CountyId, County, State) %>% 
  mutate(
    StateId = as.numeric(as.factor(State))
  )

summary(lm(Yield~poly(Year, 2)+AugPrcp+County, data=modelData))


avgPrcp <- rainMonthlyCln %>% 
  group_by(State, County) %>% 
  summarise(
    JulyAvg = mean(JulPrcp, na.rm=TRUE),
    AugAvg = mean(AugPrcp, na.rm=TRUE)
  ) %>% 
  mutate(FullCounty = paste0(County, ', ', State))


##### Bugs models #####

yieldMean <- mean(modelData$Yield)
yieldSd <- sd(modelData$Yield)

yearMean <- mean(modelData$Year)
yearSd <- sd(modelData$Year)

marMean <- mean(modelData$MarPrcp, na.rm=TRUE)
marSd <- sd(modelData$MarPrcp, na.rm=TRUE)
julMean <- mean(modelData$JulPrcp, na.rm=TRUE)
julSd <- sd(modelData$JulPrcp, na.rm=TRUE)
augMean <- mean(modelData$AugPrcp, na.rm=TRUE)
augSd <- sd(modelData$AugPrcp, na.rm=TRUE)
sepMean <- mean(modelData$SepPrcp, na.rm=TRUE)
sepSd <- sd(modelData$SepPrcp, na.rm=TRUE)


countyState <- stateAndCounties$StateId
n <- nrow(modelData)
J <- length(unique(modelData$CountyId))
K <- length(unique(countyState))
y <- (modelData$Yield - yieldMean) / yieldSd
county <- modelData$CountyId
year <- (modelData$Year - yearMean) / yearSd

mar <- (modelData$MarPrcp - marMean) / marSd
jul <- (modelData$JulPrcp - julMean) / julSd
aug <- (modelData$AugPrcp - augMean) / augSd
sep <- (modelData$SepPrcp - sepMean) / sepSd


### County mean model

data <- list('n', 'J', 'y', 'county')
inits <- function() {
  list(sigma.a = runif(1), sigma.y = runif(1), mu = rnorm(1))
}
parameters <- c('a', 'mu', 'sigma.y', 'sigma.a')
countyMean <- bugs(data, inits, parameters, 'county-mean-model.txt', working.directory='bugs-models/',
             n.chains=3, n.iter=500, bugs.seed=1)
plot(countyMean)
print(countyMean)

## Year model

data <- list('n', 'y', 'year')
inits <- function() {
  list(b0=rnorm(1), b1=rnorm(1), sigma.y=runif(1))
}
parameters <- c('b0', 'b1', 'sigma.y')
yearModel <- bugs(data, inits, parameters, 'year-model.txt', working.directory = 'bugs-models/',
                  n.chains=3, n.iter=500, bugs.seed=1)
plot(yearModel)
print(yearModel)

summary(lm(y~year))


## Year^2 model
data <- list('n', 'y', 'year')
inits <- function() {
  list(b0=rnorm(1), b1=rnorm(1), b2=rnorm(1), sigma.y=runif(1))
}
parameters <- c('b0', 'b1', 'b2', 'sigma.y')
yearSqModel <- bugs(data, inits, parameters, 'year-sq-model.txt', working.directory = 'bugs-models/',
                  n.chains=3, n.iter=500, bugs.seed=1)
plot(yearSqModel)
print(yearSqModel)

yearSq <- year^2
summary(lm(y~year+yearSq))


## Varying intercept model with year

data <- list('n', 'J', 'y', 'county', 'year')
inits <- function() {
  list(sigma.a=runif(1), sigma.y=runif(1), mu.a=rnorm(1), b1=rnorm(1), b2=rnorm(1))
}
parameters <- c('a', 'b1', 'b2', 'sigma.y', 'mu.a', 'sigma.a')
varyInterceptYear <- bugs(data, inits, parameters, 'year-vary-intercept-model.txt', 
                          working.directory = 'bugs-models/', n.chains=3, n.iter=500, bugs.seed=1)
plot(varyInterceptYear)
print(varyInterceptYear)

yearIntercepts <- varyInterceptYear$summary[1:827, 1]
yearCoef <- varyInterceptYear$summary[828, 1]
yearSqCoef <- varyInterceptYear$summary[829, 1]

yearAdd <- yearCoef * (2019 - yearMean) / yearSd + yearSqCoef * ((2019 - yearMean) / yearSd)^2
rawYearPredictions <- yearIntercepts + yearAdd
yearPredictions <- rawYearPredictions * yieldSd + yieldMean

## Varying slope model with year

data <- list('n', 'J', 'y', 'county', 'year')
inits <- function() {
  list(sigma.a=runif(1), sigma.b=runif(1), sigma.y=runif(1), mu.a=rnorm(1), mu.b=rnorm(1))
}
paramters <- c('a', 'b', 'sigma.y', 'mu.a', 'mu.b', 'sigma.a', 'sigma.b')
varySlopeYear <- bugs(data, inits, parameters, 'year-vary-slope-model.txt', working.directory = 'bugs-models/',
                      n.chains=3, n.iter=500, bugs.seed=1)
plot(varySlopeYear)
print(varySlopeYear)


## Varying slope model with aug
data <- list('n', 'J', 'y', 'county', 'year', 'aug')
inits <- function() {
  list(sigma.a=runif(1), sigma.b=runif(1), sigma.y=runif(1), mu.a=rnorm(1), mu.b=rnorm(1))
}
parameters <- c('a', 'b', 'c1', 'c2', 'sigma.y', 'mu.a', 'mu.b', 'sigma.a', 'sigma.b')
varySlopeAug <- bugs(data, inits, parameters, 'aug-vary-slope-model.txt', working.directory = 'bugs-models/',
                      n.chains=3, n.iter=500, bugs.seed=1)
plot(varySlopeAug)
print(varySlopeAug)

  
  ## Varying slope model with mar, jul, aug, sep
  data <- list('n', 'J', 'y', 'county', 'year', 'mar', 'jul', 'aug', 'sep')
  inits <- function() {
    list(
      mu.a = rnorm(1),
      mu.b1 = rnorm(1),
      mu.b2 = rnorm(1),
      sigma.a = runif(1),
      sigma.b1 = runif(1),
      sigma.b2 = runif(1),
      c1 = rnorm(1),
      c2 = rnorm(1)
    )
  }
  parameters <- c('a', 'b1', 'b2', 'b3', 'b4', 'c1', 'c2', 'sigma.y', 'mu.a', 'mu.b1', 'mu.b2', 'mu.b3', 'mu.b4',
                  'sigma.a', 'sigma.b1', 'sigma.b2', 'sigma.b3', 'sigma.b4')
  manyMonthsModel <- bugs(data, inits, parameters, 'many-months-model.txt', working.directory = 'bugs-models/',
                          n.chains = 3, n.iter = 5000, bugs.seed=1)
  print(manyMonthsModel)
  
  aValues <- manyMonthsModel$summary[1:J,]
  b1Values <- manyMonthsModel$summary[(J+1):(2*J),]
  b2Values <- manyMonthsModel$summary[(2*J+1):(3*J),]
  b3Values <- manyMonthsModel$summary[(3*J+1):(4*J),]
  b4Values <- manyMonthsModel$summary[(4*J+1):(5*J),]
  
  c1Value <- manyMonthsModel$summary[5*J + 1]
  c2Value <- manyMonthsModel$summary[5*J + 2]
  yearTrans <- (2019 - yearMean) / yearSd
  
  coefDf <- data.frame(
    CountyId = 1:J,
    Intercept = aValues[,1],
    MarCoef = b1Values[,1],
    JulCoef = b2Values[,1],
    AugCoef = b3Values[,1],
    SepCoef = b4Values[,1]
  )
  
  testCounties <- modelData %>% 
    distinct(FullCounty, CountyId)
  
  countyTestData <- rainMonthly2019 %>% 
    mutate(
      FullCounty = paste0(County, ', ', State)
    ) %>% 
    right_join(testCounties, 'FullCounty') %>% 
    arrange(CountyId) %>% 
    transmute(
      CountyId = CountyId,
      Intercept = 1,
      Mar = (MarPrcp - marMean) / marSd,
      Jul = (JulPrcp - julMean) / julSd,
      Aug = (AugPrcp - augMean) / augSd,
      Sep = (SepPrcp - sepMean) / sepSd
    )
  
  countyMultiplication <- coefDf * countyTestData
  
  countyWeatherEffects <- countyMultiplication %>% 
    select(-CountyId) %>% 
    rowSums()
  
  rawCountyPredictions <- countyWeatherEffects + c1Value * yearTrans + c2Value * yearTrans ^ 2
  countyPredictions <- rawCountyPredictions * yieldSd + yieldMean
  
  
  ## State model july and august
  data <- list('n', 'J', 'K', 'y', 'county', 'countyState', 'year', 'jul', 'aug')
  inits <- function() {
    list(
      mu.a = rnorm(1),
      mu.b1 = rnorm(1),
      mu.b2 = rnorm(1),
      mu.a.state = rnorm(1),
      mu.b1.state = rnorm(1),
      mu.b2.state = rnorm(1),
      sigma.a.state = runif(1),
      sigma.b1.state = runif(1),
      sigma.b2.state = runif(1),
      sigma.a.county = runif(1),
      signa.b1.county = runif(1),
      sigma.b2.county = runif(1),
      sigma.y = runif(1),
      c1 = rnorm(1),
      c2 = rnorm(1)
    )
  }
  parameters <- c('a', 'b1', 'b2', 'c1', 'c2', 'sigma.y', 'mu.a.state', 'mu.b1.state', 'mu.b2.state', 
                  'sigma.a.state', 'sigma.b1.state', 'sigma.b2.state', 'sigma.a', 'sigma.b1', 'sigma.b2')
  stateEffectsModel <- bugs(data, inits, parameters, 'state-effects-model.txt', working.directory = 'bugs-models/',
                          n.chains = 3, n.iter = 5000, bugs.seed=1)
  print(stateEffectsModel)
  
  
  
  ## State model july + aug + sep
  
  data <- list('n', 'J', 'K', 'y', 'county', 'countyState', 'year', 'jul', 'aug', 'sep')
  inits <- function() {
    list(
      mu.a = rnorm(1),
      mu.b1 = rnorm(1),
      mu.b2 = rnorm(1),
      mu.b3 = rnorm(1),
      mu.a.state = rnorm(1),
      mu.b1.state = rnorm(1),
      mu.b2.state = rnorm(1),
      mu.b3.state = rnorm(1),
      sigma.a.state = runif(1),
      sigma.b1.state = runif(1),
      sigma.b2.state = runif(1),
      sigma.b3.state = runif(1),
      sigma.a.county = runif(1),
      signa.b1.county = runif(1),
      sigma.b2.county = runif(1),
      sigma.b3.county = runif(1),
      sigma.y = runif(1),
      c1 = rnorm(1),
      c2 = rnorm(1)
    )
  }
  parameters <- c('a', 'b1', 'b2', 'b3', 'c1', 'c2', 'sigma.y', 
                  'mu.a.state', 'mu.b1.state', 'mu.b2.state', 'mu.b3.state',
                  'sigma.a.state', 'sigma.b1.state', 'sigma.b2.state', 'sigma.b3.state', 
                  'sigma.a', 'sigma.b1', 'sigma.b2')
  fullStateEffectsModel <- bugs(data, inits, parameters, 'state-effects-full-model.txt', 
                            working.directory = 'bugs-models/', n.chains = 3, n.iter = 5000, bugs.seed=1)
  
  fullAValues <- fullStateEffectsModel$summary[1:J, 1]
  fullB1Values <- fullStateEffectsModel$summary[(J+1):(2*J), 1]
  fullB2Values <- fullStateEffectsModel$summary[(2*J+1):(3*J), 1]
  fullB3Values <- fullStateEffectsModel$summary[(3*J+1):(4*J), 1]
  fullC1Value <- fullStateEffectsModel$summary[4*J+1, 1]
  fullC2Value <- fullStateEffectsModel$summary[4*J+2, 1]
  
  fullCoefDf <- data.frame(
    CountyId = 1:J,
    Intercept = fullAValues,
    July = b1Values,
    August = b2Values,
    September = b3Values
  )
  
  
  aValues <- stateEffectsModel$summary[1:J, 1]
  b1Values <- stateEffectsModel$summary[(J+1):(2*J), 1]
  b2Values <- stateEffectsModel$summary[(2*J+1):(3*J), 1]
  c1Value <- stateEffectsModel$summary[J*3+1, 1]
  c2Value <- stateEffectsModel$summary[J*3+2, 1]
  
  coefDf <- data.frame(
    CountyId = 1:J,
    Intercept = aValues,
    July = b1Values,
    August = b2Values
  )
  
  testCounties <- modelData %>% 
    distinct(FullCounty, CountyId)
  
  testData <- rainMonthly2019 %>% 
    mutate(
      FullCounty = paste0(County, ', ', State)
    ) %>% 
    right_join(testCounties, 'FullCounty') %>% 
    arrange(CountyId) %>% 
    transmute(
      CountyId = CountyId,
      Intercept = 1,
      Jul = (JulPrcp - julMean) / julSd,
      Aug = (AugPrcp - augMean) / augSd
    )
  
  multiplication <- coefDf * testData
  
  weatherEffects <- multiplication %>% 
    select(-CountyId) %>% 
    rowSums()
  
  rawStatePredictions <- weatherEffects + c1Value * yearTrans + c2Value * yearTrans ^ 2
  statePredictions <- rawStatePredictions * yieldSd + yieldMean
  
  rainDf <- rainMonthly2019 %>% 
    mutate(
      FullCounty = paste0(County, ', ', State)
    ) %>% 
    right_join(testCounties, 'FullCounty')
  
  fillIns <- data.frame(
    Name = c(
      'BENTON, INDIANA',
      'BROWN, ILLINOIS',
      'GRIGGS, NORTH DAKOTA',
      'JACKSON, INDIANA',
      'MORGAN, OHIO',
      'RED LAKE, MINNESOTA',
      'ROCK, NEBRASKA',
      'SCOTT, ILLINOIS',
      'SCOTT, INDIANA',
      'THURSTON, NEBRASKA',
      'UNION, KENTUCKY',
      'WASHINGTON, OKLAHOMA',
      'WYANDOT, OHIO',
      'BRACKEN, KENTUCKY',
      'MAHNOMEN, MINNESOTA',
      'CARROLL, OHIO',
      'JEFFERSON, KENTUCKY',
      'MOUNTRAIL, NORTH DAKOTA'
    ),
    EstimatedStateWeatherEffect = c(-2.05, -1.92, -2.56, -2.05, -1.03,
                             2.08, -1.92, -2.05, 2.08, -1.16, 0, -1.62, -1.62, 0,
                             -1.03, -1.62, -1.16, -2.56)
  )
  
  
  ### Data to be visualized elsewhere
  outputInfo <- data.frame(
    CountyId = 1:J,
    Name = rainDf$FullCounty,
    State = rainDf$State,
    EstimatedStateWeatherEffect = statePredictions - yearPredictions,
    EstimatedCountyWeatherEffect = countyPredictions - yearPredictions,
    WeatherStateModelPred = statePredictions,
    WeatherCountyModelPred = countyPredictions,
    YearModelPred = yearPredictions,
    MarchRain = rainDf$MarPrcp,
    JulyRain = rainDf$JulPrcp,
    AugustRain = rainDf$AugPrcp,
    SeptemberRain = rainDf$SepPrcp
  ) %>% 
    bind_rows(fillIns) %>% 
    left_join(fipsCodes, c('Name'= 'FullCounty')) %>% 
    filter(State != 'WEST VIRGINIA', State != 'KENTUCKY',
           State != 'WISCONSIN', State != 'OKLAHOMA') %>% 
    select(-State) %>% 
    left_join(avgPrcp, c('Name'='FullCounty')) %>% 
    mutate(
      AugustRainfallDiff = AugustRain - AugAvg,
      AugustJulyRainfallDiff = AugustRain + JulyRain - AugAvg - JulyAvg
    )
    
  
  # write.csv(outputInfo, 'gis-data-request-5.csv', row.names=FALSE)
  # saveRDS(stateEffectsModel, '../../models/state-effects-model.rds')
  # saveRDS(manyMonthsModel, '../../models/county-effects-model.rds')
  # saveRDS(varyInterceptYear, '../../models/year-model.rds')
    