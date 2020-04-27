library(forecast)
library(Metrics)
library(tidyverse)
library(fpp2)
library(xlsx)


fitArima <- function(trainData, p, d, q, n) {
  fit <- NA
  pred <- rep(0, n)
  tryCatch(
    {
      fit <- Arima(trainData, c(p, d, q))
      pred <- as.numeric(forecast(fit, n)$mean)
    },
    error = function(err) {
      print('Error!')
      print(err)
    }
  )

  list(fit, pred)
}


fitExpSmoothing <- function(trainData, alpha, n) {
  fit <- ses(trainData, alpha=alpha, h=n)
  pred <- as.numeric(forecast(fit, h=n)$mean, n)
  list(fit, pred)
}


soybeanPrice <- read.csv('../../data/processed/us-soybean-price.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(Date))

soybean <- read.csv('../../data/processed/us-soybean-futures.csv', stringsAsFactors = FALSE) %>% 
  arrange(Date) %>% 
  mutate(
    Date = as.Date(Date),
    DayNumber = as.numeric(Date) - 3,
    WeekNumber = DayNumber %/% 7 - 1616,
    CloseDiff = Close - dplyr::lag(Close),
    DayOfWeek = weekdays(Date)
  ) %>% 
  slice(2:n())

contractPrice <- read.csv('../../data/processed/contract-price.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(Date)) %>% 
  left_join(soybean, 'Date') %>% 
  left_join(soybeanPrice, 'Date')


marContract <- read.csv('../../data/processed/SoybeanPrices_YF_Mar.csv', stringsAsFactors = FALSE) %>% 
  transmute(
    Date = as.Date(date),
    MarchPrice = close,
    MarchVolume = volume
  )

mayContract <- read.csv('../../data/processed/SoybeanPrices_YF_May.csv', stringsAsFactors = FALSE) %>% 
  transmute(
    Date = as.Date(date),
    MayPrice = close,
    MayVolume = volume
  )

julContract <- read.csv('../../data/processed/SoybeanPrices_YF_Jul.csv', stringsAsFactors = FALSE) %>% 
  transmute(
    Date = as.Date(date),
    JulyPrice = close,
    JulyVolume = volume
  )

allContracts <- marContract %>% 
  left_join(mayContract, 'Date') %>% 
  left_join(julContract, 'Date') %>% 
  mutate(
    DateNumber = as.numeric(Date),
    WeekNumber = (DateNumber - 	17895) %/% 7,
    WeekDay = weekdays(Date)
  ) %>% 
  fill(MarchPrice, .direction='down') %>% 
  fill(MayPrice, .direction='down') %>% 
  fill(JulyPrice, .direction='down') %>% 
  filter(WeekDay != 'Sunday', WeekDay != 'Saturday') %>% 
  filter(WeekNumber >= 0) %>% 
  mutate(
    MarchPriceDiff = MarchPrice - lag(MarchPrice),
    MayPriceDiff = MayPrice - lag(MayPrice),
    JulyPriceDiff = JulyPrice - lag(JulyPrice)
  ) %>% 
  slice(1:213)

fridayPrice <- allContracts %>% 
  group_by(WeekNumber) %>% 
  summarise(
    NextWeek = mean(WeekNumber) + 1,
    MarchPriceLastPointPredict = sum((DateNumber == max(DateNumber)) * MarchPrice),
    MayPriceLastPointPredict = sum((DateNumber == max(DateNumber)) * MayPrice),
    JulyPriceLastPointPredict = sum((DateNumber == max(DateNumber)) * JulyPrice)
  ) %>% 
  select(-WeekNumber)

allContractsPredictionDf <- allContracts %>% 
  left_join(fridayPrice, c('WeekNumber'='NextWeek'))

ggplot(soybean) +
  geom_line(aes(Date, Close)) +
  expand_limits(y=0)

soybeanPrice %>% 
  filter(Date >= '2019-01-01') %>% 
ggplot() +
  geom_line(aes(Date, Price))

ggplot(soybean) +
  geom_line(aes(Date, CloseDiff)) +
  expand_limits(y=0)

contractPrice %>% 
  filter(Date >= '2019-01-01') %>% 
ggplot() +
  geom_line(aes(Date, MarchClose), color='green') +
  geom_line(aes(Date, MayClose), color='red') +
  geom_line(aes(Date, JulyClose), color='blue')

contractPrice %>% 
  filter(Date >= '2019-01-01') %>% 
ggplot() +
  geom_line(aes(Date, MarchClose - MayClose), color='green') +
  geom_line(aes(Date, MayClose - MayClose), color='red') +
  geom_line(aes(Date, JulyClose - MayClose), color='blue')


ggplot(contractPrice) +
  geom_line(aes(Date, Close)) +
  geom_line(aes(Date, Price * 99))

soybeanTrain <- soybean %>% 
  filter(WeekNumber < 800)

soybeanValidation <- soybean %>% 
  filter(WeekNumber >= 800, WeekNumber < 900)

soybeanTest <- soybean %>% 
  filter(WeekNumber >= 900)

Box.test(soybean$CloseDiff, lag=10, type='Ljung-Box')

hyperParams <- expand.grid(
  p = 0:5,
  q = 0:5,
  Error = NA
)

for(i in 1:nrow(hyperParams)) {
  param <- hyperParams[i,]
  print(param)
  totalError <- 0
  for(j in 800:899) {
    trainSubset <- soybean %>% 
      filter(WeekNumber < j)
    valSubset <- soybean %>% 
      filter(WeekNumber == j)
    n <- nrow(valSubset)
    
    modelOutput <- fitArima(trainSubset$CloseDiff, param[[1]], 0, param[[2]], n)
    error <- sum(cumsum(valSubset$CloseDiff - as.numeric(modelOutput[[2]]$mean))^2)
    totalError <- totalError + error
    cat(j, error, '\n')
  }
  hyperParams[i, 3] <- totalError
}



hyperParamsLarge <- expand.grid(
  p = 0:7,
  d = 0,
  q = 0:7,
  Error = NA
)

for(i in 1:nrow(hyperParamsLarge)) {
  param <- hyperParamsLarge[i,]
  print(param)
  totalError <- 0
  for(j in 884:933) {
    trainSubset <- soybean %>% 
      filter(WeekNumber >= 0, WeekNumber < j)
    valSubset <- soybean %>% 
      filter(WeekNumber == j)
    n <- nrow(valSubset)
    
    modelOutput <- fitArima(trainSubset$CloseDiff, param[[1]], param[[2]], param[[3]], n)
    error <- sum(cumsum(valSubset$CloseDiff - modelOutput[[2]])^2)
    totalError <- totalError + error
    cat(j, error, '\n')
  }
  hyperParamsLarge[i, 5] <- totalError
}


expSmoothHyperParams <- expand.grid(
  Alpha = seq(0.001, 0.999, 0.001),
  Error = NA
)

for(i in 1:nrow(expSmoothHyperParams)) {
  param <- expSmoothHyperParams[i,]
  totalError <- 0
  print(param)
  for(j in 834:933) {
    trainSubset <- soybean %>% 
      filter(WeekNumber < j)
    valSubset <- soybean %>% 
      filter(WeekNumber == j)
    n <- nrow(valSubset)
    
    modelOutput <- fitExpSmoothing(trainSubset$Close, param[[1]], n)
    error <- sum((valSubset$Close - modelOutput[[2]]) ^ 2)
    totalError <- totalError + error
  }
  expSmoothHyperParams[i, 2] <- totalError
}


totalNaiveError <- 0
for(i in 934:983) {
  valSubset <- soybean %>% 
    filter(WeekNumber == i)
  n <- nrow(valSubset)
  
  error <- sum(cumsum(valSubset$CloseDiff)^2)
  totalNaiveError <- totalNaiveError + error
}


# Optimal params: (5, 0, 6)
totalArimaError <- 0
startData <- soybean %>% 
  filter(Date >= '2018-12-21', Date < '2019-01-07')
startDiff <- startData$CloseDiff

errorDf <- data.frame()
for(i in 1:43) {
  print(i)
  previousTrain <- allContracts %>% 
    filter(WeekNumber >= 1, WeekNumber < i)
  
  valSubset <- allContracts %>% 
    filter(WeekNumber == i)
  n <- nrow(valSubset)
  
  marchDiff <- previousTrain$MarchPriceDiff
  mayDiff <- previousTrain$MayPriceDiff
  julyDiff <- previousTrain$JulyPriceDiff
  
  marchClose <- previousTrain$MarchPrice
  mayClose <- previousTrain$MayPrice
  julyClose <- previousTrain$JulyPrice
  
  marchModel <- fitArima(c(startDiff, marchDiff), 1, 0, 0, n)
  mayModel <- fitArima(c(startDiff, mayDiff), 1, 0, 0, n)
  julyModel <- fitArima(c(startDiff, julyDiff), 1, 0, 0, n)
  
  marchExpModel <- fitExpSmoothing(c(975.00, marchClose), 0.565, n)
  mayExpModel <- fitExpSmoothing(c(979.50, mayClose), 0.565, n)
  julyExpModel <- fitExpSmoothing(c(988.25, julyClose), 0.565, n)
  
  marchPredictions <- marchModel[[2]]
  mayPredictions <- mayModel[[2]]
  julyPredictions <- julyModel[[2]]
  
  marchExpPred <- marchExpModel[[2]]
  mayExpPred <- mayExpModel[[2]]
  julyExpPred <- julyExpModel[[2]]
  
  print(marchExpPred)
  
  newDf <- data.frame(
    Date = valSubset$Date,
    MarchArimaDiff = marchPredictions,
    MayArimaDiff = mayPredictions,
    JulyArimaDiff = julyPredictions,
    MarchExpPredict = marchExpPred,
    MayExpPred = mayExpPred,
    JulyExpPred = julyExpPred
  )
  
  errorDf <- bind_rows(errorDf, newDf)
}

fullDf <- allContractsPredictionDf %>% 
  left_join(errorDf, 'Date') %>% 
  mutate(
    MarchArimaPredict = MarchPriceLastPointPredict + MarchArimaDiff,
    MayArimaPredict = MayPriceLastPointPredict + MayArimaDiff,
    JulyArimaPredict = JulyPriceLastPointPredict + JulyArimaDiff,
  ) %>% 
  filter(WeekNumber >= 1) %>% 
  mutate(
    MarchAverage = (MarchPriceLastPointPredict + MarchExpPredict)  / 2
  )

# Optimal param: 0.819
totalExpSmoothError <- 0
for(i in 934:983) {
  trainSubset <- soybean %>% 
    filter(WeekNumber >= 0, WeekNumber < i)
  valSubset <- soybean %>% 
    filter(WeekNumber == i)
  n <- nrow(valSubset)
  
  modelOutput <- fitExpSmoothing(trainSubset$Close, 0.819, n)
  error <- sum((valSubset$Close - modelOutput[[2]])^2)
  totalExpSmoothError <- totalExpSmoothError + error
}


hyperParamsContract <- expand.grid(
  p = 0:7,
  d = 0,
  q = 0:7,
  Error = NA
)

for(i in 1:nrow(hyperParamsLarge)) {
  param <- hyperParamsContract[i,]
  print(param)
  totalError <- 0
  for(j in 10:43) {
    trainSubset <- allContracts %>% 
      filter(WeekNumber >= 1, WeekNumber < j)
    
    valSubset <- allContracts %>% 
      filter(WeekNumber == j)
    n <- nrow(valSubset)
    
    marchTrainDiff <- trainSubset$MarchPriceDiff
    mayTrainDiff <- trainSubset$MayPriceDiff
    julyTrainDiff <- trainSubset$JulyPriceDiff
    
    marchModel <- fitArima(marchTrainDiff, param[[1]], param[[2]], param[[3]], n)
    mayModel <- fitArima(mayTrainDiff, param[[1]], param[[2]], param[[3]], n)
    julyModel <- fitArima(julyTrainDiff, param[[1]], param[[2]], param[[3]], n)
    
    marchError <- sum(cumsum(valSubset$MarchPriceDiff - marchModel[[2]])^2)
    mayError <- sum(cumsum(valSubset$MayPriceDiff - mayModel[[2]])^2)
    julyError <- sum(cumsum(valSubset$JulyPriceDiff - julyModel[[2]])^2)
    
    totalError <- totalError + marchError + mayError + julyError
    cat(j, totalError, '\n')
  }
  hyperParamsContract[i, 4] <- totalError
}

expHyperParamsContract <- expand.grid(
  alpha = seq(0.001, 0.999, 0.001),
  Error = NA
)

for(i in 1:nrow(expHyperParamsContract)) {
  param <- expHyperParamsContract[i,]
  print(param)
  totalError <- 0
  for(j in 10:43) {
    trainSubset <- allContracts %>% 
      filter(WeekNumber >= 1, WeekNumber < j)
    
    valSubset <- allContracts %>% 
      filter(WeekNumber == j)
    n <- nrow(valSubset)
    
    marchTrain <- trainSubset$MarchPrice
    mayTrain <- trainSubset$MayPrice
    julyTrain <- trainSubset$JulyPrice
    
    marchModel <- fitExpSmoothing(marchTrain, param[[1]], n)
    mayModel <- fitExpSmoothing(mayTrain, param[[1]], n)
    julyModel <- fitExpSmoothing(julyTrain, param[[1]], n)
    
    marchError <- sum(cumsum(valSubset$MarchPrice - marchModel[[2]])^2)
    mayError <- sum(cumsum(valSubset$MayPrice - mayModel[[2]])^2)
    julyError <- sum(cumsum(valSubset$JulyPrice - julyModel[[2]])^2)
    
    totalError <- totalError + marchError + mayError + julyError
  }
  expHyperParamsContract[i, 2] <- totalError
}

finalTrain <- allContracts %>% 
  filter(Date >= '2019-01-07')

marchFinalDf <- read.csv('../../data/raw/ActiveSoybeanContractsForMarch2020-new.csv', stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, '%m/%d/%Y')) %>% 
  filter(Date >= '2019-01-07')
mayFinalDf <- read.csv('../../data/raw/ActiveSoybeanContractsForMay2020-new.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(Date, '%m/%d/%Y')) %>% 
  filter(Date >= '2019-01-07')
julyFinalDf <- read.csv('../../data/raw/ActiveSoybeanContractsforJuly2020-new.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(Date, '%m/%d/%Y')) %>% 
  filter(Date >= '2019-01-07')


finalMarchTrain <- marchFinalDf$Close
finalMayTrain <- mayFinalDf$Close
finalJulyTrain <- julyFinalDf$Close


ggplot() +
  geom_line(aes(Date, Close), data=marchFinalDf, color='yellow') +
  geom_line(aes(Date, Close), data=mayFinalDf, color='orange') +
  geom_line(aes(Date, Close), data=julyFinalDf, color='red')


finalMarchArima <- fitArima(finalMarchTrain, 1, 0, 0, 14)
finalMarchExp <- fitExpSmoothing(finalMarchTrain, 0.565, 14)

finalMarchPredictions <- data.frame(
  Arima = finalMarchArima[[2]],
  ExpSmoothing = finalMarchExp[[2]]
) %>% 
  mutate(
    Prediction = (Arima + ExpSmoothing + 4) / 2
  )

finalMayArima <- fitArima(finalMayTrain, 1, 0, 0, 14)
finalMayExp <- fitExpSmoothing(finalMayTrain, 0.565, 14)

finalMayPredictions <- data.frame(
  Arima = finalMayArima[[2]],
  ExpSmoothing = finalMayExp[[2]]
) %>% 
  mutate(
    Prediction = (Arima + ExpSmoothing + 4) / 2
  )

finalJulyArima <- fitArima(finalJulyTrain, 1, 0, 0, 14)
finalJulyExp <- fitExpSmoothing(finalJulyTrain, 0.565, 14)

finalJulyPredictions <- data.frame(
  Arima = finalJulyArima[[2]],
  ExpSmoothing = finalJulyExp[[2]]
) %>% 
  mutate(
    Prediction = (Arima + ExpSmoothing + 4) / 2
  )
