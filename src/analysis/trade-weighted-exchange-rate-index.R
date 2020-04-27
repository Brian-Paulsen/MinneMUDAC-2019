library(tidyverse)

df <- read.csv('../../data/processed/us-market-commodity-exchange-rates.csv', stringsAsFactors = FALSE)
monthlyDf <- read.csv('../../data/processed/monthly-supply-demand.csv', stringsAsFactors = FALSE)
yearlyDf <- read.csv('../../data/processed/yearly-supply-demand.csv', stringsAsFactors = FALSE)
brazilExports <- read.csv('../../data/raw/brazil-exports.csv', stringsAsFactors = FALSE) %>% 
  mutate(BrazilExports = Exports)


combined <- df %>% 
  inner_join(monthlyDf, c('Month', 'Year')) %>% 
  mutate(Date=as.Date(Date)) %>% 
  filter(Date >= '2015-01-01')

ggplot(combined) +
  geom_line(aes(Date, SoybeanExports)) +
  expand_limits(y=0)


    

combinedYearly <- df %>% 
  group_by(Year) %>% 
  summarise(
    SoybeanIndex = mean(Soybeans)
  ) %>% 
  inner_join(yearlyDf, 'Year') %>% 
  left_join(brazilExports, 'Year') %>% 
  mutate(
    IndexDiff = SoybeanIndex - lag(SoybeanIndex),
    PriceDiff = SoybeanPrice - lag(SoybeanPrice),
    ExportDiff = SoybeanExports - lag(SoybeanExports),
    BrazilExportDiff = BrazilExports - lag(BrazilExports),
    ExportRatio = SoybeanExports / BrazilExports,
    RatioDiff = ExportRatio - lag(ExportRatio)
  )

ggplot(combinedYearly) +
  geom_line(aes(Year, SoybeanIndex)) +
  geom_line(aes(Year, SoybeanPrice*10), color='green')

ggplot(combinedYearly) +
  geom_point(aes(SoybeanIndex, SoybeanPrice))

summary(lm(SoybeanPrice~SoybeanIndex, data=combinedYearly))

ggplot(combinedYearly) +
  geom_line(aes(Year, IndexDiff)) +
  geom_line(aes(Year, PriceDiff * 10), color='green')

ggplot(combinedYearly) +
  geom_point(aes(IndexDiff, PriceDiff))

summary(lm(PriceDiff~IndexDiff, data=combinedYearly))

ggplot(combinedYearly) +
  geom_point(aes(IndexDiff, ExportDiff))

summary(lm(ExportDiff~IndexDiff, data=combinedYearly))


data2000s <- filter(combinedYearly, Year >= 2000)

ggplot(data2000s) +
  geom_point(aes(IndexDiff, RatioDiff))
  
summary(lm(RatioDiff~IndexDiff, data=data2000s))



monthlyDf %>% 
  group_by(Month) %>% 
  summarise(MonthAvg = mean(SoybeanSeedPrice, na.rm=TRUE)) %>% 
ggplot() +
  geom_line(aes(Month, MonthAvg))


price <-read.csv('../../data/processed/us-soybean-price.csv', stringsAsFactors = FALSE) %>% 
  mutate(
    Date = as.Date(Date),
    Month = as.numeric(str_sub(Date, 6, 7)),
    Year = as.numeric(str_sub(Date, 1, 4)),
    MarketingYear = ifelse(Month <= 8, Year + 1, Year)
  )

monthTrend <- price %>% 
  filter(Date >= '2000-01-01') %>% 
  group_by(Month) %>% 
  summarise(
    MeanMonthPrice = median(Price)
  )

ggplot(monthTrend) +
  geom_line(aes(Month, MeanMonthPrice)) +
  scale_x_continuous(limits=c(1, 12), breaks = seq(0, 12, 2))

yearCenteredPrice <- price %>% 
  group_by(MarketingYear) %>% 
  mutate(
    CenteredPrice = Price - mean(Price)
  ) %>% 
  ungroup()

yearCenteredPrice %>% 
  group_by(Month) %>% 
  summarise(
    PriceSd = sd(CenteredPrice)
  ) %>% 
ggplot() +
  geom_line(aes(Month, PriceSd)) +
  expand_limits(y=0)

ggplot(yearCenteredPrice) +
  geom_boxplot(aes(x=factor(Month), y=CenteredPrice)) +
  stat_summary(aes(x=Month, y=CenteredPrice), fun.y=median, geom='line', color='red')


futurePrice <- read.csv('../../data/processed/us-soybean-futures.csv', stringsAsFactors = FALSE) %>% 
  mutate(Date = as.Date(Date))

futurePrice %>% 
  filter(Date >= '2018-01-01', Date <= '2018-12-31') %>% 
ggplot() +
  geom_line(aes(Date, CloseReal))
  
  