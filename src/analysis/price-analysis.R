library(tidyverse)


df <- read.csv('../../data/processed/us-soybean-futures.csv', stringsAsFactors = FALSE) %>% 
  mutate(
    Month = as.numeric(str_sub(Date, 6, 7))
  )

summary(aov(CloseReal~factor(Month), data=df))
