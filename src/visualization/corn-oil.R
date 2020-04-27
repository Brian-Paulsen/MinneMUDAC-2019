library(tidyverse)
library(gridExtra)
library(scales)


theme_ag <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace%
    theme(
      panel.grid.major  = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color="grey60"),
      axis.ticks = element_blank(),
      axis.text = element_text(color = "grey60"),
      axis.title = element_text(color="grey50")
    )
}


set.seed(10)
df <- read.csv('../../data/processed/outt.csv', stringsAsFactors = FALSE) %>% 
  slice(sample(n(), n()/10))


summary(lm(soy_price~corn_price, data=df))
summary(lm(soy_price~oil_price, data=df))

pCorn <- ggplot(df) +
  geom_point(aes(corn_price, soy_price), color='#1976D2') +
  geom_abline(aes(slope=1.83678, intercept=235.74766), color='grey45') +
  scale_x_continuous(labels=dollar_format()) +
  scale_y_continuous(labels=dollar_format()) +
  labs(x='Corn Price', y='Soybean Price', title='Soybean Vs. Corn Price') +
  annotate('text', 600, 800, label=bquote(''~ R^2*' = 0.7998'), color='grey20') +
  theme_minimal() +
  theme(
    panel.grid=element_blank()
  ) +
  theme_ag()

pOil <- ggplot(df) +
  geom_point(aes(oil_price, soy_price), color='#1976D2') +
  geom_abline(aes(slope=9.3782, intercept=351.0238), color='grey45') +
  scale_x_continuous(labels=dollar_format()) +
  scale_y_continuous(labels=dollar_format()) +
  labs(x='Oil Price', y='', title='Soybean Vs. Oil Price') +
  annotate('text', 100, 800, label=bquote(''~ R^2*' = 0.6206'), color='grey20') +
  theme_minimal() +
  theme(
    panel.grid=element_blank(),
    axis.text.y=element_blank()
  ) +
  theme_ag()

grid.arrange(pCorn, pOil, nrow=1)
    