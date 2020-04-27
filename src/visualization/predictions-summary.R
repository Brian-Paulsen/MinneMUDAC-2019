library(tidyverse)
library(scales)
library(gridExtra)


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

df <- data.frame(
  Date = as.Date(c('2019-11-04', '2019-11-05', '2019-11-06', '2019-11-07', '2019-11-08')),
  MarchActual = c(951.25, 947.25, 940.75, 948.75, 944),
  MarchPredicted = c(949.80, 949.54, 949.26, 948.92, 948.72),
  MayActual = c(963.25, 959, 952.75, 960.25, 955.5),
  MayPredicted = c(960.98, 960.69, 960.39, 960.02, 959.81),
  JulyActual = c(973.4, 969.2, 963.2, 970.6, 966.2),
  JulyPredicted = c(970.59, 970.33, 970.00, 969.62, 969.41)
) %>% 
  mutate(
    MarchResidual = MarchActual - MarchPredicted,
    MayResidual = MayActual - MayPredicted,
    JulyResidual = JulyActual - JulyPredicted
  )

residDf <- data.frame(
  ContractMonth = c(rep('March', 5), rep('May', 5), rep('July', 5)),
  Date = rep(as.Date(c('2019-11-04', '2019-11-05', '2019-11-06', '2019-11-07', '2019-11-08')), 3),
  Residual = c(1.45, -2.29, -8.51, -0.17, -4.72, 2.27, -1.69, -7.64,
               0.23, -4.31, 2.81, -1.13, -6.80, 0.98, -3.21)
) %>% 
  mutate(
    ContractMonth = factor(ContractMonth, levels=c('July', 'May', 'March'))
  )

residHeatmap <- ggplot(residDf) +
  geom_tile(aes(Date, ContractMonth, fill=Residual)) +
  geom_text(aes(x=Date, y=ContractMonth, label=paste0(ifelse(Residual <= 0, '-', ''),round(abs(Residual), 2)))) +
  scale_fill_gradient2(low='#F57C00', high='#0D47A1') +
  labs(x='Date', y='Contract Month', title='Model Residuals') +
  theme_ag() +
  theme(
    axis.line = element_blank()
  )

residLine <- df %>% 
  mutate(
    MeanPredicted = (MarchPredicted + MayPredicted + JulyPredicted) / 3,
    MeanActual = (MarchActual + MayActual + JulyActual) / 3
  ) %>% 
ggplot() +
  geom_line(aes(Date, MeanPredicted), color='#1976D2') +
  geom_line(aes(Date, MeanActual), color='grey40') +
  labs(x='Date', y='Residual', title='Average of Models') +
  scale_y_continuous(labels=dollar_format()) +
  theme_ag()

marchPlot <- ggplot(df) +
  geom_line(aes(Date, MarchActual), color='grey40') +
  geom_point(aes(Date, MarchActual), color='grey40') +
  geom_text(aes(as.Date('2019-11-04'), 951.25 + 0.75, label='951.25'), color='grey40') +
  geom_text(aes(as.Date('2019-11-05'), 947.25 + 0.95, label='947.25'), color='grey40') +
  geom_text(aes(as.Date('2019-11-06'), 940.75 - 0.75, label='940.75'), color='grey40') +
  geom_text(aes(as.Date('2019-11-07')+0.05, 948.75 - 1.3, label='948.75'), color='grey40') +
  geom_text(aes(as.Date('2019-11-08'), 944 - 0.75, label='944.00'), color='grey40') +
  geom_line(aes(Date, MarchPredicted), color='#1976D2') +
  geom_point(aes(Date, MarchPredicted), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-04'), 949.8 - 0.7, label='949.80'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-05'), 949.54 + 0.7, label='949.54'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-06'), 949.26 + 0.7, label='949.26'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-07'), 948.92 + 0.7, label='948.92'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-08'), 948.74 + 0.7, label='948.74'), color='#1976D2') +
  labs(x='Date', y='Price', title='March Contract Predictions') +
  scale_y_continuous(breaks=seq(940, 952, 2)) +
  annotate('text', as.Date('2019-11-09')-0.7, y = 948.74, label='Predicted', color='#1976D2') +
  annotate('text', as.Date('2019-11-09')-0.76, y = 944, label='Actual', color='grey40') +
  theme_ag()

mayPlot <- ggplot(df) +
  geom_line(aes(Date, MayActual), color='grey40') +
  geom_point(aes(Date, MayActual), color='grey40') +
  geom_text(aes(as.Date('2019-11-04'), 963.2 + 0.75, label='$963.25'), color='grey40') +
  geom_text(aes(as.Date('2019-11-05'), 959.0 - 1.45, label='$959.00'), color='grey40') +
  geom_text(aes(as.Date('2019-11-06'), 952.6 - 0.75, label='$952.75'), color='grey40') +
  geom_text(aes(as.Date('2019-11-07'), 960.2 + 0.65, label='$960.20'), color='grey40') +
  geom_text(aes(as.Date('2019-11-08'), 955.4 - 0.75, label='$955.40'), color='grey40') +
  geom_line(aes(Date, MayPredicted), color='#1976D2') +
  geom_point(aes(Date, MayPredicted), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-04'), 960.98 - 0.7, label='$960.98'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-05'), 960.69 + 0.7, label='$960.69'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-06'), 960.39 + 0.7, label='$690.39'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-07'), 960.02 - 1.25, label='$960.02'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-08'), 959.81 + 0.7, label='$959.81'), color='#1976D2') +
  labs(x='Date', y='Price', title='May Contract Predictions') +
  scale_y_continuous(labels=dollar_format(), breaks=seq(952, 964, 2)) +
  annotate('text', as.Date('2019-11-09')-0.7, y = 959.81, label='Predicted', color='#1976D2') +
  annotate('text', as.Date('2019-11-09')-0.76, y = 955.4, label='Actual', color='grey40') +
  theme_ag()

julyPlot <- ggplot(df) +
  geom_line(aes(Date, JulyActual), color='grey40') +
  geom_point(aes(Date, JulyActual), color='grey40') +
  geom_text(aes(as.Date('2019-11-04'), 973.50 + 0.75, label='$973.50'), color='grey40') +
  geom_text(aes(as.Date('2019-11-05') - 0.1, 969.25 - 0.95, label='$969.25'), color='grey40') +
  geom_text(aes(as.Date('2019-11-06'), 963.25 - 0.75, label='$963.25'), color='grey40') +
  geom_text(aes(as.Date('2019-11-07'), 970.75 + 0.75, label='$970.75'), color='grey40') +
  geom_text(aes(as.Date('2019-11-08'), 966.25 - 0.75, label='$966.25'), color='grey40') +
  geom_line(aes(Date, JulyPredicted), color='#1976D2') +
  geom_point(aes(Date, JulyPredicted), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-04'), 970.59 - 0.7, label='$970.59'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-05'), 970.33 + 0.7, label='$970.33'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-06'), 970.00 + 0.7, label='$970.00'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-07'), 969.62 - 0.7, label='$969.62'), color='#1976D2') +
  geom_text(aes(as.Date('2019-11-08'), 969.41 + 0.7, label='$969.41'), color='#1976D2') +
  labs(x='Date', y='Price', title='July Contract Predictions') +
  scale_y_continuous(labels=dollar_format(), breaks=seq(962, 974, 2)) +
  annotate('text', as.Date('2019-11-09')-0.7, y = 969.41, label='Predicted', color='#1976D2') +
  annotate('text', as.Date('2019-11-09')-0.76, y = 966.25, label='Actual', color='grey40') +
  theme_ag()

grid.arrange(marchPlot, mayPlot, julyPlot)

grid.arrange(marchPlot, residHeatmap)
