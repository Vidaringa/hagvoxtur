# Univariate

library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(forecastHybrid)

df <- readxl::read_excel("data.xlsx", sheet = 2)
df$date <- as.yearqtr(df$date)

# 152 er 2007Q4

df_ts <- ts(df$GDP, start = 1970, frequency = 4)

df_train <- window(df_ts, end = 2008 + 1/4)
df_test <- window(df_ts, start = 2008 + 2/4)



# ARIMA og ETS ------------------------------------------------------------

arima_fit <- auto.arima(df_train, approximation = FALSE, stepwise = FALSE)
arima_fc <- forecast(arima_fit, h = 8)


ets_fit <- ets(df_train)
ets_fc <- forecast(ets_fit)



df_spar <- tibble(date = time(df_test[1:8]),
                  raun = df_test[1:8],
                  arima = arima_fc$mean,
                  ets = ets_fc$mean) %>% 
  gather("key", "value", 2:4)


ggplot(df_spar,
       aes(x = date,
           y = value,
           col = key)) +
  geom_line()




# Out of sample -----------------------------------------------------------

gdp <- df %>% select(date, GDP)

ggplot(df,
       aes(x = date,
           y = log(GDP))) + 
  geom_line()

gdp_ts <- ts(gdp$GDP, start = 1970, frequency = 4)


arima_lv <- auto.arima(gdp_ts, approximation = FALSE, stepwise = FALSE)
arima_lv_fc <- forecast(arima_lv, h = 8)
arima_lv_fc %>% autoplot()

arima_lv_fc


# -------------------------------------------------------------------------


gdp_gr <- gdp %>% 
  mutate(gdp_gr = GDP/lag(GDP, 4) - 1) %>% 
  na.omit()

gdp_gr_ts <- ts(gdp_gr$gdp_gr, start = 1971, frequency = 4)

arima_gr <- auto.arima(gdp_gr_ts, approximation = FALSE, stepwise = FALSE)
arima_gr_fc <- forecast(arima_gr, h = 8)
arima_gr_fc %>% autoplot()
