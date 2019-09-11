# Predict-a hagvöxt

library(tidyverse)
library(lubridate)


# Mánaðartölur ------------------------------------------------------------

df <- readxl::read_excel("data.xlsx") %>% janitor::clean_names()
df$date <- ymd(df$date)

monthly <- ts(df[,-1], start = c(1980, 1), frequency = 12)
quarterly <- aggregate(monthly, nfrequency = 4, FUN = mean)

quarter <- paste(rep(1980:2019, each = 4), rep(c("Q1", "Q2", "Q3", "Q4")), sep = " ")
quarter <- head(quarter, -2)

quarterly <- as_tibble(quarterly)
quarterly$quarter <- quarter

quarterly <- quarterly %>% 
  select(quarter, everything())

quarterly$quarter <- zoo::as.yearqtr(quarterly$quarter)



# Ársfjórðungsgögn --------------------------------------------------------

df_q <- readxl::read_excel("data.xlsx", sheet = 2) %>% janitor::clean_names()

df_q$date <- zoo::as.yearqtr(df_q$date)


# Final -------------------------------------------------------------------

df_final <- quarterly %>% 
  select(-contains("utlan")) %>% 
  filter(year(quarter) >= 1995) %>% 
  left_join(df_q, by = c("quarter" = "date")) %>% 
  select(-vnv)


df_final %>% 
  gather("key", "value", 2:ncol(df_final)) %>% 
  ggplot(aes(x = quarter,
             y = value)) + 
  geom_line() +
  facet_wrap(~key,
             scales = "free_y")



# Feature engineering -----------------------------------------------------

df_final <- df_final %>% 
  mutate(prod_man_eur_gr = production_in_total_manufacturing_sa_index_europe / lag(production_in_total_manufacturing_sa_index_europe, 4) - 1,
         prod_man_us_gr = production_in_total_manufacturing_sa_index_us / lag(production_in_total_manufacturing_sa_index_us, 4) - 1,
         prod_ind_oecd_gr = production_of_total_industry_sa_index_oecd / lag(production_of_total_industry_sa_index_oecd, 4) - 1,
         real_wage_gr = real_wage_index / lag(real_wage_index, 4) - 1,
         gdp_gr = gdp / lag(gdp, 4) - 1) %>% 
  select(-c(production_of_total_industry_sa_index_oecd,
            production_in_total_manufacturing_sa_index_us,
            production_in_total_manufacturing_sa_index_europe,
            real_wage_index,
            gdp))



# Interaction terms?

