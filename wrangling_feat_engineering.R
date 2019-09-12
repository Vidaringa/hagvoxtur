# Predict-a hagvöxt

library(tidyverse)
library(lubridate)
library(caret)
library(tidymodels)
library(glmnet)

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


# Feature engineering -----------------------------------------------------

df_final <- df_final %>% 
  mutate(prod_man_eur_gr = production_in_total_manufacturing_sa_index_europe / lag(production_in_total_manufacturing_sa_index_europe, 4) - 1,
         prod_man_us_gr = production_in_total_manufacturing_sa_index_us / lag(production_in_total_manufacturing_sa_index_us, 4) - 1,
         prod_ind_oecd_gr = production_of_total_industry_sa_index_oecd / lag(production_of_total_industry_sa_index_oecd, 4) - 1,
         real_wage_gr = real_wage_index / lag(real_wage_index, 4) - 1,
         nyskr_bila_gr = nyskraning_bila_fjoldi / lag(nyskraning_bila_fjoldi, 4) - 1,
         gdp_gr = gdp / lag(gdp, 4) - 1) %>% 
  select(-c(production_of_total_industry_sa_index_oecd,
            production_in_total_manufacturing_sa_index_us,
            production_in_total_manufacturing_sa_index_europe,
            real_wage_index,
            nyskraning_bila_fjoldi,
            gdp))



df_final %>% 
  gather("key", "value", 2:ncol(df_final)) %>% 
  ggplot(aes(x = quarter,
             y = value)) + 
  geom_line() +
  facet_wrap(~key,
             scales = "free_y")


# Split -------------------------------------------------------------------
# 2016Q4 - 2019Q2 eru bráðabirgða

hag_split <- initial_time_split(df_final, prop = 0.80) # Með þessu þá næ ég 6 obs í validation set án bráðabirgðatalna   
hag_train <- training(hag_split)

hag_train <- hag_train %>% select(-quarter)

# Interactions ------------------------------------------------------------
# Get annað hvort búið til öll og notað glmnet eða notað random forest til að veita vísbendingu um hugsanlega interaction


main_rec <- 
  recipe(
    gdp_gr ~ .,
    data = hag_train) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())



int_vars <- 
  main_rec %>% 
  pluck("var_info") %>% 
  filter(role == "predictor") %>% 
  pull(variable)


interactions <- t(combn(as.character(int_vars), 2))
colnames(interactions) <- c("var1", "var2")


interactions <- 
  interactions %>% 
  as_tibble() %>% 
  mutate(
    term = 
      paste0(
        "starts_with('",
        var1,
        "'):starts_with('",
        var2,
        "')"
      )
  ) %>% 
  pull(term) %>% 
  paste(collapse = "+")

interactions <- paste("~", interactions)
interactions <- as.formula(interactions)


int_rec <- 
  recipe(
    gdp_gr ~ .,
    data = hag_train) %>% 
  step_interact(interactions) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())