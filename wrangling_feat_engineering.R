# Predict-a hagvöxt

library(tidyverse)
library(lubridate)
library(caret)
library(tidymodels)
library(glmnet)
library(xts)
library(randomForest)


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
            gdp)) %>% 
  na.omit()


# Lags

tafin <- 4

ts_train <- ts(df_final[,-1])
ts_train <- as.xts(ts_train)
train_lag <- lag.xts(ts_train, 0:tafin)
train_lag <- as_tibble(train_lag)
train_lag$date <- df_final$quarter
train_lag <- train_lag %>% select(date, everything())
train_lag <- train_lag %>% na.omit()


# df_final %>% 
#   gather("key", "value", 2:ncol(df_final)) %>% 
#   ggplot(aes(x = quarter,
#              y = value)) + 
#   geom_line() +
#   facet_wrap(~key,
#              scales = "free_y")


# Split -------------------------------------------------------------------
# 2016Q4 - 2019Q2 eru bráðabirgða

hag_split <- initial_time_split(train_lag, prop = 0.77) # Með þessu þá næ ég 6 obs í validation set án bráðabirgðatalna   
hag_train <- training(hag_split)

hag_train <- hag_train %>% select(-date)
hag_test <- testing(hag_split)

# Interactions ------------------------------------------------------------
# Get annað hvort búið til öll og notað glmnet eða notað random forest til að veita vísbendingu um hugsanlega interaction


y_rf <- hag_train$gdp_gr
x_rf <- hag_train %>% select(-gdp_gr)

rf_forest <- randomForest(y = y_rf,
                          x = x_rf,
                          ntree = 500,
                          importance = TRUE)



# -------------------------------------------------------------------------


main_rec <- 
  recipe(
    gdp_gr ~ .,
    data = hag_train) %>% 
  step_interact(terms = ~ real_wage_gr.1:real_wage_gr.2) %>% 
  step_interact(terms = ~ real_wage_gr.1:real_ex) %>% 
  step_interact(terms = ~ real_wage_gr.1:prod_man_eur_gr.1) %>% 
  step_interact(terms = ~ real_wage_gr.1:gdp_gr.1) %>% 
  step_interact(terms = ~ real_wage_gr.2:real_ex) %>% 
  step_interact(terms = ~ real_wage_gr.2:prod_man_eur_gr.1) %>% 
  step_interact(terms = ~ real_wage_gr.2:gdp_gr.1) %>% 
  step_interact(terms = ~ real_ex:prod_man_eur_gr.1) %>% 
  step_interact(terms = ~ real_ex:gdp_gr.1) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())



# int_vars <-
#   main_rec %>%
#   pluck("var_info") %>%
#   filter(role == "predictor") %>%
#   pull(variable)
# 
# 
# interactions <- t(combn(as.character(int_vars), 2))
# colnames(interactions) <- c("var1", "var2")
# 
# 
# interactions <-
#   interactions %>%
#   as_tibble() %>%
#   mutate(
#     term =
#       paste0(
#         "starts_with('",
#         var1,
#         "'):starts_with('",
#         var2,
#         "')"
#       )
#   ) %>%
#   pull(term) %>%
#   paste(collapse = "+")
# 
# interactions <- paste("~", interactions)
# interactions <- as.formula(interactions)
# 
# 
# int_rec <-
#   recipe(
#     gdp_gr ~ .,
#     data = hag_train) %>%
#   step_interact(interactions) %>%
#   step_center(all_predictors()) %>%
#   step_scale(all_predictors())


# -------------------------------------------------------------------------


ctrl <- trainControl(
  method = "timeslice",
  initialWindow = 30,
  fixedWindow = FALSE,
  horizon = 4
)


main_glmn_h4 <- 
  train(main_rec,
        data = hag_train,
        method = "enet",
        tuneLength = 10,
        trControl = ctrl)

# Fæ error: Error: C stack usage  15924288 is too close to the limit
# int_glmn_h4 <- 
#   train(int_rec,
#         data = hag_train,
#         method = "enet",
#         tuneLength = 10,
#         trControl = ctrl)
# 

rf_model <- train(main_rec,
                  data = hag_train,
                  method = "rf",
                  trControl = ctrl,
                  importance = TRUE)




# -------------------------------------------------------------------------


grid <- expand.grid(committees = seq(1, 100),
                    neighbors = seq(0, 9))


cubist_model <- train(main_rec,
                      data = hag_train,
                      method = "cubist",
                      tuneGrid = grid)





pred_main <- tibble(enet = predict(main_glmn_h4, newdata = hag_test[,-1]),
                    # rf_spa = predict(rf_model, newdata = hag_test[,-1]),
                    # cubist = predict(cubist_model, newdata = hag_test[,-1]),
                    # spa_int = predict(int_glmn, newdata = hag_test[,-1]),
                    raun = hag_test$gdp_gr,
                    quarter = hag_test$date) %>% 
  gather("breyta", "gildi", 1:2)

ggplot(pred_main,
       aes(x = quarter,
           y = gildi,
           col = breyta)) + 
  geom_line() +
  geom_point()



