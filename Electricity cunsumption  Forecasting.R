library(fpp3)
library(tidyverse)
library(knitr)
library(seasonal)
library(dplyr)
library(tsibble)
library(forecast)

### Filtering for Required Data ### :

summarized_data <- D202 %>%
  group_by(date) %>%
  summarize(usage = sum(usage), cost = sum(cost)) %>%
  ungroup

filtered_data <- summarized_data %>%
  filter(date >= as.Date("2017-01-01") & date <= as.Date("2017-12-31"))

filtered_data <- data.frame(type = "Electricity usage", filtered_data)


## converted the "date" column from character type to Date type to 
#facilitate time series analysis 

filtered_data <- filtered_data %>%
  mutate(date = as.Date(date)) 

## transformed the data into a time series format known as tsibble, 
#with the "date" column serving as the index
Electricity_Consumption <- as_tsibble(filtered_data, index = date, key = type)

### 2. Visualization ###

#Time Plot

Electricity_Consumption %>% autoplot(usage)+
  labs(y = "Usage of electricity in kilowatthour",
       title = "Time Plot of Electricity Cunsumption")


#ACF Plot

Electricity_Consumption %>%
  ACF(usage, lag_max = 50) %>%
  autoplot() +
  labs(title = "ACF Plot of Electricity Cunsumption")

### Transformation ###

##Decomposition  
Decomposition <- Electricity_Consumption %>%
  model(
    classical = classical_decomposition(usage, type = "additive"),
    stl = STL(usage)
  )

# STL Decomposition

stlcomps <- Decomposition %>% select(stl) %>% components(Decomposition)
stlcomps %>% autoplot()

# create density plot
density_check <- density(stlcomps$remainder)
plot(density_check, main = "Density Plot of Electricity usage",
     xlab = "Electricity usage", ylab = "Density")

# Classical decomposition

classicalcomps <- Decomposition %>% select(classical) %>% components(Decomposition)
classicalcomps %>% autoplot()

### Forecasting ###

# Splitting data set into training set and test set. 
Electric_usage_train <- Electricity_Consumption %>% 
  filter(date >= as.Date("2017-01-01") & date <= as.Date("2017-10-31"))
Electric_usage_test <- Electricity_Consumption %>% 
  filter(date >= as.Date("2017-11-01") & date <= as.Date("2017-12-31"))


## 1. Mean Model ##

#Creating Mean Model using training data

Electric_usage_mean_fit <- Electric_usage_train %>% 
  model(
    Mean = MEAN(usage)
  )
#Forecasting for Next 2 Months
Electric_usage_mean_fc <- Electric_usage_mean_fit %>% forecast(h = 61)
#plot forecast line
Electric_usage_mean_fc %>% autoplot(Electric_usage_test, level = NULL)

#Checking Accuracy
accuracy(Electric_usage_mean_fc, Electric_usage_test) %>%
  select(.model, RMSE, MAE)

#Residual Plots
Electric_usage_mean_fit %>% gg_tsresiduals()


## 2.Naive Model ##

#Creating Naive Model using training data
Electric_usage_naive_fit <- Electric_usage_train %>% 
  model(
    Naive = NAIVE(usage)
  )
#Forecasting Next 2 Months
Electric_usage_naive_fc <- Electric_usage_naive_fit %>% forecast(h = 61)
#plot forecast line
Electric_usage_naive_fc %>% autoplot(Electric_usage_test, level = NULL)

#Checking Accuracy
accuracy(Electric_usage_naive_fc, Electric_usage_test) %>%
  select(.model, RMSE, MAE)

#Residual Plots
Electric_usage_naive_fit %>% gg_tsresiduals()


## 3. Drift Model ##
#Creating Drift Model using training data
Electric_usage_drift_fit <- Electric_usage_train %>% 
  model(
    Drift = NAIVE(usage ~ drift())
  )
#Forecasting Next 2 Months
Electric_usage_drift_fc <- Electric_usage_drift_fit %>% forecast(h = 61)
#plot forecast line
Electric_usage_drift_fc %>% autoplot(Electric_usage_test, level = NULL)

#Checking Accuracy
accuracy(Electric_usage_drift_fc, Electric_usage_test) %>%
  select(.model, RMSE, MAE)

#Residual Plots
Electric_usage_drift_fit %>% gg_tsresiduals()

