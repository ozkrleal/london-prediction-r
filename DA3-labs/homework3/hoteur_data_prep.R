# Clear memory, use it in development phase only
rm(list=ls())


library(tidyverse)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DataExplorer)

source("helper_functions/theme_bg.R")
source("helper_functions/da_helper_functions.R")

price <- read_csv("homework3/hotels-europe_price.csv")
features <- read_csv("homework3/hotels-europe_features.csv")

data <- merge(price, features)

data <- data %>% filter(city %in% c('Berlin', 'Munich', 'Vienna', 'Budapest', 'Prague', 'Warsaw'))

data <- data %>% filter(year == 2017 & month == 11 & weekend == 0)

data <- data %>% select(- c(year, month, weekend))



glimpse(data)
skimr::skim(data)
summary(data)

DataExplorer::create_report(data)

