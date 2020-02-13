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

data <- data %>% filter(year == 2017 & month == 11 & weekend == 0 & accommodation_type == "Hotel")

data <- data %>% select(- c(year, month, weekend, accommodation_type))



glimpse(data)
skimr::skim(data)
summary(data)

#DataExplorer::create_report(data)

DataExplorer::introduce(data)
DataExplorer::plot_intro(data)
DataExplorer::plot_missing(data)

## Left: frequency distribution of all discrete variables
DataExplorer::plot_bar(data)
## Right: `price` distribution of all discrete variables
DataExplorer::plot_bar(data, with = "price")

DataExplorer::plot_histogram(data)
DataExplorer::plot_density(data)

DataExplorer::plot_qq(data, by = "stars")

DataExplorer::plot_correlation(data)

DataExplorer::plot_scatterplot(split_columns(data)$continuous, by = "price", sampled_rows = 1000L)



#### Data Gen and Descriptives


data <- DataExplorer::dummify(data, select = 'offer_cat')

data <- data %>%
  mutate(agesq = age^2,
         agecu = age^3)

##### Regression Analysis

ggplot(data = data, aes(x=stars, y=price)) +
  geom_point(size=2,  shape=20, stroke=2, fill=color[3], color=color[3]) +
  geom_smooth(method="loess", se=F, colour=color[1], size=1.5, span=0.9) +
  labs(x = "Stars",y = "Price") +
  theme_bg() +
  scale_x_continuous(limits = c(0,5), breaks = seq(0,30, 10))

