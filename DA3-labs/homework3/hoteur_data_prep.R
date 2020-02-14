# Clear memory, use it in development phase only
rm(list=ls())


library(tidyverse)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DataExplorer)
library(caret)
library(rattle)


source("helper_functions/theme_bg.R")
source("helper_functions/da_helper_functions.R")

price <- read_csv("homework3/hotels-europe_price.csv")
features <- read_csv("homework3/hotels-europe_features.csv")

data <- merge(price, features)

data <- data %>% filter(city %in% c('Berlin', 'Munich', 'Vienna', 'Budapest', 'Prague', 'Warsaw'))

data <- data %>% filter(year == 2017 & month == 11 & weekend == 0 & accommodation_type == "Hotel")

data <- data %>% select(- c(year, month, weekend, accommodation_type, city_actual))
data <- data %>% select(- c(center1label, center2label))


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
#factor variables
data <- data %>% mutate(f_city = factor(city),
                        f_offer_cat = factor(offer_cat),
                        f_country = factor(country))
#data <- DataExplorer::dummify(data, select = 'offer_cat')


data <- data %>%
  mutate(ln_price = log(price),
         ln_rating_reviewcount = log(rating_reviewcount),
         ln_ratingta_count = log(ratingta_count))

data <- data %>%
  mutate(ln_distance = log(distance),
         ln_distance_alter = log(distance_alter))

data <- data %>%
  mutate(sq_distance = distance^2,
         sq_rating_reviewcount = rating_reviewcount^2)

DataExplorer::plot_scatterplot(split_columns(data)$continuous, by = "ln_price", sampled_rows = 1000L)

ggplot(data, aes(price)) + geom_histogram(binwidth = 50) + theme_bg()
ggplot(data, aes(ln_price)) + geom_histogram(binwidth = 0.25) + theme_bg()

#removing outliers
data <- data %>% filter(price <= 1000)

##### Regression Analysis

ggplot(data = data, aes(x=stars, y=price)) +
  geom_point(size=2,  shape=20, stroke=2, fill=color[3], color=color[3]) +
  geom_smooth(method="loess", se=F, colour=color[1], size=1.5, span=0.9) +
  labs(x = "Stars",y = "ln_Price") +
  theme_bg() +
  scale_x_continuous(limits = c(0.5,5.5), breaks = seq(0,30, 10))

ggplot(data = data, aes(x=stars, y=ln_price)) +
  geom_point(size=2,  shape=20, stroke=2, fill=color[3], color=color[3]) +
  geom_smooth(method="loess", se=F, colour=color[1], size=1.5, span=0.9) +
  labs(x = "Stars",y = "ln_Price") +
  theme_bg() +
  scale_x_continuous(limits = c(0.5,5.5), breaks = seq(0,30, 10))


## country: look at distribution
data %>%
  group_by(rating) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data = data, aes(x=rating_reviewcount, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,1000)+
  labs(x="Rating review count",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()

ggplot(data = data, aes(x=ln_rating_reviewcount, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,1000)+
  labs(x="Ln rating review count",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()

ggplot(data = data, aes(x=ln_distance, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,1000)+
  labs(x="ln distance",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()

ggplot(data = data, aes(x=distance, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,1000)+
  labs(x="ln distance",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)


#################################
# Create test and train samples #
#################################
# now all stuff runs on training vs test (holdout), alternative: 4-fold CV 


# create test and train samples (70% of observations in train sample)
train_indices <- createDataPartition(data$price, p = 0.7, list = FALSE)
data_train <- data[train_indices, ]
data_test <- data[-train_indices, ]


# Define models: simpler, extended -----------------------------------------------------------

## Linear Regression

linmod <- lm(ln_price ~ distance + sq_distance + ln_rating_reviewcount, data=data)
# Regression 2: ln price and log num of rat
#linmod2 <- lm(ln_price ~ ln_distance, data=data)
# Regression 3: ln price and num of accomodates
#linmod21 <- lm(ln_price ~ distance, data=data)

#linmod3 <- lm(ln_price ~ rating_reviewcount + sq_rating_reviewcount, data=data)
# Regression 2: ln price and log num of rat
#linmod4 <- lm(ln_price ~ ln_rating_reviewcount, data=data)
# Regression 3: ln price and num of accomodates
#linmod5 <- lm(ln_price ~ rating_reviewcount, data=data)

basic_vars <- c(
  "offer", "f_offer_cat", "f_city",
  "f_country","distance", "ln_distance", "holiday", "nnights",
  "ln_distance", "ln_distance_alter", "sq_distance", "stars", "neighbourhood")

# reviews
rating <- c("rating", "ratingta" ,"ratingta_count","ln_rating", 
             "rating_reviewcount","ln_rating_reviewcount",  "sq_rating_reviewcount")

predictors <- c(basic_vars, rating)


###########################################################################################

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# set tuning
tune_grid <- expand.grid(
  .mtry = c(7, 9),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)


#proly wont need
# simpler model for model A (1)
set.seed(1234)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity",
    na.action = na.omit
  )
})
rf_model_1

#### OLS
set.seed(1234)
system.time({
  ols_model <- train(
    formula(paste0("price ~", paste0(predictors, collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control,
    na.action = na.omit
  )
})

ols_model
ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))


#### CART
set.seed(1234)
system.time({
  cart_model <- train(
    formula(paste0("price ~", paste0(predictors, collapse = " + "))),
    data = data_train,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control,
    na.action = na.omit
  )
})

fancyRpartPlot(cart_model$finalModel, sub = "")
cart_model

# GBM  -------------------------------------------------------
gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = 250, # number of iterations, i.e. trees
                         shrinkage = c(0.05, 0.1), # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(1234)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid,
                     na.action = na.omit)
})
gbm_model

