############################################################
#
# DATA ANALYSIS TEXTBOOK
# Chapter 15
# CART
# Used cars

# v1.3 2019-10-15 operational, caret version
# v1.4 2019-12-13 minor fixes, scatter graphs in eps
# v1.5 2020-01-07 minor fixes on sample design
# v1.6 2020-01-08 minor fixes on graphs
############################################################  
#
# WHAT THIS CODES DOES:
# creates regression trees with the CART method
# builds trees, does pruning


#############################
#TODO
#############################

# TODO 
# graphs -- make tree graphs it nicer, also print as eps.
# have a nice varimp graph like in ch16, for the pruned tree

# Clear memory
rm(list=ls())

# Descriptive statistics and regressions
library(caret)
library(tidyverse)
library(skimr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(glmnet)
library(rpart)
library(rattle)
library(rpart.plot)
library(xtable)
library(Hmisc)

# CHECK WORKING DIRECTORY - CHANGE IT TO YOUR WORKING DIRECTORY
dir <-  "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
#dir <- "D:/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"

# location folders
data_in <- "lab2/data/"
data_out <- "lab2/data/"
output   <- "lab2/output/"

# load ggplot theme function
source("helper_functions/theme_bg.R")
source("helper_functions/da_helper_functions.R")

source("lab2/code/Ch14_airbnb_prediction_functions.R")


# DATA IMPORT
data <- read.csv(paste0(data_in,"used-cars_2cities_prep.csv"), stringsAsFactors = TRUE)

# SAMPLE DESIGN

# manage missing
data$fuel <- fct_explicit_na(data$fuel, na_level = "Missing")
data$drive <- fct_explicit_na(data$drive, na_level = "Missing")
data$cylinders <- fct_explicit_na(data$cylinders, na_level = "Missing")
data$transmission <- fct_explicit_na(data$transmission, na_level = "Missing")
data$type <- fct_explicit_na(data$type, na_level = "Missing")


# missing changed to good not missing
# data$condition <- fct_explicit_na(data$condition, na_level = "Missing")
table(data$condition       )
data$condition[is.na(data$condition)] <- "good"
table(data$condition       )

# same steps as in ch13, see code in ch13 for details
data <- data %>% filter(Hybrid ==0) %>% dplyr::select(-Hybrid)
data <- data %>% filter(fuel=="gas")
data <- data %>% filter(!condition %in% c("new", "fair"))
data <- data %>% filter(price %in% c(500:25000), odometer <=100)
data <- data %>% filter(!(price < 1000 & (condition == "like new"|age < 8)))
data <- data %>% filter(!(transmission == "manual"))
data <- data %>% filter(!type %in% c("truck", "pickup"))
data <- data %>% dplyr::select(-pricestr)



# to be on the safe side
data <- data %>% drop_na(price)


################################################################################

# DATA GENERATION & DESCRIPTIVES
# price  age   odometer + condition cylinder dealer city LE 

# condition
data <- data %>%
  mutate(cond_excellent = ifelse(condition == "excellent", 1,0),
         cond_good = ifelse(condition == "good", 1,0),
         cond_likenew = ifelse(condition == "like new", 1,0))

# cylinders
data <- data %>%
  mutate(cylind6 = ifelse(cylinders=="6 cylinders",1,0))


#chicago
data$chicago <- ifelse(data$area=="chicago",1,0)

# age: quadratic, cubic
data <- data %>%
  mutate(agesq = age^2,
         agecu = age^3)

# odometer: quadratic
data <- data %>%
  mutate(odometersq = odometer^2)


# save workfile
write.csv(data, paste0(data_out, "usedcars_work.csv"), row.names = F)

summary(data$price)






#################################
# Create test and train samples #
#################################
# now all stuff runs on training vs test (holdout), alternative: 4-fold CV 


# create test and train samples (70% of observations in train sample)
smp_size <- floor(0.7 * nrow(data))
set.seed(20180122)

train_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$train <- 0
data$train[train_ids] <- 1
# Create train and test sample variables
data_train <- data %>% filter(train == 1)
data_test <- data %>% filter(train == 0)



#####################
# Regression tree (rpart)

summary(data_train$price)

# AGE IS THE ONLY  PREDICTOR VARIABLE
model1 <- formula(price ~ age)

# Single split
# (make sure it's a single split by setting "maxdepth" to 1)

cart1 <- train(
  model1, data = data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=1))

summary(cart1)
pred_cart1 <- predict(cart1, data_test)
rmse_cart1 <- sqrt(mean((pred_cart1 - data_test$price)^2))

# Tree graph
png(filename =paste0(output, "ch15_usedcars_tree1.png"))
rpart.plot(cart1$finalModel, tweak=1.2, digits=-1, extra=1)
dev.off()

# Scatterplot with step function
data_train$xend <- c(data_train$age+1)
data_train$yend <- predict(cart1, data.frame(age=data_train$age))
pred_cart1t <- predict(cart1, data_train)

g1<-ggplot(data = data_train, aes(x = age, y=yend, xend=xend, yend=yend)) +
  geom_point(aes(x=age , y=price), color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  geom_segment(color=color[4], size=1, na.rm=TRUE) +
  scale_x_continuous(limits=c(0, 25), breaks=seq(0, 25, by=5)) + 
  labs(x = "Age (years)", y = "Price (USD)") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
g1
ggsave(plot = g1, paste0(output, "ch15_usedcars_tree1_stepfn.png"), 
       width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "ch15_usedcars_tree1_stepfn.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(g1)
dev.off()





###########
# Splits at two levels
# (make sure it stops by setting "maxdepth" to 2)

cart2 <- train(
  model1, data = data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=2))

summary(cart2)


tab_cart2 <- data.frame(
  "Category" = c("Age 1-4", "Age 5-7","Age 8-12","Age 13 or more"),
  "Count" = c(summary(cart2)$frame$n[7], summary(cart2)$frame$n[6], summary(cart2)$frame$n[4], summary(cart2)$frame$n[3]),
  "Average_price" = c(summary(cart2)$frame$yval[7], summary(cart2)$frame$yval[6], summary(cart2)$frame$yval[4], summary(cart2)$frame$yval[3])
  )

print(xtable(tab_cart2, type = "latex"), file = paste0(output, "ch15_table_cart2.tex"),include.rownames=FALSE, booktabs=TRUE, floating = FALSE)

pred_cart2 <- predict(cart2, data_test)
rmse_cart2 <- sqrt(mean((pred_cart2 - data_test$price)^2))

# Tree graph
par(mar=c(1,1,1,1))
png(filename =paste0(output, "ch15_usedcars_tree2.png"))
rpart.plot(cart2$finalModel, tweak=1.2, digits=-1, extra=1)
dev.off()

# Scatterplot with step function
data$xend <- c(data$age+1)
data$yend <- predict(cart2, data.frame(age=data$age))

# Scatterplot with step function
data_train$xend <- c(data_train$age+1)
data_train$yend <- predict(cart2, data.frame(age=data_train$age))
pred_cart2t <- predict(cart2, data_train)



g2<-ggplot(data = data_train, aes(x = age, y=yend, xend=xend, yend=yend)) +
  geom_point(aes(x=age , y=price), color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  geom_segment(color=color[4], size=1, na.rm=TRUE) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, by=5000)) + 
  scale_x_continuous(breaks=seq(0, 25, by=5)) + 
  labs(x = "Age (years)", y = "Price (USD)") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
g2
ggsave(plot = g2, paste0(output, "ch15_usedcars_tree2_stepfn.png"), 
       width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "ch15_usedcars_tree2_stepfn.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(g2)
dev.off()


############
# Splits go on according to rpart defaults

cart3 <- train(
  model1, data = data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=3))

summary(cart3)
pred_cart3 <- predict(cart3, data_test)
rmse_cart3 <- sqrt(mean((pred_cart3 - data_test$price)^2))


# Tree graph
png(filename =paste0(output, "ch15_usedcars_tree3.png"))
rpart.plot(cart3$finalModel, tweak=1.2, digits=-1, extra=1)
dev.off()


# Scatterplot with step function
data$xend <- c(data$age+1)
data$yend <- predict(cart3, data.frame(age=data$age))

# Scatterplot with step function - train data
data_train$xend <- c(data_train$age+1)
data_train$yend <- predict(cart3, data.frame(age=data_train$age))
pred_cart3t <- predict(cart3, data_train)


g3<-ggplot(data = data_train, aes(x = age, y=yend, xend=xend, yend=yend)) +
  geom_point(aes(x=age , y=price), color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  geom_segment(color=color[4], size=1, na.rm=TRUE) +
  scale_y_continuous(limits=c(0, 20000), breaks=seq(0, 20000, by=5000)) + 
  scale_x_continuous(breaks=seq(0, 25, by=5)) + 
  labs(x = "Age (years)", y = "Price (USD)") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
g3
ggsave(plot = g3, paste0(output, "ch15_usedcars_tree3_stepfn.png"), 
       width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "ch15_usedcars_tree3_stepfn.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(g3)
dev.off()



#####################
# Age only, Linear regression

linreg1 <- lm(model1 , data=data_train)
linreg1
pred_linreg1 <- predict(linreg1, data_test)
rmse_linreg1 <- sqrt(mean((pred_linreg1 - data_test$price)^2))

# Scatterplot with predicted values
linreg1 <- lm(model1 , data=data_train)
pred_linreg1t<- predict(linreg1, data_train)

g4<-ggplot(data = data_train) +
  geom_point(aes(x = age, y = price), color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm = TRUE) + 
  geom_line(aes(x=age,y=pred_linreg1t), colour=color[4], size=0.5) +
  scale_x_continuous(breaks=seq(0, 25, by=5)) + 
  scale_y_continuous(breaks=seq(0, 20000, by=5000)) + 
  labs(x = "Age (years)", y = "Price (USD)") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
g4
ggsave(plot = g4, paste0(output, "ch15_usedcars_linreg1.png"), 
       width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "ch15_usedcars_linreg1.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(g4)
dev.off()



#####################
# Age only, Lowess  regression

lowess1 <- loess(model1, data=data_train)
# no prediction with loess on test
pred_lowess1 <- predict(lowess1, data_test)
rmse_lowess1 <- sqrt(mean((pred_lowess1 - data_test$price)^2))

# Scatterplot with predicted values
lowess1 <- loess(model1, data=data_train)
pred_lowess1t <- predict(lowess1, data_train)

ggplot(data = data_train, aes(x=age , y=price)) +
  geom_point(size=1.5, colour="black" ) +
  labs(x = "Age", y = "Price") +
  coord_cartesian(xlim=c(0, 25), ylim=c(0, 20000)) +
  geom_smooth(method="loess", colour="darkblue", se=F, size=1.5) + 
  theme_bg()


g5<-ggplot(data = data_train, aes(x = age, y = price)) +
  geom_point(color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(method="loess", color=color[4], se=F, size=0.5, na.rm=T)+
  scale_x_continuous(breaks=seq(0, 25, by=5)) + 
  scale_y_continuous(breaks=seq(0, 20000, by=5000)) + 
  labs(x = "Age (years)", y = "Price (USD)") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
g5
ggsave(plot = g5, paste0(output, "ch15_usedcars_lowess1.png"), 
       width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "ch15_usedcars_lowess1.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(g5)
dev.off()


########################################################
# MULTIPLE PREDICTOR VARIABLES


#####################
# Linear regression with multiple variables
model2 <- formula(price ~ age + odometer + LE + XLE + SE + cond_excellent + cond_good + cylind6 + dealer+chicago)
linreg2 <- lm(model2 , data=data_train)
linreg2
pred_linreg2 <- predict(linreg2, data_test, na.action = na.pass)
rmse_linreg2 <- sqrt(mean((pred_linreg2 - data_test$price)^2))
rmse_linreg2


# add squared for age, odometer
model3 <- formula(price ~ age + agesq+ odometer+odometersq +LE + XLE + SE + cond_excellent + cond_good + cylind6 + dealer+chicago)
linreg3 <- lm(model3 , data=data_train)
linreg3
pred_linreg3 <- predict(linreg3, data_test, na.action = na.pass)
rmse_linreg3 <- sqrt(mean((pred_linreg3 - data_test$price)^2))
rmse_linreg3

#############
# Tree

# Splits at four levels, for illustrative purposes
# (make sure it stops by setting "maxdepth" to 3)
cart4 <- train(
  model2, data=data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=4),
  na.action = na.pass)

# alternative to show the use of cp. 
# same outcome
cart4 <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.01),
  control = rpart.control(minsplit = 20),
  na.action = na.pass)



summary(cart4)
pred_cart4 <- predict(cart4, data_test, na.action = na.pass)
rmse_cart4 <- sqrt(mean((pred_cart4 - data_test$price)^2))


# Tree graph
png(filename =paste0(output, "ch15_usedcars_tree4.png"))
rpart.plot(cart4$finalModel, tweak=1.2, digits=-1, extra=1)
dev.off()

cart5 <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.002),
  control = rpart.control(minsplit = 20),
  na.action = na.pass)

print(cart5)

summary(cart5)
pred_cart5 <- predict(cart5, data_test, na.action = na.pass)
rmse_cart5 <- sqrt(mean((pred_cart5 - data_test$price)^2))

# Tree graph
png(filename =paste0(output, "ch15_usedcars_tree5.png"))
rpart.plot(cart5$finalModel, tweak=1.2, digits=-1, extra=1)
dev.off()


############################
# prune the tree 
############################


# build very large tree

cart6 <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.0001),
  control = rpart.control(minsplit = 4),
  na.action = na.pass)

#print(cart5)
# Tree graph
png(filename =paste0(output, "ch15_usedcars_tree6.png"))
rpart.plot(cart6$finalModel, tweak=1.2, digits=-1, extra=1)
dev.off()


summary(cart6)
pred_cart6 <- predict(cart6, data_test, na.action = na.pass)
rmse_cart6 <- sqrt(mean((pred_cart6 - data_test$price)^2))
rmse_cart6


# take the last model (large tree) and prunce (cut back)
pfit <-prune(cart6$finalModel, cp=0.005 )
summary(pfit)

# getting rmse 
pred_cart7 <- predict(pfit, data_test, na.action = na.pass)
rmse_cart7 <- sqrt(mean((pred_cart7 - data_test$price)^2))
rmse_cart7

printcp(pfit)
plotcp(pfit) # doesnt work
# TODO why?

# Tree graph
png(filename =paste0(output, "ch15_usedcars_tree6prune.png"))
rpart.plot(pfit, digits=-1, extra=1, tweak=1)
dev.off()



########x summary perfromance table

tab_rmse <- data.frame(
  "Model" = c("CART1", "CART2","CART3","OLS"),
  "Describe" = c("2 term. nodes", "4 term. nodes","5 term. nodes","1 variable only"),
  "RMSE" = c(rmse_cart1, rmse_cart2, rmse_cart3, rmse_linreg1)
)

print(xtable(tab_rmse, type = "latex"), file = paste0(output, "ch15_table_rmse.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)

#v3
tab_rmse <- data.frame(
  "Model" = c("CART1", "CART2","CART3","CART4", "CART5","CART6","CART7", "OLS multivar", "OLS extended"),
  "Describe" = c("2 term. nodes", "4 term. nodes","5 term. nodes","cp = 0.01","cp = 0.002","cp = 0.0001","pruned", "multi-var", "w/ squared vars"),
  "RMSE" = c(rmse_cart1, rmse_cart2, rmse_cart3, rmse_cart4,rmse_cart5,rmse_cart6,rmse_cart7, rmse_linreg2, rmse_linreg3)
)

print(xtable(tab_rmse, type = "latex"), file = paste0(output, "ch15_table_rmse_ext.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)

#############
# Varimp


# FIXME
# export also as .png
# ideally it should be like the approach we use in ch16 around line 300

plot(caret::varImp(cart4))
cairo_ps(filename = paste0(output, "ch15_varimp_cart4.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
plot(caret::varImp(cart4))
dev.off()

