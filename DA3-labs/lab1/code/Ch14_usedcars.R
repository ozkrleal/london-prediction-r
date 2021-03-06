################################################################################
#
# DATA ANALYSIS TEXTBOOK
# Ch 14 log vs level
# Used car data for  Chicago
# v1.2 2009-01-09 major changes re actual data
# v1.3 2019-09-09 minor changes
# v1.4 2020-01-04 minor graph changes, small model changes

################################################################################
#
################################################################################

# WHAT THIS CODES DOES:
# Level vs log


################################################################################

# DIRECTORY SETTING

# Clear memory
rm(list=ls())

# import libraies
library(lmtest)
library(ggplot2)
library(sandwich)
library(dplyr)
library(tidyverse)
library(tidyr)
library(cowplot)
library(haven)
library(stargazer)
library(caret)

# location folders
data_in <- "lab1/data/"
data_out <- "lab1/data/"
output   <- "lab1/output/"

# load ggplot theme function
source("helper_functions/theme_bg.R")
source("helper_functions/da_helper_functions.R")


################################################################################

# DATA IMPORT

 
 # DATA IMPORT
 data <- read.csv(paste0(data_in,"used-cars_2cities_prep.csv"), stringsAsFactors = TRUE)
 
 # SAMPLE DESIGN
 
 # manage missing
 data$fuel <- fct_explicit_na(data$fuel, na_level = "Missing")
 data$condition <- fct_explicit_na(data$condition, na_level = "Missing")
 data$drive <- fct_explicit_na(data$drive, na_level = "Missing")
 data$cylinders <- fct_explicit_na(data$cylinders, na_level = "Missing")
 data$transmission <- fct_explicit_na(data$transmission, na_level = "Missing")
 data$type <- fct_explicit_na(data$type, na_level = "Missing")
 
 
 # same steps as in ch13, see code in ch13 for details
 data <- data %>% filter(Hybrid ==0) %>% dplyr::select(-Hybrid)
 data <- data %>% filter(fuel=="gas")
 data <- data %>% filter(!condition %in% c("new", "fair"))
 data <- data %>% filter(price %in% c(500:25000), odometer <=100)
 data <- data %>% filter(!(price < 1000 & (condition == "like new"|age < 8)))
 data <- data %>% filter(!(transmission == "manual"))
 data <- data %>% filter(!(type == "truck"))
 data <- data %>% dplyr::select(-pricestr)

 # focus only on Chicago
 data <- data %>%    filter(area=="chicago")
 
 
 
  # condition
 data <- data %>%
   mutate(cond_excellent = ifelse(condition == "excellent", 1,0),
          cond_good = ifelse(condition == "good", 1,0),
          cond_likenew = ifelse(condition == "like new", 1,0))
 # cylinders
 data <- data %>%
   mutate(cylind6 = ifelse(cylinders=="6 cylinders",1,0))

 
 Hmisc::describe(data$age)

 # age: quadratic, cubic
 data <- data %>%
   mutate(agesq = age^2,
          agecu = age^3)
 
 # odometer: quadratic
 data <- data %>%
   mutate(odometersq = odometer^2)
 
################################################################################

# COMPARE GRAPHS

# lowess: price

Ch14_p_age_lowess_R <- ggplot(data = data, aes(x = age, y = price)) +
  geom_point(color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(method="loess", color=color[4], se=F, size=0.5, na.rm=T)+
  scale_x_continuous(limits = c(0,30), breaks = seq(0,30, 5)) +
  labs(x = "Age (years)", y = "Price (USD)") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
Ch14_p_age_lowess_R
ggsave(paste0(output, "Ch14_p_age_lowess_R.png"), width=mywidth_small, height=myheight_small,       units = "cm",dpi = 1200)

cairo_ps(filename = paste0(output, "Ch14_p_age_lowess_R.eps"),
         width = mywidth_small, height = myheight_small,       pointsize = 8, 
         fallback_resolution = 1200)
print(Ch14_p_age_lowess_R)
dev.off()


# lowess: lnprice

Ch14_lnp_age_lowess_R <- ggplot(data = data, aes(x = age, y = lnprice)) +
  geom_point(color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(method="loess", color=color[4], se=F, size=0.5, na.rm=T)+
  scale_x_continuous(limits = c(0,30), breaks = seq(0,30, 5)) +
  scale_y_continuous(limits = c(6, 10), breaks = seq(6,10, 1)) +
  labs(x = "Age (years)", y = "ln Price") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
Ch14_lnp_age_lowess_R
ggsave(paste0(output, "Ch14_lnp_age_lowess_R.png"), width=mywidth_small, height=myheight_small,
       units = "cm", dpi = 1200)

cairo_ps(filename = paste0(output, "Ch14_lnp_age_lowess_R.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,     
         fallback_resolution = 1200)
print(Ch14_lnp_age_lowess_R)
dev.off()
#################################################################
###################################
# Linear regressions in logs now

# Model 1: Linear regression on age
model1log <- as.formula(lnprice ~ age )
# Models 2-5: no quads
model2log <- as.formula(lnprice ~ age  + odometer)
model3log <- as.formula(lnprice ~ age  + odometer +  LE + cond_excellent + cond_good + dealer)
model4log <- as.formula(lnprice ~ age  + odometer +  LE + XLE + SE + cond_likenew +
                       cond_excellent + cond_good + cylind6 + dealer)
model5log <- as.formula(lnprice ~ age +  odometer + LE*age + XLE*age + SE*age +
                       cond_likenew*age + cond_excellent*age + cond_good*age + cylind6*age + odometer*age + dealer*age)


reg1log <- lm(model1log, data=data)
reg2log <- lm(model2log, data=data)
reg3log <- lm(model3log, data=data)
reg4log <- lm(model4log, data=data)
reg5log <- lm(model5log, data=data)

# evaluation of the models

models <- c("reg1log", "reg2log","reg3log", "reg4log", "reg5log")
AIC <- c()
BIC <- c()
RMSE <- c()
RSquared <- c()
regr <- c()
k <- c()

for ( i in 1:length(models)){
  AIC[i] <- AIC(get(models[i]))
  BIC[i] <- BIC(get(models[i]))
  RMSE[i] <- RMSE(predict(get(models[i])), get(models[i])$model$lnprice)
  RSquared[i] <-summary(get(models[i]))$r.squared
  regr[[i]] <- coeftest(get(models[i]), vcov = sandwich)
  k[i] <- get(models[i])$rank -1
}

############################################################
# Linear regression evaluation


# All models
eval <- data.frame(models, k, RSquared, RMSE, BIC)
eval <- eval %>%
  mutate(models = paste0("(",gsub("reg","",models),")")) %>%
  rename(Model = models, "R-squared" = RSquared, "Training RMSE" = RMSE, "N predictors" = k)
stargazer(eval, summary = F, out=paste(output,"Ch14_bicrmselog_R.tex",sep=""), digits=2, float = F, no.space = T)
stargazer(eval, summary = F, type = "text", digits=2, float = F, no.space = T)


#################################################################
# Cross-validation

# set number of folds (4 because of small sample)
k <- 4

# need to set the same seed again and again
set.seed(13505)
cv1log <- train(model1log, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv2log <- train(model2log, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv3log <- train(model3log, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv4log <- train(model4log, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv5log <- train(model5log, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")

# calculate average rmse
cv <- c("cv1log", "cv2log", "cv3log", "cv4log", "cv5log")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2)/4)
}


# summarize results
cv_matlog <- data.frame(rbind(cv1log$resample[4], "Average"),
                        rbind(cv1log$resample[1], rmse_cv[1]),
                        rbind(cv2log$resample[1], rmse_cv[2]),
                        rbind(cv3log$resample[1], rmse_cv[3]),
                        rbind(cv4log$resample[1], rmse_cv[4]),
                        rbind(cv5log$resample[1], rmse_cv[5])
)

colnames(cv_matlog)<-c("Resample","Model1log", "Model2log", "Model3log", "Model4log", "Model5log")
cv_matlog

stargazer(cv_matlog, summary = F, digits=3, float=F, out=paste(output,"Ch14_cvmatlog_R.tex",sep=""))
stargazer(cv_matlog, summary = F, digits=3, float=F, type="text",  out=paste(output,"Ch14_cvmatlog_R.txt",sep=""))




################################################################################

# PREDICTION

# repeat what we did in ch 13, now in logs

# Prediction
#data <- data %>% dplyr::select(age, agesq, odometer, odometersq, SE, LE, XLE, cond_likenew,cond_excellent, cond_good, dealer,price, cylind6)

# Add new observation
new <- list(age=10, agesq=10^2,odometer=12,odometersq=12^2,SE=0,XLE=0, LE=1, 
            cond_likenew=0,cond_excellent=1,cond_good=0, 
            dealer=0, cylind6=0, price=NA)



#data <- rbind(data, new)


# Predict lnprice with Model 3 from ch13
# Predict price with all predictors (Model3)
reg3 <- lm(lnprice ~ age  + odometer +  LE + cond_excellent + cond_good + dealer, data=data)
reg3
# prediction
data$lnp2 <- predict(reg3, data)
rmse3 <- RMSE(data$lnp2,data$lnprice)

# prediction for new observation
predln_new <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction")
predln_new80 <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction", level=0.80)
predln_new80
lnp2_new <- predln_new$fit[[1]]

# predictions in levels
data$lnplev <- exp(data$lnp2)*exp((rmse3^2)/2)
lnp2_new_lev <- exp(lnp2_new)*exp((rmse3^2)/2)

# prediction intervals (log and level)
lnp2_PIlow <- predln_new$fit[2]
lnp2_PIhigh <- predln_new$fit[3]
lnplev_PIlow <- exp(lnp2_PIlow)*exp(rmse3^2/2)
lnplev_PIhigh <- exp(lnp2_PIhigh)*exp(rmse3^2/2)

#80%
lnp2_PIlow80 <- predln_new80$fit[2]
lnp2_PIhigh80 <- predln_new80$fit[3]
lnplev_PIlow80 <- exp(lnp2_PIlow80)*exp(rmse3^2/2)
lnplev_PIhigh80 <- exp(lnp2_PIhigh80)*exp(rmse3^2/2)



# summary of predictions and PI
sum <- matrix( c( lnp2_new, lnp2_PIlow ,lnp2_PIhigh,
                  lnp2_new_lev, lnplev_PIlow, lnplev_PIhigh) , nrow = 3 ,ncol = 2)

colnames(sum) <- c('Model in logs', 'Recalculated to level')
rownames(sum) <- c('Predicted', 'PI_low', 'PI_high')
sum
stargazer(sum, out=paste(output,"Ch14_levlog_R.tex",sep=""), type = "latex", float=F, digits=2)
stargazer(sum, out=paste(output,"Ch14_levlog_R.txt",sep=""), type = "text", digits=2)



# summary of predictions and PI 80% version
sum <- matrix( c( lnp2_new, lnp2_PIlow80 ,lnp2_PIhigh80,
                  lnp2_new_lev, lnplev_PIlow80, lnplev_PIhigh80) , nrow = 3 ,ncol = 2)

colnames(sum) <- c('Model in logs', 'Recalculated to level')
rownames(sum) <- c('Predicted', 'PI_low 80%', 'PI_high 80%')


sum
stargazer(sum, out=paste(output,"Ch14_levlog80_R.tex",sep=""), type = "latex", float=F, digits=2)
stargazer(sum, out=paste(output,"Ch14_levlog80_R.txt",sep=""), type = "text", digits=2)


################################################################################
