############################################################
#
# DATA ANALYSIS TEXTBOOK
# Chapter 17 probability prediction and classification
# Case study
# Bisnode data

# v1.0 Zsuzsi 2019-12-23
# v1.1. gabor 2019-12-27 - runs alone, adds cor()
#
# WHAT THIS CODES DOES:
#
# looks at random forest

#
###########################################################

# Clear memory
rm(list=ls())


library(rattle)
library(tidyverse)
library(caret)
library(glmnet)
library(purrr)
library(pROC)
library(margins)
library(ranger)
library(randomForest)
library(skimr)
library(kableExtra)
library(ggraph)
library(igraph)

# CHECK WORKING DIRECTORY - CHANGE IT TO YOUR WORKING DIRECTORY
dir <-  "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"
#dir <- "/Users/zholler/Documents/Private/"
# dir <- "D:/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"

#location folders
data_in <- paste0(dir,"da_data_repo/bisnode-firms/clean/")
data_out <- paste0(dir,"da_case_studies/ch17-predicting-firm-exit/")
output <- paste0(dir,"da_case_studies/ch17-predicting-firm-exit/output/")
func <- paste0(dir, "da_case_studies/ch00-tech-prep/")

source(paste0(func, "theme_bg.R"))
source(paste0(data_out, "bisnode_helper_functions.R"))



data <- read_rds(paste0(data_out,"bisnode_firms_clean.rds"))

summary(data)

# Define variable sets ----------------------------------------------
# (making sure we use ind2_cat, which is a factor)

rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "sales", "share_eq", "subscribed_cap")
qualityvars <- c("balsheet_flag", "balsheet_length", "balsheet_notfullyear")
engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
            "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
            "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")
engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")
engvar3 <- c(grep("*flag_low$", names(data), value = TRUE),
             grep("*flag_high$", names(data), value = TRUE),
             grep("*flag_error$", names(data), value = TRUE),
             grep("*flag_zero$", names(data), value = TRUE))
d1 <-  c("d1_sales_mil_log_mod", "d1_sales_mil_log_mod_sq",
         "flag_low_d1_sales_mil_log", "flag_high_d1_sales_mil_log")
hr <- c("female", "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age",
        "flag_miss_ceo_age", "ceo_count", "labor_avg_mod",
        "flag_miss_labor_avg", "foreign_management")
firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")

# interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", "ind2_cat*labor_avg_mod")
interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")


X1 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "ind2_cat")
X2 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "fixed_assets_bs","share_eq_bs","curr_liab_bs ",   "curr_liab_bs_flag_high ", "curr_liab_bs_flag_error",  "age","foreign_management" , "ind2_cat")
X3 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar,                   d1)
X4 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, qualityvars)
X5 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, qualityvars, interactions1, interactions2)

# for LASSO
logitvars <- c("sales_mil_log", "sales_mil_log_sq", engvar, engvar2, engvar3, d1, hr, firm, qualityvars, interactions1, interactions2)

# for RF (no interactions, no modified features)
rfvars  <-  c("sales_mil", "d1_sales_mil_log", rawvars, hr, firm, qualityvars)

# separate datasets -------------------------------------------------------

set.seed(13505)

train_indices <- createDataPartition(data$default, p = 0.8, list = FALSE)
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

set.seed(7564)

# Option 1 
# Split by Gini, majority vote in each tree, 
# majority vote over trees (simple classification tree - no prob. prediction)

ranger_class <- 
  ranger(
    formula = formula(paste0("default_f ~ ", paste0(rfvars , collapse = " + "))),
    data = data_train,
    mtry = 5,
    min.node.size = 15,
    splitrule = "gini"
  )

rf_class <- 
  randomForest(
    formula = formula(paste0("default_f ~ ", paste0(rfvars , collapse = " + "))),
    data = data_train,
    mtry = 5,
    nodesize = 15,
    cutoff = c(0.5,0.5) # only affects aggregation accross trees
  )

ranger_class_pred <- predict(ranger_class, data = data_train)$predictions
rf_class_pred <- predict(rf_class, newdata = data_train, type = "response")

table(ranger_class_pred, rf_class_pred)

ranger_class_pred <- predict(ranger_class, data = data_holdout)$predictions
rf_class_pred <- predict(rf_class, newdata = data_holdout, type = "response")

table(ranger_class_pred, rf_class_pred)


# Option 2 - 
# Split by gini, majority vote in each tree, 
# average over trees (classification tree with prob. predictions)

ranger_class_pred_p <- rowMeans(predict(ranger_class, data = data_train, predict.all = TRUE)$predictions == 2)
rf_class_pred_p <- predict(rf_class, newdata = data_train, type = "prob")[,"default"]
# rf_class_pred_p <- rowMeans(predict(rf_class, newdata = data_train, predict.all = TRUE)$individual == "default")

plot(ranger_class_pred_p, rf_class_pred_p)
cor(ranger_class_pred_p, rf_class_pred_p) # does not matter


# Option 3 - Split by gini, 
# ratio of 1's in each tree, average over trees (probability option of ranger)

ranger_prob <- 
  ranger(
    formula = formula(paste0("default_f ~ ", paste0(rfvars , collapse = " + "))),
    data = data_train,
    mtry = 5,
    min.node.size = 15,
    splitrule = "gini",
    probability = TRUE
  )

ranger_prob_pred_p <- predict(ranger_prob, data = data_train)$predictions[,"default"]

plot(ranger_class_pred_p, ranger_prob_pred_p)
cor(ranger_class_pred_p, ranger_prob_pred_p)  # does not matter


# Option 4 - Split by variance, ratio of 1's in each tree, average over trees (simple regression tree)

rf_prob <- 
  randomForest(
    formula = formula(paste0("default ~ ", paste0(rfvars , collapse = " + "))),
    data = data_train,
    mtry = 5,
    nodesize = 15
  )

rf_prob_pred_p <- predict(rf_prob, newdata = data_train)

plot(rf_class_pred_p, rf_prob_pred_p)


