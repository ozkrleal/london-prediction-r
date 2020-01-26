############################################################
#
# DATA ANALYSIS TEXTBOOK
# Chapter 17 probability prediction and classification
# Case study
# Bisnode data

#
# version 1.6 2019-11-08 # substantially new version with new structure
# version 1.7 2019-11-17 # added new structure, RF. helper fn
# version 1.8 2019-11-18 # change some minor bits, tables.
# version 1.9 2019-11-19 # minor fixes
# version 1.10 2019-11-20 # minor changes, todos added
# version 1.11 2019-11-29 # includes earlier code re showing threshold
# version 1.12 2019-12-15 # minor edits, mostly graphs
# Version 1.13 2019-12-17 # rf thinking added
# version 1.14 2019-12-22 # zsuzsi redid RF + minor changes in graphix
# version 1.15 2019-12-27 # cart v2
# version 1.16 2020-01-03 # threshold search now on test set
# version 1.17 2020-01-16 # loss fn is 1,10 (+ve) 

#

# This code is co-authored with Zsuzsa Holler and Jeno Pal
############################################################
#
# WHAT THIS CODES DOES:
#
# Sets up models
# Probability prediction
# classification with loss fn - thershold search, expected loss
# classification without loss fn - ROC, AUC
# adds random forest

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
library(skimr)
library(kableExtra)
library(rpart)
library(partykit)

# location folders
data_in <- "lab3/data/"
data_out <- "lab3/data/"
output   <- "lab3/output/"

# load ggplot theme function
source("helper_functions/theme_bg.R")
source("helper_functions/da_helper_functions.R")

source("lab3/code/bisnode_helper_functions.R")


# THIS IS THE SECOND PART OF THE ch17 CODE
# USES INTERMEDIATE OUTPUT by ch17-firm-exit-data-prep.R


# Loading and preparing data ----------------------------------------------

# Use R format so it keeps factor definitions
# data <- read_csv(paste0(data_out,"bisnode_firms_clean.csv"))
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


# Check simplest model X1
ols_modelx1 <- lm(formula(paste0("default ~", paste0(X2, collapse = " + "))),
                data = data)
summary(ols_modelx1)

glm_modelx2 <- glm(formula(paste0("default ~", paste0(X2, collapse = " + "))),
                 data = data, family = "binomial")
summary(glm_modelx2)

#calculate average marginal effects (dy/dx) for logit
# mx2 <- margins(glm_modelx2)
# 
# sum_table <- summary(glm_modelx2) %>%
#   coef() %>%
#   as.data.frame() %>%
#   select(Estimate, `Std. Error`) %>%
#   mutate(factor = row.names(.)) %>%
#   merge(summary(mx2)[,c("factor","AME")])
# 
# kable(x = sum_table, format = "latex", digits = 3,
#       col.names = c("Variable", "Coefficient", "SE", "dx/dy"),
#       caption = "Average Marginal Effects (dy/dx) for Logit Model") %>%
#   cat(.,file= paste0(output,"AME_logit_X2.tex"))


# baseline model is X4 (all vars, but no interactions) -------------------------------------------------------

ols_model <- lm(formula(paste0("default ~", paste0(X4, collapse = " + "))),
                data = data)
summary(ols_model)

glm_model <- glm(formula(paste0("default ~", paste0(X4, collapse = " + "))),
                 data = data, family = "binomial")
summary(glm_model)

#calculate average marginal effects (dy/dx) for logit
# vce="none" makes it run much faster, here we do not need variances
# 
# m <- margins(glm_model, vce = "none")
# 
# sum_table2 <- summary(glm_model) %>%
#   coef() %>%
#   as.data.frame() %>%
#   select(Estimate, `Std. Error`) %>%
#   mutate(factor = row.names(.)) %>%
#   merge(summary(m)[,c("factor","AME")])
# 
# kable(x = sum_table2, format = "latex", digits = 3,
#       col.names = c("Variable", "Coefficient", "SE", "dx/dy"),
#       caption = "Average Marginal Effects (dy/dx) for Logit Model") %>%
#   cat(.,file= paste0(output,"AME_logit_X4.tex"))


# separate datasets -------------------------------------------------------

set.seed(13505)

# note: train share is low in order to speed up computations
train_indices <- createDataPartition(data$default, p = 0.2, list = FALSE)
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

Hmisc::describe(data$default_f)

#######################################################x
# PART I PREDICT PROBABILITIES
# Predict logit models ----------------------------------------------
#######################################################x

# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)


# Train Logit Models ----------------------------------------------

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5)

CV_RMSE <- list()
holdout_RMSE <- list()
logit_models <- list()

for (model_name in names(logit_model_vars)) {

  features <- logit_model_vars[[model_name]]

  set.seed(13505)
  glm_model <- train(
    formula(paste0("default_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = binomial,
    trControl = train_control
  )

  colname <- paste0(model_name,"_prediction")

  logit_predicted_probabilities <- predict(glm_model, newdata = data_train, type = "prob")
  data_train[,colname] <-  logit_predicted_probabilities[,"default"]

  CV_RMSE[[model_name]] <- glm_model$results$RMSE

  logit_predicted_probabilities_holdout <- predict(glm_model, newdata = data_holdout, type = "prob")
  data_holdout[,colname] <- logit_predicted_probabilities_holdout[,"default"]

  holdout_RMSE[[model_name]] <- RMSE(data_holdout[, colname, drop=TRUE], data_holdout$default)
  logit_models[[model_name]] <- glm_model

}

# Logit lasso -----------------------------------------------------------

lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(13505)
system.time({
  logit_lasso_model <- train(
    formula(paste0("default_f ~", paste0(logitvars, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})

tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))
# write.csv(lasso_coeffs, paste0(output, "lasso_logit_coeffs.csv"))


lasso_logit_predicted_probabilities <- predict(logit_lasso_model, newdata = data_train, type = "prob")
data_train[,"LASSO_prediction"] <-  lasso_logit_predicted_probabilities[,"default"]

logit_lasso_RMSE <- logit_lasso_model$results
CV_RMSE[["LASSO"]] <- logit_lasso_RMSE %>%
  filter(lambda == best_lambda) %>%
  select(RMSE)

lasso_logit_predicted_probabilities_holdout <- predict(logit_lasso_model, newdata = data_holdout, type = "prob")
data_holdout[,"LASSO_prediction"] <- lasso_logit_predicted_probabilities_holdout[,"default"]

holdout_RMSE[["LASSO"]] <- RMSE(data_holdout[, "LASSO_prediction", drop=TRUE],
                                      data_holdout$default)
view(holdout_RMSE)

# Results -----------------------------------------------------------

# We have 6 models, (5 logit and the logit lasso). For each we have a 5-CV RMSE.
# We pick our preferred model based on that (not holdout).

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_results <-
  data.frame("Number of predictors" = unlist(nvars),
             "CV RMSE" = unlist(CV_RMSE),
             "Holdout RMSE" = unlist(holdout_RMSE))


kable(x = logit_results, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors","CV RMSE","Holdout RMSE")) %>%
  cat(.,file= paste0(output, "logit_results.tex"))

# Calibration curve -----------------------------------------------------------
# how well do estimated vs actual event probabilities relate to each other?

actual_vs_predicted_logit <- data_holdout %>%
  select(actual = default, predicted = X4_prediction)

# order observations according to predicted score from lowest to highest and group them into
# roughly equally-sized bins
num_groups <- 20

calibration_logit <- actual_vs_predicted_logit %>%
  mutate(predicted_score_group = ntile(predicted, num_groups)) %>%
  group_by(predicted_score_group) %>%
  summarise(mean_actual = mean(actual), mean_predicted = mean(predicted), num_obs = n())

logit_calib <- ggplot(calibration_logit,aes(x = mean_actual, y = mean_predicted)) +
  geom_point(color=color[1], size=4, alpha=0.8) +
  geom_abline(intercept = 0, slope = 1, color=color[4]) +
  labs(x = "Actual event probability", y = "Predicted event probability") +
  ylim(0, 1) + xlim(0, 1) +
  theme_bg() +
  theme(axis.text.x = element_text(size=13), axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))
logit_calib
ggsave(plot = logit_calib, paste0(output, "logit_X4_calibration_plot.png"),
       width=mywidth_small, height=myheight_small, dpi=1200)
cairo_ps(filename = paste0(output, "logit_X4_calibration_plot.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(logit_calib)
dev.off()

#############################################x
# PART Ib
# Look at why we need a threshold for classification
########################################

# take logit 4 ------------------------------

# default: the threshold 0.5 is used to convert probabilities to binary classes
logit_class_prediction <- predict(logit_models[["X4"]], newdata = data_holdout)
summary(logit_class_prediction)

# confusion matrix: summarize different type of errors and successfully predicted cases
# positive = "yes": explicitly specify the positive case
cm_object1 <- confusionMatrix(logit_class_prediction, data_holdout$default_f, positive = "default")
cm1 <- cm_object1$table
cm1

# we can apply different thresholds

# 0.5 same as before
holdout_prediction <-
  ifelse(data_holdout$X4_prediction < 0.5, "no_default", "default") %>%
  factor(levels = c("no_default", "default"))
cm_object1b <- confusionMatrix(holdout_prediction,data_holdout$default_f)
cm1b <- cm_object1b$table
cm1b

# a sensible choice: mean of predicted probabilities
mean_predicted_default_prob <- mean(data_holdout$X4_prediction)
mean_predicted_default_prob
holdout_prediction <-
  ifelse(data_holdout$X4_prediction < mean_predicted_default_prob, "no_default", "default") %>%
  factor(levels = c("no_default", "default"))
cm_object2 <- confusionMatrix(holdout_prediction,data_holdout$default_f)
cm2 <- cm_object2$table
cm2

# show confusion tables for different thresholds
thresholds <- seq(0.05, 0.75, by = 0.05)

cm <- list()
true_positive_rates <- c()
false_positive_rates <- c()
for (thr in thresholds) {
  holdout_prediction <- ifelse(data_holdout[,"X4_prediction"] < thr, "no_default", "default") %>%
    factor(levels = c("no_default", "default"))
  cm_thr <- confusionMatrix(holdout_prediction,data_holdout$default_f)$table
  cm[[as.character(thr)]] <- cm_thr
  true_positive_rates <- c(true_positive_rates, cm_thr["default", "default"] /
                             (cm_thr["default", "default"] + cm_thr["no_default", "default"]))
  false_positive_rates <- c(false_positive_rates, cm_thr["default", "no_default"] /
                              (cm_thr["default", "no_default"] + cm_thr["no_default", "no_default"]))
}

# display the trade-off between false positive rate and true positive rate
tpr_fpr_for_thresholds <- tibble(
  "threshold" = thresholds,
  "true_positive_rate" = true_positive_rates,
  "false_positive_rate" = false_positive_rates
)

discrete_roc_plot <- ggplot(
  data = tpr_fpr_for_thresholds,
  aes(x = false_positive_rate, y = true_positive_rate, color = threshold)) +
  labs(x = "False positive rate (1 - Specificity)", y = "True positive rate (Sensitivity)") +
  geom_point(size=3, alpha=0.8) +
  lims(x = c(0, 1), y = c(0, 1)) +
  scale_color_viridis(option = "D", direction = -1) +
  theme_bg()+
  theme(axis.text.x = element_text(size=13), axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))
discrete_roc_plot
ggsave(plot = discrete_roc_plot, paste0(output, "roc_discrete.png"),
       width=mywidth_small, height=myheight_small, dpi=1200)
cairo_ps(filename = paste0(output, "roc_discrete.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 12,
         fallback_resolution = 1200)
print(discrete_roc_plot)
dev.off()

# ROC curve and AUC computed below in PART III

#############################################x
# PART II.
# We have a loss function
# Get best threshold and expected loss ----------------------------------------------
########################################

# Introduce loss function
# relative cost of of a false negative classification (as compared with a false positive classification)
FP=1
FN=10
cost = FN/FP

# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(data_train$default)/length(data_train$default)

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_holdout_rocs <- list()

for (model_name in names(logit_models)) {

  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <- 
      model$pred %>% 
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$default)
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
  }
  
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold

  # ROC curve on holdout
  roc_obj_holdout <- roc(data_holdout$default, data_holdout[, colname, drop=TRUE])
  logit_holdout_rocs[[model_name]] <-  roc_obj_holdout

  # Get expected loss on holdout
  holdout_treshold <- coords(roc_obj_holdout, x = best_tresholds[[model_name]], input= "threshold",
                             ret="all", transpose = FALSE)
  expected_loss[[model_name]] <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$default)
}

# Create plots based on Fold5 in CV ----------------------------------------------

for (model_name in names(logit_cv_rocs)) {

  r <- logit_cv_rocs[[model_name]]
  best_coords <- logit_cv_threshold[[model_name]]
  createLossPlot(r, best_coords, 
                 paste0(output, model_name, "_loss_plot"))
  createRocPlotWithOptimal(r, best_coords, 
                paste0(output, model_name, "_roc_plot"))
}

############################################
# PART III HAVE
# ROC and AUC
# we do not have a loss fn
############################################

auc <- list()


# Nice ROC plots on holdout sample
for (model_name in names(logit_holdout_rocs)) {

  r <- logit_holdout_rocs[[model_name]]
  auc[[model_name]] <- as.numeric(r$auc)

  createRocPlot(r, paste0(output, model_name, "_roc_plotv2"))
}

# Table with models and AUC
logit_auc <- data.frame("AUC" = unlist(auc))

kable(x = logit_auc, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("AUC")) %>%
  cat(.,file= paste0(output, "logit_auc.tex"))

# Summary table
expected_loss <- lapply(expected_loss, FUN = function(x) x[1])

logit_results2 <-
  data.frame("Number of predictors" = unlist(nvars),
             "Holdout RMSE" = unlist(holdout_RMSE),
             "Holdout expected loss" = unlist(expected_loss),
             "AUC" = unlist(auc))


kable(x = logit_results2, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors","Holdout RMSE","Holdout expected loss","AUC")) %>%
  cat(.,file= paste0(output, "logit_results2.tex"))


#################################################
# PREDICTION WITH RANDOM FOREST
#################################################

# -----------------------------------------------
# RANDOM FOREST GRAPH EXAMPLE
# -----------------------------------------------

set.seed(13505)
rf_for_graph <- 
  rpart(
    formula = default_f ~ sales_mil + profit_loss_year+ foreign_management,
    data = data_train, 
    control = rpart.control(cp = 0.0028, minbucket = 100)
  )
party_rf <- as.party(rf_for_graph)

png(filename = paste0(output, "tree_plot.png"),
    width = mywidth_small, height = myheight_small, units="cm", res=1200, pointsize = 3
) 
plot(party_rf)
dev.off()

cairo_ps(filename = paste0(output, "tree_plot.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
plot(party_rf)
dev.off()

#################################################
# Probability forest 
# Split by gini, ratio of 1's in each tree, average over trees
#################################################

# 5 fold cross-validation

train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
train_control$verboseIter <- TRUE

tune_grid <- expand.grid(
  .mtry = c(5, 6, 7),
  .splitrule = "gini",
  .min.node.size = c(10, 15)
)

# getModelInfo("ranger")
set.seed(13505)
rf_model_p <- train(
  formula(paste0("default_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)

rf_model_p$results

best_mtry <- rf_model_p$bestTune$mtry
best_min_node_size <- rf_model_p$bestTune$min.node.size

data_train$rf_p_prediction <-  rf_model_p$finalModel$predictions[,"default"]
rf_predicted_probabilities_holdout <- predict(rf_model_p, newdata = data_holdout, type = "prob")
data_holdout$rf_p_prediction <- rf_predicted_probabilities_holdout[,"default"]

# performance metrics: ROC curve, RMSE, AUC ------------------------------------

holdout_RMSE[["rf_p"]] <- RMSE(data_holdout$rf_p_prediction, data_holdout$default)

rf_holdout_rocs <- list()
# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$default, data_holdout$rf_p_prediction)
rf_holdout_rocs[["rf_p"]] <- roc_obj_holdout

auc[["rf_p"]] <- as.numeric(roc_obj_holdout$auc)

createRocPlot(roc_obj_holdout, paste0(output, "rf_p", "_roc_plotv2"))

# loss fn, threshold search, best threshold, classification, expected loss -----

best_tresholds_cv <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <- 
    rf_model_p$pred %>% 
    filter(mtry == best_mtry,
           min.node.size == best_min_node_size,
           Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$default)
  best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                          best.method="youden", best.weights=c(cost, prevelance))
  best_tresholds_cv[[fold]] <- best_treshold$threshold
}

best_tresholds[["rf_p"]] <- mean(unlist(best_tresholds_cv))

# Get expected loss on holdout
holdout_treshold <- coords(roc_obj_holdout, x = best_tresholds[["rf_p"]], input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss[["rf_p"]] <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$default)

# Create plots - this is for Fold5 currently

createLossPlot(roc_obj, best_treshold, paste0(output, "rf_p_loss_plot"))
createRocPlotWithOptimal(roc_obj, best_treshold, paste0(output, "rf_p_roc_plot"))

#  Confusion table
data_holdout$rf_p_prediction_class <- 
  ifelse(data_holdout$rf_p_prediction > best_tresholds[["rf_p"]], 
         "default", "no_default") %>% 
  factor(levels = c("no_default", "default"))

cm_object3 <- confusionMatrix(data_holdout$rf_p_prediction_class,data_holdout$default_f)
cm3 <- cm_object3$table
cm3

#################################################
# Classification forest
# Split by Gini, majority vote in each tree, majority vote over trees
#################################################

train_control <- trainControl(
  method = "cv",
  n = 5
)
train_control$verboseIter <- TRUE

set.seed(13505)
rf_model_f <- train(
  formula(paste0("default_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)

rf_model_f$results

data_train$rf_f_prediction_class <-  predict(rf_model_f,type = "raw")
data_holdout$rf_f_prediction_class <- predict(rf_model_f, newdata = data_holdout, type = "raw")

#We use predicted classes to calculate expected loss based on our loss fn
fp <- sum(data_holdout$rf_f_prediction_class == "default" & data_holdout$default_f == "no_default")
fn <- sum(data_holdout$rf_f_prediction_class == "no_default" & data_holdout$default_f == "default")
expected_loss[["rf_f"]] <- (fp*FP + fn*FN)/length(data_holdout$default)

#  Confusion table
cm_object4 <- confusionMatrix(data_holdout$rf_f_prediction_class,data_holdout$default_f)
cm4 <- cm_object4$table
cm4

# Summary results ---------------------------------------------------

model_names <- c("Logit X1","Logit X2","Logit X3","Logit X4","Logit X5",
                 "Logit LASSO","RF probability")
expected_loss <- lapply(expected_loss, FUN = function(x) x[1])
nvars[["rf_p"]] <- length(rfvars)

summary_results <- data.frame("Number of predictors" = unlist(nvars),
           "Holdout RMSE" = unlist(holdout_RMSE),
           "AUC" = unlist(auc))
rownames(summary_results) <- model_names

kable(x = summary_results, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors", "Holdout RMSE", "AUC")) %>%
  cat(.,file= paste0(output, "summary_results.tex"))

thresholds <- lapply(best_tresholds, FUN = function(x) x[1])
thresholds[["rf_f"]] <- 0.5

expected_loss_summary <- data.frame("Threshold" = unlist(thresholds),
                                    "Expected loss" = unlist(expected_loss))
rownames(expected_loss_summary) <- c(model_names, "RF class")

kable(x = expected_loss_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Threshold", "Expected loss")) %>%
  cat(.,file= paste0(output, "expected_loss_summary.tex"))



