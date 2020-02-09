############################################################
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
library(rpart)
library(partykit)

# location folders
data_in <- "data/"
data_out <- "data/"
output   <- "output/"

# load ggplot theme function
source("helper_functions/bisnode_helper_functions.R")
source("helper_functions/theme_bg.R")

# Loading and preparing data ----------------------------------------------

data <- readRDS(paste0(output,"bisnode_firms_clean.rds"))

summary(data)

#filtered out all balsheets length that 0
data <- data %>% filter(balsheet_length != 0)

# Define variable sets ----------------------------------------------
# (making sure we use ind2_cat, which is a factor)

rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "sales_mil", "share_eq", "subscribed_cap")
qualityvars <- c("balsheet_flag", "balsheet_length")
engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "extra_exp_pl",
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
        "flag_miss_labor_avg", "foreign_management", "ceo_young")
firm <- c("age", "age2", "ind2_cat", "m_region_loc", "urban_m")

# interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", "ind2_cat*labor_avg_mod")
interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")


X1 <- c("age", "sales_mil_log", "d1_sales_mil_log", "profit_loss_year", "ind2_cat", "ceo_age", "personnel_exp")
X2 <- c("age", "sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log", "profit_loss_year", "ind2_cat", "ceo_age", "personnel_exp","total_assets_bs", "share_eq" )
X3 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, qualityvars)


# for LASSO
logitvars <- c("sales_mil_log", "sales_mil_log_sq", engvar, engvar2, engvar3, d1, hr, firm, qualityvars, interactions1, interactions2)

# for RF (no interactions, no modified features)
rfvars  <-  c("sales_mil_log", "d1_sales_mil_log", rawvars, hr, firm, qualityvars)

# baseline model is X3 (all vars, but no interactions) -------------------------------------------------------

# separate datasets -------------------------------------------------------

set.seed(13505)

train_indices <- createDataPartition(data$fastgrowth, p = 0.8, list = FALSE)
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

Hmisc::describe(data_train$fastgrowth_f)
str(data_train$fastgrowth_f)

#######################################################x
# PART I PREDICT PROBABILITIES
# Predict logit models ----------------------------------------------
#######################################################x

# 5 fold cross-validation for all models
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,# same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)

# Train Logit Models ----------------------------------------------

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3)

CV_RMSE <- list()
holdout_RMSE <- list()
logit_models <- list()
for (model_name in names(logit_model_vars)) {

  features <- logit_model_vars[[model_name]]

  set.seed(13505)
  glm_model <- train(
    formula(paste0("fastgrowth_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = binomial,
    trControl = train_control
  )

  logit_predicted_probabilities <- predict(glm_model, newdata = data_train, type = "prob")
  colname <- paste0(model_name,"_prediction")
  data_train[,colname] <-  logit_predicted_probabilities[,"fast"]
  
  CV_RMSE[[model_name]] <- glm_model$results$RMSE
  
  
  logit_predicted_probabilities_holdout <- predict(glm_model, newdata = data_holdout, type = "prob")
  
  data_holdout[,colname] <- logit_predicted_probabilities_holdout[,"fast"]
  holdout_RMSE[[model_name]] <- RMSE(data_holdout[, colname, drop=TRUE], data_holdout$fastgrowth)
  
  logit_models[[model_name]] <- glm_model
  
}

#holdout_RMSE[["X1"]]
#View(logit_predicted_probabilities_holdout[,"fast"])

# Logit lasso -----------------------------------------------------------

lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(13505)
system.time({
  logit_lasso_model <- train(
    formula(paste0("fastgrowth_f ~", paste0(logitvars, collapse = " + "))),
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

lasso_logit_predicted_probabilities <- predict(logit_lasso_model, newdata = data_train, type = "prob")
data_train[,"LASSO_prediction"] <-  lasso_logit_predicted_probabilities[,"fast"]

logit_lasso_RMSE <- logit_lasso_model$results
CV_RMSE[["LASSO"]] <- logit_lasso_RMSE %>%
  filter(lambda == best_lambda) %>%
  select(RMSE)

lasso_logit_predicted_probabilities_holdout <- predict(logit_lasso_model, newdata = data_holdout, type = "prob")
data_holdout[,"LASSO_prediction"] <- lasso_logit_predicted_probabilities_holdout[,"fast"]
holdout_RMSE[["LASSO"]] <- RMSE(data_holdout[, "LASSO_prediction", drop=TRUE],
                                      data_holdout$fastgrowth)

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

rf_model <- train(
  formula(paste0("fastgrowth_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control,
  importance = "impurity"
)


plot(varImp(rf_model), top=15)
rf_model$results

best_mtry <- rf_model$bestTune$mtry
best_min_node_size <- rf_model$bestTune$min.node.size

data_train$rf_prediction <-  rf_model$finalModel$predictions[,"fast"]

rf_predicted_probabilities_holdout <- predict(rf_model, newdata = data_holdout, type = "prob")
data_holdout$rf_prediction <- rf_predicted_probabilities_holdout[,"fast"]


CV_RMSE[["rf"]] <- RMSE(data_train$rf_prediction, data_train$fastgrowth)
holdout_RMSE[["rf"]] <- RMSE(data_holdout$rf_prediction, data_holdout$fastgrowth)


# Results -----------------------------------------------------------

# We have 5 models, (3 logit, logit lasso, RF). For each we have a 5-CV RMSE.
# We pick our preferred model based on that (not holdout).

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)
nvars[["rf"]] <- length(rfvars)


#Choosing model
model_names <- c("Logit X1","Logit X2","Logit X3",
                 "Logit LASSO","RF")

summary_results <- data.frame("Number of predictors" = unlist(nvars),
                              "CV RMSE" = unlist(CV_RMSE))
rownames(summary_results) <- model_names
summary_results

# Calibration curve -----------------------------------------------------------
# how well do estimated vs actual event probabilities relate to each other?
View(data_holdout$rf_prediction)

comparison<-as.data.frame(cbind(data_holdout$rf_prediction, data_holdout$fastgrowth))
colnames(comparison) <- c("predicted", "actual")

#Hmisc::describe(comparison)

# order observations according to predicted score from lowest to highest and group them into
# roughly equally-sized bins
num_groups <- 100

calibration <- comparison %>%
  mutate(predicted_score_group = ntile(predicted, num_groups)) %>%
  group_by(predicted_score_group) %>%
  summarise(mean_actual = mean(actual), mean_predicted = mean(predicted), num_obs = n())

rf_calib <- ggplot(calibration,aes(x = mean_actual, y = mean_predicted)) +
  geom_point(color=color[1], size=4, alpha=0.6) +
  geom_abline(intercept = 0, slope = 1, color=color[4]) +
  labs(x = "Actual event probability", y = "Predicted event probability") +
  ylim(0, 1) + xlim(0, 1) +
  theme_bg() +
  theme(axis.text.x = element_text(size=13), axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=13), axis.title.y = element_text(size=13))
rf_calib
ggsave(plot = rf_calib, paste0(output, "rf_calibration_plot.png"),
       width=mywidth_small, height=myheight_small, dpi=1200)
dev.off()

# PART IIa

# We have a loss function
# Get best threshold and expected loss ----------------------------------------------
########################################

# Introduce loss function
# relative cost of of a false negative classification (as compared with a false positive classification)
FP=10
FN=2
cost = FN/FP

# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(data_train$fastgrowth)/length(data_train$fastgrowth)

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_holdout_rocs <- list()

#Hmisc::describe(rf_model$pred)

for (model_name in names(logit_models)) {
  
  model <- logit_models[[model_name]]
  
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <- 
      model$pred %>% 
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$fast,levels = c( "no_fast", "fast"))
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
  }
  
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold
  
  #logit_cv_rocs
  # for RF
  best_tresholds_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <- 
      rf_model$pred %>% 
      filter(mtry == best_mtry,
             min.node.size == best_min_node_size,
             Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$fast,levels = c( "no_fast", "fast"))
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
  }
  
  
   # ROC curve on holdout
  roc_obj_holdout <- roc(data_holdout$fastgrowth, data_holdout[, colname, drop=TRUE],  quiet = FALSE)
  logit_holdout_rocs[[model_name]] <-  roc_obj_holdout
  
  # Get expected loss on holdout
  holdout_treshold <- coords(roc_obj_holdout, x = best_tresholds[[model_name]], input= "threshold",
                             ret="all", transpose = FALSE)
  expected_loss[[model_name]] <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$fastgrowth)
}

best_tresholds[["rf"]] <- mean(unlist(best_tresholds_cv))

# Get expected loss on holdout
holdout_treshold <- coords(roc_obj_holdout, x = best_tresholds[["rf"]], input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss[["rf"]] <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$fastgrowth)

# Create plots based on Fold5 in CV ----------------------------------------------

for (model_name in names(logit_cv_rocs)) {
  
  r <- logit_cv_rocs[[model_name]]
  best_coords <- logit_cv_threshold[[model_name]]
  createLossPlot(r, best_coords, 
                 paste0(output, model_name, "_loss_plot"))
  createRocPlotWithOptimal(r, best_coords, 
                           paste0(output, model_name, "_roc_plot"))
}


createLossPlot(roc_obj, best_treshold, paste0(output, "rf_loss_plot"))
createRocPlotWithOptimal(roc_obj, best_treshold, paste0(output, "rf_roc_plot"))

#str(cv_fold$obs)
# Summary results ---------------------------------------------------


thresholds <- lapply(best_tresholds, FUN = function(x) x[1])
expected_loss <- lapply(expected_loss, FUN = function(x) x[1])
expected_loss_summary <- data.frame("Threshold" = unlist(thresholds),
                                    "Expected loss" = unlist(expected_loss),
                                    "Holdout RMSE"=unlist(holdout_RMSE))
rownames(expected_loss_summary) <- c(model_names)
expected_loss_summary

#  Confusion table for RF
data_holdout$rf_prediction_class <- 
  ifelse(data_holdout$rf_prediction <best_tresholds[["rf"]], 
          "no_fast", "fast") %>% 
  factor(levels = c("no_fast","fast" ))

cm_object <- confusionMatrix(data_holdout$rf_prediction_class, data_holdout$fastgrowth_f, positive = "fast")
cm <- cm_object$table
cm
