############################################################
#
# DATA ANALYSIS TEXTBOOK
# RANDOM FOREST
# ILLUSTRATION STUDY
# Airbnb London 2017 march 05 data

# v2.1 2019-04-24
# v2.2 2019-07-25 added todo-s
# v2.3 2019-08-12 some small changes by BG
# v2.4 2019-19-18 major update by Zsuzsi
# v2.5 2019-19-19 small fixes by BG, some minor todos
# v2.6 small fixes by Zs, better graphs
# v2.7 new model definitions, new train-holdout split
# v2.8 2019-10-04 many small changes by GB, some minor todos
# v2.9 2020-01-11 data cleaning taken out to prep code + minor graph changes + minor model changes
# v3.0 2020-01-12  table edits, revert varimp graphs to pct

#
############################################################
#
# WHAT THIS CODES DOES:
#
# Define models
# Run random forest, GBM
# Shows tuning
# Evaluate the performance
# Compares models
# Does diagnostics
###########################################################

# CLEAR MEMORY
# best to start new session
rm(list=ls())


library(rattle)
library(tidyverse)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)

# location folders
data_in <- "lab2/data/"
data_out <- "lab2/data/"
output   <- "lab2/output/"

# load ggplot theme function
source("helper_functions/theme_bg.R")
source("helper_functions/da_helper_functions.R")

source("lab2/code/Ch14_airbnb_prediction_functions.R")


#########################################################################################
#
# PART I
# Loading and preparing data ----------------------------------------------
#
#########################################################################################

# Used area
area <- "london"
data <- read_csv(paste0(data_in, "airbnb_", area, "_workfile_adj.csv")) %>%
  mutate_if(is.character, factor) %>%
  filter(!is.na(price))


count_missing_values <- function(data) {
  num_missing_values <- map_int(data, function(x) sum(is.na(x)))
  num_missing_values[num_missing_values > 0]
}

count_missing_values(data)

# Sample definition and preparation ---------------------------------------

# We focus on normal apartments, n<8
data <- data %>% filter(n_accommodates < 8)

# copy a variable - purpose later, see at variable importance
data <- data %>% mutate(n_accommodates_copy = n_accommodates)

# basic descr stat -------------------------------------------
skimr::skim(data)
summary(data$price)
Hmisc::describe(data$price)
describe(data$f_room_type)
describe(data$f_property_type)
table(data$f_number_of_reviews)

# create train and holdout samples -------------------------------------------
# train is where we do it all, incl CV

set.seed(2801)

# First pick a smaller than usual training set so that models run faster and check if works
# If works, start anew without these two lines

# try <- createDataPartition(data$price, p = 0.2, list = FALSE)
#data <- data[try, ]

# CUSTOM NEIGHBORHOOD FILTER FOR SMALLER RUNTIMES
selected_boroughs <- c("Hackney", "Camden")
data <- data %>% filter(
  f_neighbourhood_cleansed %in% selected_boroughs)
data <- data %>% 
  mutate(
    f_neighbourhood_cleansed = factor(
      f_neighbourhood_cleansed, levels = selected_boroughs))

train_indices <- createDataPartition(data$price, p = 0.7, list = FALSE)
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

# Define models: simpler, extended -----------------------------------------------------------

# Basic Variables inc neighnourhood
basic_vars <- c(
  "n_accommodates", "n_beds", "n_days_since",
  "f_property_type","f_room_type", "f_bathroom", "f_cancellation_policy", "f_bed_type",
  "f_neighbourhood_cleansed")

# reviews
reviews <- c("n_number_of_reviews", "flag_n_number_of_reviews" ,"n_review_scores_rating", "flag_review_scores_rating")

# Dummy variables
amenities <-  grep("^d_.*", names(data), value = TRUE)

#interactions for the LASSO
# from ch14
X1  <- c("n_accommodates*f_property_type",  "f_room_type*f_property_type",  "f_room_type*d_familykidfriendly",
         "d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type")
# with boroughs
X2  <- c("f_property_type*f_neighbourhood_cleansed", "f_room_type*f_neighbourhood_cleansed",
         "n_accommodates*f_neighbourhood_cleansed" )


predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews, amenities)
predictors_E <- c(basic_vars, reviews, amenities, X1,X2)


#########################################################################################
#
# PART II
# RANDOM FORESTS -------------------------------------------------------
#
#########################################################################################


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


# simpler model for model A (1)
set.seed(1234)
system.time({
rf_model_1 <- train(
  formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity"
)
})
rf_model_1

# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = c(10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)

set.seed(1234)
system.time({
rf_model_2 <- train(
  formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity"
)
})

rf_model_2

# # auto tuning first
# set.seed(1234)
# system.time({
#   rf_model_2auto <- train(
#     formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
#     data = data_train,
#     method = "ranger",
#     trControl = train_control,
#     tuneLength = 2,
#     importance = "impurity"
#   )
# })
# rf_model_2auto


# evaluate random forests -------------------------------------------------

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
    # model_2b = rf_model_2b
  )
)
summary(results)

# Save outputs -------------------------------------------------------

# Show Model B rmse shown with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
  select(mtry, min.node.size, RMSE) %>%
  rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

kable(x = rf_tuning_modelB, format = "latex", digits = 2, caption = "CV RMSE") %>%
  add_header_above(c(" ", "vars" = 3)) %>%
  cat(.,file= paste0(output,"rf_tuning_modelB.tex"))


# Turning parameter choice 1
result_1 <- matrix(c(
                     rf_model_1$finalModel$mtry,
                     rf_model_2$finalModel$mtry,
                     rf_model_1$finalModel$min.node.size,
                     rf_model_2$finalModel$min.node.size
                     ),
                    nrow=2, ncol=2,
                    dimnames = list(c("Model A", "Model B"),
                                    c("Min vars","Min nodes"))
                   )
kable(x = result_1, format = "latex", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_turning_choices.tex"))

# Turning parameter choice 2
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`)
),
nrow=2, ncol=1,
dimnames = list(c("Model A", "Model B"),
                c(results$metrics[2]))
)


kable(x = result_2, format = "latex", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_rmse.tex"))


#########################################################################################
#
# PART III
# MODEL DIAGNOSTICS -------------------------------------------------------
#
#########################################################################################


#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################
# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


# variable importance plot
# 1) full varimp plot, full
# 2) varimp plot grouped
# 3) varimp plot , top 10
# 4) varimp plot  w copy, top 10


rf_model_2_var_imp <- importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


##############################
# 1) full varimp plot, above a cutoff
##############################

# to have a quick look
plot(varImp(rf_model_2))

cutoff = 600
rf_model_2_var_imp_plot <- ggplot(rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp>cutoff,],
                                  aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[3], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[3], size=0.75) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_bg() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_plot
ggsave(paste0(output, "rf_varimp1.png"), width=mywidth_small, height=myheight_small, unit="cm", dpi=1200)
cairo_ps(filename = paste0(output, "rf_varimp1.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(rf_model_2_var_imp_plot)
dev.off()

##############################
# 2) full varimp plot, top 10 only
##############################


# have a version with top 10 vars only
rf_model_2_var_imp_plot_b <- ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[3], size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[3], size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_bg() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_plot_b
ggsave(paste0(output, "rf_varimp1_b.png"), width=mywidth_small, height=myheight_small, unit="cm", dpi=1200)
cairo_ps(filename = paste0(output, "rf_varimp1_b.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(rf_model_2_var_imp_plot_b)
dev.off()


##############################
# 2) varimp plot grouped
##############################
# grouped variable importance - keep binaries created off factors together

varnames <- rf_model_2$finalModel$xNames
f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_cancellation_policy_varnames <- grep("f_cancellation_policy",varnames, value = TRUE)
f_bed_type_varnames <- grep("f_bed_type",varnames, value = TRUE)
f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)

groups <- list(f_neighbourhood_cleansed=f_neighbourhood_cleansed_varnames,
               f_cancellation_policy = f_cancellation_policy_varnames,
               f_bed_type = f_bed_type_varnames,
               f_property_type = f_property_type_varnames,
               f_room_type = f_room_type_varnames,
               f_bathroom = "f_bathroom",
               n_days_since = "n_days_since",
               n_accommodates = "n_accommodates",
               n_beds = "n_beds")

rf_model_2_var_imp_grouped <- group.importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_2_var_imp_grouped),
                                            imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_2_var_imp_grouped_plot <-
  ggplot(rf_model_2_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[3], size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[3], size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_bg() +
  theme(axis.text.x = element_text(size=8), axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=8), axis.title.y = element_text(size=8))

rf_model_2_var_imp_grouped_plot
ggsave(paste0(output, "rf_varimp_grouped1.png"), width=mywidth_large, height=myheight_large, unit="cm", dpi=1200)
cairo_ps(filename = paste0(output, "rf_varimp_grouped1.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(rf_model_2_var_imp_grouped_plot)
dev.off()



# a side investigation -------------------------------------------------------
# with an important variable duplicated: how does it change?

##############################
# 4) varimp plot  w copy, top 10
##############################
# repeat model B
# done smartly: we take mtry min.node.size from the final model

set.seed(1234)
rf_model_2_with_copy <- train(
  formula(paste0("price ~", paste0(c(predictors_2, "n_accommodates_copy"), collapse = " + "))),
  data = data_train,
  method = "ranger",
  trControl = train_control,
  tuneGrid = data.frame(
    .mtry = rf_model_2$finalModel$mtry,
    .splitrule = "variance",
    .min.node.size = rf_model_2$finalModel$min.node.size
  ),
  importance = "impurity"
)

rf_model_2_with_copy_var_imp <- importance(rf_model_2_with_copy$finalModel)
rf_model_2_with_copy_var_imp_df <- data.frame(varname = names(rf_model_2_with_copy_var_imp),
                                              imp = rf_model_2_with_copy_var_imp) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


# only keep top 10 as well
rf_model_2_with_copy_var_imp_plot <- ggplot(rf_model_2_with_copy_var_imp_df[1:10,],
                                            aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[3], size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[3], size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme_bg() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_with_copy_var_imp_plot
ggsave(paste0(output, "rf_varimp_withcopy.png"), width=mywidth_small, height=myheight_small, unit="cm", dpi=1200)
cairo_ps(filename = paste0(output, "rf_varimp_withcopy.eps"),
         width = mywidth_small, height = myheight_small, pointsize = 8,
         fallback_resolution = 1200)
print(rf_model_2_with_copy_var_imp_plot)
dev.off()


#########################################################################################
# Partial Dependence Plots -------------------------------------------------------
#########################################################################################
pltnames_pdp <- list("n_accommodates" = "rf_pdp_n_accom",
                          "f_room_type" = "rf_pdp_roomtype")

pdp::partial(rf_model_2, pred.var = "n_accommodates", pred.grid = distinct_(data_train, "n_accommodates"), train = data_train) %>%
  autoplot( ) +
  theme_bg() +
  theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

pdp::partial(rf_model_2, pred.var = "f_room_type", pred.grid = distinct_(data_train, "f_room_type"), train = data_train) %>%
  autoplot( ) +
  theme_bg() +
  theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))


# Subsample performance: RMSE / mean(y) ---------------------------------------
# NOTE  we do this on the holdout set. 

# ---- cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))

ggplot(data_holdout_w_prediction, aes(x = price, y = price - predicted_price)) +
  geom_point(alpha = 0.01, color = color[3]) +
  geom_vline(xintercept = median(data_holdout_w_prediction[["price"]]), linetype = "dashed") +
  theme_bw()

describe(data_holdout_w_prediction$n_accommodates)

######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


b <- data_holdout_w_prediction %>%
  filter(f_neighbourhood_cleansed %in% c("Westminster", "Camden", "Kensington and Chelsea", "Tower Hamlets", "Hackney", "Newham")) %>%
  group_by(f_neighbourhood_cleansed) %>%
  summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("Apartment", "House")) %>%
  group_by(f_property_type) %>%
  summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_prediction %>%
  summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("Borough", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

options(knitr.kable.NA = '')
kable(x = result_3, format = "latex", booktabs=TRUE, linesep = "",digits = c(0,2,1,2), col.names = c("","RMSE","Mean price","RMSE/price")) %>%
  cat(.,file= paste0(output, "performance_across_subsamples.tex"))
options(knitr.kable.NA = NULL)

##########################################



#########################################################################################
#
# PART IV
# HORSERACE: compare with other models -----------------------------------------------
#
#########################################################################################



# OLS with dummies for area
# using model B

set.seed(1234)
system.time({
ols_model <- train(
  formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
  data = data_train,
  method = "lm",
  trControl = train_control
)
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))

# * LASSO
# using extended model w interactions

set.seed(1234)
system.time({
lasso_model <- train(
  formula(paste0("price ~", paste0(predictors_E, collapse = " + "))),
  data = data_train,
  method = "glmnet",
  preProcess = c("center", "scale"),
  tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.01)),
  trControl = train_control
)
})

lasso_coeffs <- coef(
    lasso_model$finalModel,
    lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`)  # the column has a name "1", to be renamed

lasso_coeffs_non_null <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]

regression_coeffs <- merge(ols_model_coeffs_df, lasso_coeffs_non_null, by = "variable", all=TRUE)
regression_coeffs %>%
  write.csv(file = paste0(output, "regression_coeffs.csv"))

# CART
set.seed(1234)
system.time({
cart_model <- train(
  formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
  data = data_train,
  method = "rpart",
  tuneLength = 10,
  trControl = train_control
)
})

fancyRpartPlot(cart_model$finalModel, sub = "")

# GBM  -------------------------------------------------------
gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = 250, # number of iterations, i.e. trees
                         shrinkage = c(0.05, 0.1), # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(1234)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model


# much more tuning

#  faster, for testing
#gbm_grid2 <-  expand.grid(interaction.depth = c( 5, 7, 9, 11), # complexity of the tree
#                          n.trees = (1:10)*50, # number of iterations, i.e. trees
#                          shrinkage = c(0.05, 0.1), # learning rate: how quickly the algorithm adapts
#                          n.minobsinnode = c(10,20) # the minimum number of training set samples in a node to commence splitting
#)


# # the next will be in final model, loads of tuning
#  gbm_grid2 <-  expand.grid(interaction.depth = c(1, 3, 5, 7, 9, 11), # complexity of the tree
#                            n.trees = (1:10)*50, # number of iterations, i.e. trees
#                            shrinkage = c(0.02, 0.05, 0.1, 0.15, 0.2), # learning rate: how quickly the algorithm adapts
#                            n.minobsinnode = c(5,10,20,30) # the minimum number of training set samples in a node to commence splitting
# )


# set.seed(1234)
# system.time({
#   gbm_model2 <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
#                       data = data_train,
#                       method = "gbm",
#                       trControl = train_control,
#                       verbose = FALSE,
#                       tuneGrid = gbm_grid2)
# })
# gbm_model2


# and get prediction rmse and add to next summary table

# ---- compare these models

final_models <-
  list("OLS" = ols_model,
  "LASSO (model w/ interactions)" = lasso_model,
  "CART" = cart_model,
  "Random forest (smaller model)" = rf_model_1,
  "Random forest" = rf_model_2,
  # "Random forest (auto tuned)" = rf_model_2auto,
  "GBM (basic tuning)"  = gbm_model
  # "GBM (broad tuning)" = gbm_model2
)

results <- resamples(final_models) %>% summary()


# Save output --------------------------------------------------------
# Model selection is carried out on this CV RMSE

result_4 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

kable(x = result_4, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
  cat(.,file= paste0(output,"horse_race_of_models_cv_rmse.tex"))


# evaluate preferred model on the holdout set -----------------------------
RMSE(predict(rf_model_2, newdata = data_holdout), data_holdout[["price"]])