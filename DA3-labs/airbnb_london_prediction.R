############################################################
#
# DATA ANALYSIS TEXTBOOK
# MODEL SELECTION
# CASE  STUDY
# Ch 14
# Airbnb London 2017 march 05 data

# v1.4 2019 09 05
#
############################################################
#
# WHAT THIS CODES DOES:

# Descriptive statistics and regressions
library(caret)
library(tidyverse)
library(skimr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)
library(lattice)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)

# location folders
#data_in <- "lab2/data/"
#data_out <- "lab2/data/"
#output   <- "lab2/output/"

# load ggplot theme function
source("homework1/theme_bg.R")
source("homework1/da_helper_functions.R")

source("airbnb_prediction_functions.R")

options(digits = 3)

#############
# Load data #
#############
# Used area
#airbnb_london_not_hackney_workfile_adj.rds
area <- "london_not_hackney"
data <-
  readRDS(paste0("homework1/airbnb_", area, "_workfile_adj.rds")) %>%
  mutate_if(is.character, factor)

######################
# Quick look at data #
######################
glimpse(data)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# drop if no target
data <- data %>%
  drop_na(price)


#what to do with missing values? just an example:
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds)) #assume n_beds=n_accomodates

# what are those factor variables?
data %>%
  select_if(is.factor) %>%
  skim()

#to_drop <- c("neighbourhood_cleansed", "f_neighbourhood_cleansed")
#data <- data %>%
#  select(-one_of(to_drop))

###################################
# Business logic- define our prediction problem
###################################

# Decision 1
# Size, we need a normal apartment
data <- data %>%
  filter(n_accommodates < 6)

# Decision 2
# Remove missing data, that has no score rating
data %>%
  select(n_review_scores_rating) %>%
  is.na() %>%
  sum()

data <- data %>%
  drop_na(n_review_scores_rating)


data <- data %>%
  filter(f_room_type != "Hotel room")

# save workfile
#saveRDS(data, paste0(data_out, "airbnb_hackney_work.csv"), row.names = F)
saveRDS(data, "homework1/airbnb_london_work.rds")


#####################################
# Look at some descriptive statistics
#####################################

#How is the average price changing in my district by `property_type`, `room_type` and the `bed_type`?
data %>%
  group_by(f_property_type, f_room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

data %>%
  group_by(f_bed_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

Hmisc::describe(data$price)

## Distribution of price by type below 400
datau <- subset(data, price<400)

# Density chart
plot1 <- ggplot(data = datau, aes(x=price)) +
  geom_density(aes(color=f_room_type, fill=f_room_type),  na.rm =TRUE, alpha= 0.3) +
  labs(x="Price", y="Density", color = "") +
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3]),
                     labels=c("Entire home/apt","Private room", "Shared room")) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1], color[3]),
                    labels=c("Entire home/apt","Private room", "Shared room")) +
  theme_bg() +
  background_grid(major = "y", minor = "y") +
  theme(legend.position = "bottom")
plot1
ggsave(paste0("homework1/", "plot1_R.png"),plot = plot1, width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)
cairo_ps(filename = paste0("homework1/", "plot1_R.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(plot1)
dev.off()
plot1

## Boxplot of price by room type
plot2 <- ggplot(data = datau, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3, color = viridis(3, begin=0.2, end=0.7), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_room_type),  color = viridis(3, begin=0.2, end=0.7), fill = viridis(3, begin = 0.2, end=0.7), size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price")+
  theme_bg() +
  background_grid(major = "xy", minor="none")
plot2
ggsave(paste0("homework1/", "plot2_R.png"), plot = plot2, width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)
cairo_ps(filename = paste0("homework1/", "plot2_R.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(plot2)
dev.off()
plot2

# way to combine them
ch14_priceplot1and2 <- plot_grid(plot1, plot2, nrow=2)
ggsave(paste("homework1/","ch14_priceplot1and2.png",sep=""))
cairo_ps(filename = paste0("homework1/", "ch14_priceplot1and2.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(ch14_priceplot1and2)
dev.off()
ch14_priceplot1and2

# Boxplot
plot3 <- ggplot(datau, aes(x = factor(n_accommodates), y = price, fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1])) +
  labs(x = "Accomodate Persons",y = "Price")+
  theme_bg() +
  background_grid(major = "xy", minor="none") +
  theme(legend.position = "bottom")
plot3
ggsave(paste0("homework1/", "plot3_R.png"), plot = plot3, width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)
cairo_ps(filename = paste0("homework1/", "plot3_R.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(plot3)
dev.off()
plot3

# Barchart
plot4 <- ggplot(data = datau, aes(x = factor(n_accommodates), color = f_room_type, fill = f_room_type)) +
  geom_bar(alpha=0.8, na.rm=T, width = 0.8) +
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1],  color[3])) +
  labs(x = "Accomodate Persons",y = "Frequency")+
  theme_bg() +
  background_grid(major = "y", minor="y") +
  theme(legend.position = "bottom")
plot4
ggsave(paste0("homework1/", "plot4_R.png"), plot = plot4, width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)
cairo_ps(filename = paste0("homework1/", "plot4_R.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(plot4)
dev.off()
plot4

ch14_priceplot3and4 <-  plot_grid(plot3, plot4, nrow=2)
ggsave(paste("homework1/","ch14_priceplot3and4.png",sep=""))
cairo_ps(filename = paste0("homework1/", "ch14_priceplot3and4.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(ch14_priceplot3and4)
dev.off()
ch14_priceplot3and4


#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", "n_days_since")
basic_log <- c("ln_accommodates", "ln_beds", "f_property_type", "f_room_type","ln_days_since")

# Factorized variables
basic_add <- c("f_bathroom","f_cancellation_policy","f_bed_type")
reviews <- c("f_number_of_reviews","n_review_scores_rating")
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")
poly_log <- c("ln_accommodates2","ln_days_since2","ln_days_since3")

#not use p_host_response_rate due to missing obs

# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)

#################################################
# Look for interactions
################################################

colnames(data)

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "f_room_type2", "d_balcony", "Room type", "Balcony")
p2 <- price_diff_by_variables2(data, "f_room_type2", "f_property_type", "Room type", "Property type")
#Look up canelation policy
p3 <- price_diff_by_variables2(data, "f_cancellation_policy", "d_balcony", "Cancellation policy", "Balcony")
p4 <- price_diff_by_variables2(data, "f_cancellation_policy", "d_cabletv", "Cancellation policy", "Cable TV")
#Look up property type
p5 <- price_diff_by_variables2(data, "f_property_type", "d_cats", "Property type", "Cats")
p6 <- price_diff_by_variables2(data, "f_property_type", "d_breakfast", "Property type", "Breakfast")

ch14_airbnb_interactions <- plot_grid(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
save_plot(paste0(output, "ch14_airbnb_interactions.png"), ch14_airbnb_interactions, nrow=3, ncol=2,
          base_width=mywidth_large/2, base_height=myheight_large/3, dpi = 1200)
cairo_ps(filename = paste0(output, "ch14_airbnb_interactions.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(ch14_airbnb_interactions)
dev.off()
ch14_airbnb_interactions



# dummies suggested by graphs
X1  <- c("f_room_type*f_property_type",  "f_room_type*d_balcony")

# Additional interactions of factors and dummies
X2  <- c("d_balcony*f_property_type", "d_cats*f_property_type", "d_breakfast*f_property_type")
X3  <- c(paste0("(f_property_type + f_room_type + f_cancellation_policy + f_bed_type) * (",
                paste(amenities, collapse=" + "),")"))

# Create models in levels models: 1-8
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))

# Create models in logs, models: 1-8
modellog1 <- " ~ ln_accommodates"
modellog2 <- paste0(" ~ ",paste(basic_log,collapse = " + "))
modellog3 <- paste0(" ~ ",paste(c(basic_log, basic_add),collapse = " + "))
modellog4 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log),collapse = " + "))
modellog5 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1),collapse = " + "))
modellog6 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2),collapse = " + "))
modellog7 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities),collapse = " + "))
modellog8 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities,X3),collapse = " + "))

#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180123)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)


##############################
#      cross validation      #
##############################

## K/N = 5
n_folds=5
# Create the folds
set.seed(20180124)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()

for (type in c("lev","log")) {
  for (i in (1:8)){
    model_name <-  paste0("model",type,i)
    model_pretty_name <- paste0("(",i,")")
    
    yvar <- ifelse(type=="lev","price","ln_price")
    xvars <- eval(parse(text = model_name))
    formula <- formula(paste0(yvar,xvars))
    
    # Initialize values
    rmse_train <- c()
    rmse_test <- c()
    
    model_work_data <- lm(formula,data = data_work)
    BIC <- BIC(model_work_data)
    nvars <- model_work_data$rank -1
    r2 <- summary(model_work_data)$r.squared
    
    # Do the k-fold estimation
    for (k in 1:n_folds) {
      test_i <- which(folds_i == k)
      # Train sample: all except test_i
      data_train <- data_work[-test_i, ]
      # Test sample
      data_test <- data_work[test_i, ]
      # Estimation and prediction
      model <- lm(formula,data = data_train)
      prediction_train <- predict(model, newdata = data_train)
      prediction_test <- predict(model, newdata = data_test)
      

      # Criteria evaluation
      if (type=="lev") {
        rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
        rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)
      } else {
        rmselog <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
        rmse_train[k] <- mse_log(prediction_train, data_train[,yvar] %>% pull,rmselog)**(1/2)
        rmse_test[k] <- mse_log(prediction_test, data_test[,yvar] %>% pull,rmselog)**(1/2)
      }
      
    }
    
    model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                           rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                           model_name = model_pretty_name, nvars = nvars, r2 = r2)
  }
}


t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                  "Test RMSE")

# Nice table produced and saved as .tex without \beign{table}
# -R2, BIC on full work data-n.
# -In sample rmse: average on training data; avg test : average on test data

t14_2 <- t1 %>%
  filter(grepl("lev",model_name)) %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2) <- column_names
print(xtable(t14_2, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(output, "ch14_table_fit_level.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)

t14_2_log <- t1 %>%
  filter(grepl("log",model_name)) %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2_log) <- column_names
print(xtable(t14_2_log, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(output, "ch14_table_fit_log.tex"),include.rownames=FALSE, booktabs=TRUE, floating = FALSE)


# Graph where x axis is models (noting number of vars), y axis: 1: in sample rmsse, 2: CV avg test RMSE, 3: BIC (Right scale)

t1_levels <- t1 %>%
  filter(grepl("lev",model_name)) %>%
  mutate(BIC = BIC/1000) %>%
  select("nvars", "BIC", "rmse_train", "rmse_test") %>%
  gather(var,value, BIC:rmse_test) %>%
  mutate(var = factor(var, levels = c("BIC", "rmse_train", "rmse_test"),
                      labels = c("BIC","RMSE In-sample","RMSE test CV")))

t1_logs <- t1 %>%
  filter(grepl("log",model_name)) %>%
  mutate(BIC = BIC/100) %>%
  select("nvars", "BIC", "rmse_train", "rmse_test") %>%
  gather(var,value, BIC:rmse_test) %>%
  mutate(var = factor(var, levels = c("BIC", "rmse_train", "rmse_test"),
                      labels = c("BIC","RMSE In-sample","RMSE test CV")))

model_result_plot_levels <- ggplot(data = t1_levels, aes(x = factor(nvars), y = value, color=var, group = var)) +
  geom_line() +
  scale_y_continuous(
    name = "RMSE",
    sec.axis = sec_axis(~ . * 1000 , name = "CV average BIC"),
    limits = c(20, 45)) +
  scale_x_discrete( name = "Number of vars", expand=c(0, 1)) +
  geom_dl(aes(label = var), method = list("top.points", cex=0.7)) +
  scale_colour_discrete(guide = 'none') +
  theme_bg() +
  ggtitle("Model fit measures")

ggsave(paste0(output, "ch14_airbnb_model_result_levels.png"), model_result_plot_levels,
       width=mywidth_large, height=myheight_large, dpi = 1200)
cairo_ps(filename = paste0(output, "ch14_airbnb_model_result_levels.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(model_result_plot_levels)
dev.off()

model_result_plot_logs <- ggplot(data = t1_logs, aes(x = factor(nvars), y = value, color=var, group = var)) +
  geom_line() +
  scale_y_continuous(
    name = "RMSE",
    sec.axis = sec_axis(~ . * 100 , name = "CV average BIC"),
    limits = c(10, 45)) +
  scale_x_discrete( name = "Number of vars", expand=c(0, 1)) +
  geom_dl(aes(label = var), method = list("top.points", cex=0.7)) +
  scale_colour_discrete(guide = 'none') +
  theme_bg() +
  ggtitle("Model fit measures")

ggsave(paste0(output, "ch14_airbnb_model_result_logs.png"), model_result_plot_logs,
       width=mywidth_large, height=myheight_large, dpi = 1200)
cairo_ps(filename = paste0(output, "ch14_airbnb_model_result_logs.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(model_result_plot_logs)
dev.off()




#################################
#           LASSO               #
#################################

# take model 7 and find observations where there is no missing data
vars_model_7 <- c("price", basic_lev,basic_add,reviews,poly_lev,amenities)
data_work_complete <- data_work %>%
  select_(.dots = vars_model_7) %>%
  drop_na()

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_7, "price"), collapse = " + ")))

set.seed(1234)
lasso_model <- caret::train(formula,
                            data = data_work_complete,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

print(lasso_coeffs)

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  select(RMSE)
print(lasso_cv_rmse[1, 1])



###################################################
# Diagnsotics #
###################################################

# T2
# rows: linear model #3, linear model #7 log linear model #7,  LASSO, post-Lasso OLS:

# keep vars from lasso, do OLS. show predicted mse
nonzero_lasso_vars <- lasso_coeffs %>%
  filter(coefficient != 0) %>%
  pull(variable)
# do not use the few interactions for now:
post_lasso_ols_vars <- intersect(nonzero_lasso_vars, names(data_work_complete))

model3_level <- model_results_cv[["modellev3"]][["model_work_data"]]
model7_level <- model_results_cv[["modellev7"]][["model_work_data"]]
model7_log <- model_results_cv[["modellog7"]][["model_work_data"]]
post_lasso <- lm(formula(paste("price ~ ", paste(post_lasso_ols_vars, collapse = " + "))),
                 data = data_work_complete)

# evaluate on holdout set
model3_level_work_rmse <- mse_lev(predict(model3_level, newdata = data_work), data_work[,"price"] %>% pull)**(1/2)
model3_level_holdout_rmse <- mse_lev(predict(model3_level, newdata = data_holdout), data_holdout[,"price"] %>% pull)**(1/2)

model7_level_work_rmse <- mse_lev(predict(model7_level, newdata = data_work), data_work[,"price"] %>% pull)**(1/2)
model7_level_holdout_rmse <- mse_lev(predict(model7_level, newdata = data_holdout), data_holdout[,"price"] %>% pull)**(1/2)

rmselog <- mse_lev(predict(model7_log, newdata = data_work), data_work[,"ln_price"] %>% pull)**(1/2)
model7_log_work_rmse <- mse_log(predict(model7_log, newdata = data_work), data_work[,"ln_price"] %>% pull,rmselog)**(1/2)
model7_log_holdout_rmse <- mse_log(predict(model7_log, newdata = data_holdout), data_holdout[,"ln_price"] %>% pull,rmselog)**(1/2)

data_holdout_complete <- data_holdout %>%
  select_(.dots = vars_model_7) %>%
  drop_na()

lasso_holdout_rmse <- RMSE(predict(lasso_model, newdata = data_holdout_complete), data_holdout_complete$price)
lasso_work_rmse <- RMSE(predict(lasso_model, newdata = data_work_complete), data_work_complete$price)

post_lasso_holdout_rmse <- RMSE(predict(post_lasso, newdata = data_holdout_complete), data_holdout_complete$price)
post_lasso_work_rmse <- RMSE(predict(post_lasso, newdata = data_work_complete), data_work_complete$price)


t2 <- data.frame(
  "model_name" = c("Model 3, Levels", "Model 7, Levels", "Model 7, Logs", "LASSO", "post-Lasso OLS"),
  "rmse_work" = c(model3_level_work_rmse, model7_level_work_rmse, model7_log_work_rmse, lasso_work_rmse, post_lasso_work_rmse),
  "rmse_holdout" = c(model3_level_holdout_rmse, model7_level_holdout_rmse, model7_log_holdout_rmse, lasso_holdout_rmse, post_lasso_holdout_rmse))
t2
print(xtable(t2, type = "latex"), file = paste0(output, "ch14_table_rmse_holdout.tex"),include.rownames=FALSE, digits=2, booktabs=TRUE, floating = FALSE)



model7_level_holdout_rmse <- mse_lev(predict(model7_level, newdata = data_holdout), data_holdout[,"price"] %>% pull)**(1/2)

p2 <- predict(model7_level, data_holdout)
resid_p <- p2-data_holdout$price
summary(resid_p)

pred2_new <- predict(model7_level, data_holdout ,se.fit = TRUE, interval = "prediction")
p2<- pred2_new$fit
sum(p2)

sum1 <- cbind(t(p1), t(p2))
colnames(sum1) <- c('Model1', 'Model3')
rownames(sum1) <- c('Predicted', 'PI_low', 'PI_high')

sum1



###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################


# Target variable
Ylev <- data_holdout[["price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -2 * sdY
meanY_p2SE <- meanY + 2 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])




# Logged target variable
# model log 7 on log price
Ylog <- data_holdout[["ln_price"]]
predictionlog_test <- predict(model7_log, newdata = data_holdout)
predictionlog_test2 <- exp(predictionlog_test) * exp((rmselog)^2/2)

# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, ylog=Ylog, predlev=predictionlev_holdout[,"fit"] , predlog=predictionlog_test2)
# Check the differences
d$elev <- d$ylev - d$predlev



# Plot predicted vs price
level_vs_pred <- ggplot(data = d[(d$ylev<400),]) +
  geom_point(aes(y=ylev, x=predlev), color = color[3], size = 1.5,  shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[1], se=F, size=1, na.rm=T)+
  scale_x_continuous(limits=c(0, 300), breaks=seq(0, 300, by=50)) +
  scale_y_continuous(limits=c(0, 300), breaks=seq(0, 300, by=50)) +
  labs(y = "Price", x = "Predicted price") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
level_vs_pred
ggsave(paste0(output, "level_vs_pred.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)
cairo_ps(filename = paste0(output, "level_vs_pred.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(level_vs_pred)
dev.off()

# calculate  PI for modellev7 for n_accomodate
# show a graph with x: n_accomodate, y: price
# graph: F14_CI_n_accomodate

# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])


predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(n_accommodates) %>%
  summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
            conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

F14_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(n_accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1] ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  # geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Price") +
  scale_x_discrete(name = "Number of people accomodated") +
  scale_color_manual(values=c(color[4], color[4])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="bottom")
F14_CI_n_accomodate

ggsave(paste0(output, "F14_CI_n_accomodate.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)

cairo_ps(filename = paste0(output, "F14_CI_n_accomodate.eps"),
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(F14_CI_n_accomodate)
dev.off()




###############################################
###############################################
###############################################
# not used
###############################################
###############################################
###############################################
###############################################

# Level prediction against the errors
ggplot(data =d, aes(x=ylev, y=elev)) +
  geom_point(color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(method="lm", color=color[3], se=F, size=1, na.rm=T)+
  labs(x = "Price", y = "Residual") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
#ggsave(paste0(output, "F14_preerr1.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)




# Plot the Level and Log Prediction less than 400
ggplot(data = d[(d$ylev<400),]) +
  geom_point(aes(x = ylev, y = predlev), color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_point(aes(x = ylev, y = predlog), color = color[2], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(aes(x = ylev, y = predlev), method="lm", color=color[1], se=F, size=1, na.rm=T)+
  geom_smooth(aes(x = ylev, y = predlog), method="lm", color=color[2], se=F, size=1, na.rm=T) +
  labs(x = "Price", y = "Predicted price") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
#ggsave(paste0(output, "ch14_log_vs_lin_all.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)


# Plot the Level and Log Prediction within 0.5% and 95%
ggplot(data =d[(d$ylev>Y5p) & (d$ylev<Y95p),]) +
  geom_point(aes(x = ylev, y = predlev), color = color[1], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_point(aes(x = ylev, y = predlog), color = color[2], size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(aes(x = ylev, y = predlev), method="lm", color=color[1], se=F, size=1, na.rm=T)+
  geom_smooth(aes(x = ylev, y = predlog), method="lm", color=color[2], se=F, size=1, na.rm=T) +
  labs(x = "Price", y = "Predicted price") +
  theme_bg() +
  background_grid(major = "xy", minor="none")
#ggsave(paste0(output, "log_vs_lin_95.png"), width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)



###################################################
# Post Lasso OLS #
###################################################

# keep vars from lasso, do OLS. show predicted mse
nonzero_lasso_vars <- lasso_coeffs %>%
  filter(coefficient != 0) %>%
  pull(variable)
# do not use the few interactions for now:
post_lasso_ols_vars <- intersect(nonzero_lasso_vars, names(data_work_complete))

fit_control <- trainControl(method = "cv", number = 5)

set.seed(1234)  # set the same seed as before so that CV folds are EXACTLY the same
post_lasso_ols_model <- caret::train(
  formula(paste("price ~ ", paste(post_lasso_ols_vars, collapse = " + "))),
  data = data_work_complete,
  method = "lm",
  trControl = fit_control
)

post_lasso_ols_rmse <- post_lasso_ols_model$results[["RMSE"]]
post_lasso_ols_rmse #RMSE on CV

