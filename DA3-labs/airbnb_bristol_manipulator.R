library(DataExplorer)
library(gtools)
library(tidyverse)
library(stargazer)
library(Hmisc)

#location folders
#data_in  <- paste0(dir,"cases_studies_public/airbnb/clean/")
#data_out <- paste0(dir,"textbook_work/ch14/airbnb/")
#output   <- paste0(dir,"textbook_work/ch14/airbnb/output/")
#func     <- paste0(dir, "textbook_work/ch00_tech_prep/")
source("helper_functions/theme_bg.R")

#import data
data <- readRDS("airbnb_bristol_cleaned.rds")
#-------------------------------------------------------

table(data$property_type)

data <- data %>%
  filter(property_type %in% c("Apartment", "Condominium", "Serviced apartment", "Loft"))
# rename Loft or Serviced apartment to to House

data <- data %>%
  mutate(property_type = ifelse(data$property_type %in% c("Serviced apartment", "Condominium"), 
                                "Loft", data$property_type),
         f_property_type = factor(property_type))

data <- data %>%
  mutate(property_type = ifelse(data$property_type == 2, "Apartment", ifelse(data$property_type == 28, "Loft", data$property_type)),
         f_property_type = factor(property_type))

#Room type as factor
table(data$room_type)
data <- data %>%
  mutate(f_room_type = factor(room_type))

# Rename roomt type because it is too long
data$f_room_type2 <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt",
                                   ifelse(data$f_room_type== "Private room", "Private",
                                          ifelse(data$f_room_type== "Shared room", "Shared", "."))))

# cancellation policy as factor
table(data$cancellation_policy)
# if cancellation policy is super strict 30 or 60, rename it as strict
data <- data %>%
  mutate(
    cancellation_policy = ifelse(cancellation_policy %in% c("super_strict_30", "super_strict_60"),
                                 "strict", cancellation_policy),
    f_cancellation_policy = factor(cancellation_policy))

# bed_type and neighbourhood_cleansed as factors
table(data$bed_type)
# rename to Couch
data <- data %>%
  mutate(
    bed_type = ifelse(bed_type %in% c("Futon", "Pull-out Sofa", "Airbed"), "Couch", bed_type),
    f_bed_type = factor(bed_type),
    f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

#---------------------------------------------------------------------------------------------------------------------------
## Create Numerical variables
data <- data %>%
  mutate(
    usd_price_day = price,
    p_host_response_rate = as.numeric(host_response_rate))
# rename cleaning_fee column
data <- data %>%
  rename(usd_cleaning_fee = cleaning_fee)
#-------------------------------------------------------------------

# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews","guests_included",
                "reviews_per_month","extra_people","minimum_nights","beds")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)


#create days since first review
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))

# create dummy vars
dummies <- names(data)[seq(83,225)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))


# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price,
         neighbourhood_cleansed,cancellation_policy,room_type,property_type)

#left dollar signs outside, continous values!
data$price = as.numeric(gsub("[\\$,]", "", data$price))
data$usd_cleaning_fee = as.numeric(gsub("[\\$,]", "", data$price))
data$usd_price_day = as.numeric(gsub("[\\$,]", "", data$price))

saveRDS(data, "airbnb_bristol_workfile.rds")

##################################
# DESCRIBE

#--------------------------------
data <- readRDS("airbnb_bristol_workfile.rds")

data <- data %>%
  filter(!is.na(price)) %>% filter(price > 0)
saveRDS(data, "airbnb_bristol_workfile.rds")


#create_report(data, y = "price")

#
#####################
### look at price ###
#####################
summary(data$price)
describe(data$price)

data <- data %>%
  mutate(ln_price = log(price))

# Plot
ggplot(data, aes(price)) + geom_histogram(binwidth = 50) + theme_bg()
ggplot(data, aes(ln_price)) + geom_histogram(binwidth = 0.25) + theme_bg()

# Remove extreme values from prices
data <- data %>%
  filter(price <=1000)

# Much neater histograms
R_F14_h_lnprice <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log price") +
  theme_bg()

ggsave("homework1/bristol_R_F14_h_lnprice.png", width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)
cairo_ps(filename = "homework1/bristol_R_F14_h_lnprice.eps",
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(R_F14_h_lnprice)
dev.off()

R_F14_h_price <- ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Price") +
  theme_bg()

ggsave("homework1/bristol_R_F14_h_price.png", width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)
cairo_ps(filename = "homework1/bristol_R_F14_h_price.eps",
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(R_F14_h_price)
dev.off()


################################################
# look at some cnts. key vars, functional form #
################################################

## n_accomodates: look at distribution

data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

R_14_s_n_accommodates <- ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,800)+
  xlim(0,15)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()

ggsave("homework1/bristol_R_14_s_n_accommodates.png", width=mywidth_large, height=myheight_large, units = "cm", dpi = 1200)
cairo_ps(filename = "homework1/bristol_R_14_s_n_accommodates.eps",
         width = mywidth_large, height = myheight_large, pointsize = 12,
         fallback_resolution = 1200)
print(R_14_s_n_accommodates)
dev.off()

# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2)

summary(data$ln_price)

# Regression 1: ln price and num of accomodates and squares
lm(ln_price ~ n_accommodates + n_accommodates2, data=data)
# Regression 2: ln price and log num of accomodates
lm(ln_price ~ ln_accommodates , data=data)
# Regression 3: ln price and num of accomodates
lm(ln_price ~ n_accommodates, data=data)

## Beds
data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())
# maybe best is to have log beds
data <- data %>%
  mutate(ln_beds = log(n_beds))

## bathrooms
ggplot(data, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bathrooms") +
  theme_bg()

# Pool accomodations with 0,1,2,10 bathrooms

data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) )


data %>%
  group_by(f_bathroom) %>%
  summarise(mean_price = mean(price), n = n())

## Number of reviews
nreview_plot <- data %>%
  filter(n_number_of_reviews <100)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bg()

# number of reviews: use logs as well
data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))
hackneydata <- hackneydata %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bg()

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))
hackneydata <- hackneydata %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))

data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())
# Regression 1: log-price and number of reviews
lm(ln_price ~ f_number_of_reviews, data=data)
# Regression 2: log-price and log number of reviews
lm(ln_price ~ ln_number_of_reviews, data=data)

## Time since
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since),
    ln_days_since2 = log(n_days_since)^2,
    ln_days_since3 = log(n_days_since)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3)

# Check the effect
lndays_plot <- data %>%
  filter(data$price<=800, ln_days_since>2)

ggplot(data = lndays_plot, aes(x=ln_days_since , y=ln_price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(1,7)+
  xlim(2,7)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bg()

#-Inf values
#lm(ln_price ~ ln_days_since + ln_days_since2 + ln_days_since3, data=data)

## review score effect
ggplot(data = data, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(0,800)+
  xlim(20,100)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bg()

# Create log of review scores
data <- data %>%
  mutate(ln_review_scores_rating = log(n_review_scores_rating))

# Regression 1) ln price - num of review scores
lm(ln_price ~ n_review_scores_rating,data=data)
# Regression 2) ln price - log num of review scores
lm(ln_price ~ ln_review_scores_rating,data=data)
#leave as is

## minimum nights
lm(ln_price ~ n_minimum_nights,data=data)

# Pool and categorize the number of minimum nights: 1,2,3, 3+

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

lm(ln_price ~ f_minimum_nights,data=data)

###########################
## look at categoricals  ##
###########################

categoricals <- c("f_property_type", "f_room_type", "f_cancellation_policy", "f_bed_type")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}
#####################################

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

#saveRDS(data, "homework1/airbnb_hackney_workfile_adj.rds")

#--------------------------------------------------------------------

colnames(data)

#missing are 
plot_missing(data)
#create_report(data)
plot_correlation(data)
## check report html


saveRDS(data, "homework1/airbnb_bristol_workfile_adj.rds")
londondata<- readRDS('homework1/airbnb_london_not_hackney_workfile_adj.rds')
bristoldata<- readRDS('homework1/airbnb_bristol_workfile_adj.rds')


mixeddata <- smartbind(londondata, bristoldata)

mixeddata <- mixeddata %>% select(names(londondata))

saveRDS(mixeddata, "homework1/airbnb_londonbristol_workfile_adj.rds")
