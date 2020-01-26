library(DataExplorer)

library(tidyverse)
library(stargazer)
library(Hmisc)


#location folders
#data_in  <- paste0(dir,"cases_studies_public/airbnb/clean/")
#data_out <- paste0(dir,"textbook_work/ch14/airbnb/")
#output   <- paste0(dir,"textbook_work/ch14/airbnb/output/")
#func     <- paste0(dir, "textbook_work/ch00_tech_prep/")

source("theme_bg.R")


#import data
data <- readRDS("airbnb_london_listing.rds")

#-------------------------------------------------------

table(data$property_type)
data <- data %>%
  filter(property_type %in% c("Apartment", "Condominium", "Serviced apartment", "Loft"))
# rename Loft or Serviced apartment to to House
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Serviced apartment" | data$property_type == "Loft", "Apartment", data$property_type),
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

glimpse(data)

# create dummy vars
dummies <- names(data)[seq(73,122)]
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

write_csv(data, paste0(data_out, "airbnb_london_workfile.csv"))

create_report(df)

## check report html

colnames(df)

head(df)

##drop_columns(df, "")