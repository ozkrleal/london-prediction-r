# Cleaning London airbnb file

# IN data from web
# out: airbnb_london_cleaned.csv



#setting working directory
rm(list=ls())
#setwd("D:/Documents/Data Analysis 3 Prediction and Introduction to Machine Learning/DA3-labs-week-2/DA3-labs/assignment01/raw_data_for_assignment01")

#* need to download
#* http://data.insideairbnb.com/united-kingdom/england/london/2017-03-04/data/listings.csv.gz
#* unzip
#* save to raw data folder 


#zero step
getwd()
data<-read.csv("manchester/listings.csv.gz")
drops <- c("host_thumbnail_url","host_picture_url","listing_url","thumbnail_url","medium_url","picture_url","xl_picture_url","host_url","last_scraped","description", "experiences_offered", "neighborhood_overview", "notes", "transit", "access", "interaction", "house_rules", "host_about", "host_response_time", "name", "summary", "space", "host_location")
data<-data[ , !(names(data) %in% drops)]
saveRDS(data, file="airbnb_manchester_listing.rds")
#saveRDS(data, file="airbnb_london_listing.rds")
#write.csv(data,file="airbnb_london_listing.csv")

#####################################
#opening dfset
#df<-readRDS("airbnb_london_listing.rds")
df<-readRDS("airbnb_manchester_listing.rds")

#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]


#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)

#####################
#formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variables
for (pricevars in c("price", "weekly_price","monthly_price","security_deposit","cleaning_fee","extra_people")){
  df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
  df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]))
}



#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified","is_location_exact","requires_license","instant_bookable","require_guest_profile_picture","require_guest_phone_verification")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

#amenities
df$amenities<-gsub("\\{","",df$amenities)
df$amenities<-gsub("\\}","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))


#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49","translation missing: en.hosting_amenity_50")
df<-df[ , !(names(df) %in% drops)]

#write csv or RDS
#write.csv(df,file="airbnb_london_cleaned.csv")
#saveRDS(df,file="airbnb_london_cleaned.rds")
saveRDS(df,file="airbnb_manchester_cleaned.rds")

