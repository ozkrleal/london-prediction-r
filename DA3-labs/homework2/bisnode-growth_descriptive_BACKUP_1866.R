rm(list=ls())


library(tidyverse)
library(DataExplorer)
library(ggthemes)
library(gridExtra)
library(grid)
library(directlabels)
library(dplyr)
library(ggplot2)
library(skimr)
library(lattice)
library(DataExplorer)
library(cowplot)
library(labeling)

# location folders
data_in <- "input/"
data_out <- "homework2/"

data<-read_csv(paste0(data_out,"bisnode_firms_clean.csv"))

# DESCRIBE

#####################
### look at sales
#####################
summary(data$sales)

data_sales<-data%>%
  filter(sales<1000000)
# Plot
ggplot(data, aes(sales))+geom_histogram(color = 'white', fill = 'blue', 
<<<<<<< HEAD
                                        position = "identity", alpha = 0.6)+
  scale_x_log10(labels=scales::comma)+scale_y_continuous(labels=scales::comma)+
  labs(x="Sales in euro", 
       y = "Frequency (number)")+theme_classic()+facet_wrap(data$fastgrowth)
=======
                                              position = "identity", alpha = 0.6, binwidth = 500000)+
  scale_x_continuous(labels=scales::comma)+scale_y_continuous(labels=scales::comma)+
labs(x="Sales", 
     y = "Frequency (number)")+theme_classic()+facet_wrap(data$fastgrowth)
>>>>>>> eac669b64f1fc08d43fed2666d4f9dccf5a6b79b

ggsave("sales_till_mil__hist.png", plot=last_plot())

# Plot2
ggplot(data_sales, aes(sales_mil_log))+geom_histogram(color = 'white', fill = 'blue', 
                                              position = "identity", alpha = 0.6, binwidth=0.5)+
  scale_x_continuous(labels=scales::comma)+scale_y_continuous(labels=scales::comma)+
  labs(x="Log Sales", 
       y = "Frequency (number)")+theme_classic()+facet_wrap(data_sales$fastgrowth)

ggsave("sales_till_mil_log__hist.png", plot=last_plot())
#####################
### look at profit
#####################
summary(data$profit_loss_year)

ggplot(data, aes(profit_loss_year))+geom_histogram( color = 'white', fill = 'blue')+
  scale_x_continuous(labels=scales::comma, limits =c(0, 14665))+scale_y_continuous(labels=scales::comma)+
  labs(x="Distribution of profit in euro", 
       y = "Frequency (number)") +
  theme_classic()+facet_wrap(data$fastgrowth)
ggsave("profit__hist.png", plot=last_plot())

ggplot(data, aes(profit_loss_year))+geom_histogram( color = 'white', fill = 'blue')+
  scale_x_continuous(labels=scales::comma, limits =c(-50000, 0))+scale_y_continuous(labels=scales::comma)+
  labs(x="Distribution of loses in euro", 
       y = "Frequency (number)") +
  theme_classic()+facet_wrap(data$fastgrowth)

ggsave("loss__hist.png", plot=last_plot())

<<<<<<< HEAD
=======
introduce(data)
plot_bar(data)
>>>>>>> eac669b64f1fc08d43fed2666d4f9dccf5a6b79b

################################################
# look at some cnts. key vars, functional form #
################################################

## total_assets_bs: look at distribution

# Jittered point plot of total_assets_bs
ggplot(data, aes(x = factor(fastgrowth), y = total_assets_bs)) +
  geom_point(alpha=0.15, color = "blue", position = 'jitter') +
  scale_y_continuous(labels=scales::comma, limits = c(0, 7500000)) +
  labs(x = "Fast Growth",y = "Total Assets")+
  theme_bw() +
  # background_grid(major = "xy", minor="none") +
  theme(legend.position = "none")
ggsave("jittered_assets_bs.png", plot=last_plot())


#################################################
# Look for interactions
################################################

## View correlation plot for selected (possibly high importance variables)
correlationdata <- data %>% select(total_assets_bs, d1_sales_mil_log, age, material_exp, ceo_age, inc_bef_tax, profit_loss_year, sales_mil, personnel_exp)
<<<<<<< HEAD
plot_correlation(correlationdata)
=======

introduce(data)

plot_correlation(correlationdata)
>>>>>>> eac669b64f1fc08d43fed2666d4f9dccf5a6b79b
