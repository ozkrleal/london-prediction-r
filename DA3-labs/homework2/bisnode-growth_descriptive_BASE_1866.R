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
data_out <- "output/"

data<-read_csv(paste0(data_out,"bisnode_firms_clean.csv"))

# DESCRIBE

#####################
### look at sales
#####################
summary(data$sales)
histogram(data$sales)

data_sales<-data%>%
  filter(sales<1000000)
# Plot
ggplot(data, aes(sales))+geom_histogram(color = 'white', fill = 'blue', 
                                              position = "identity", alpha = 0.4, binwidth = 100000)+
  scale_x_continuous(labels=scales::comma)+scale_y_continuous(labels=scales::comma)+
labs(x="Sales", 
     y = "Frequency (number)")+theme_classic()+facet_wrap(data$fastgrowth)

ggsave("sales_till_mil__hist.png", plot=last_plot())

#####################
### look at profit
#####################
summary(data$profit_loss_year)
histogram(data$profit_loss_year, breaks=3, plot=F)

#looks awful, don't know how to improve yet, because ~9000 observations have profit ~43K
ggplot(data, aes(profit_loss_year))+geom_histogram( color = 'white', fill = 'blue')+
  scale_x_continuous(labels=scales::comma, breaks =40000, 50000)+scale_y_continuous(labels=scales::comma)+
  labs(x="Profit/Loss in euro", 
       y = "Frequency (number)") +
  theme_classic()+facet_wrap(data$fastgrowth)

?breaks_extended()
ggsave("profit__hist.png", plot=last_plot())

introduce(data)
plot_bar(data)
create_report(data)

################################################
# look at some cnts. key vars, functional form #
################################################

## total_assets_bs: look at distribution

# Boxplot of total_assets_bs
ggplot(data, aes(x = factor(fastgrowth), y = total_assets_bs)) +
  geom_boxplot(alpha=0.5, fill='blue') +
  labs(x = "Fast Growth",y = "Total Assets")+
  theme_bw() +
  # background_grid(major = "xy", minor="none") +
  theme(legend.position = "none")

#################################################
# Look for interactions
################################################

price_diff_by_variables2 <- function(df, factor_var, dummy_var, factor_lab, dummy_lab){
  # Looking for interactions.
  require("dplyr")
  require("ggplot2")
  library(stringr)
  factor_var <- as.name(factor_var)
  dummy_var <- as.name(dummy_var)
  
  stats <- df %>%
    group_by_(factor_var, dummy_var) %>%
    dplyr::summarize(N=n())
  
  stats[,2] <- lapply(stats[,2], factor)
  
  ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position = position_dodge(width=0.9), alpha=0.8)+
    scale_color_manual(name=dummy_lab,
                       values=c('green','blue')) +
    scale_fill_manual(name=dummy_lab,
                      values=c('green','blue')) +
    ylab('N of observations')+
    xlab(factor_lab) +
    theme_bw()
  
}

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "fastgrowth_f", "total_assets_bs", "fastgrowth", "total_assets_bs")
p2 <- price_diff_by_variables2(data, "fastgrowth_f", "total_assets_bs", "fastgrowth", "Loxbox")
#Look up canelation policy
p3 <- price_diff_by_variables2(data, "fastgrowth_f", "total_assets_bs", "fastgrowth", "total_assets_bs")
p4 <- price_diff_by_variables2(data, "fastgrowth_f", "total_assets_bs", "fastgrowth", "total_assets_bs")
#Look up property type
p5 <- price_diff_by_variables2(data, "fastgrowth_f", "total_assets_bs", "fastgrowth", "Pets allowed")
p6 <- price_diff_by_variables2(data, "fastgrowth_f", "total_assets_bs", "fastgrowth", "Cable TV")


plot_interactions <- plot_grid(p1, p2, p3, p5, p6,p4, nrow=3, ncol=2)
ggsave("airbnb_interactions.png", plot=last_plot())

?plot_grid()
write_csv(data, paste0(data_out, "airbnb_Manchester_working_file_final.csv"))

## View basic description for data is I missed anything manually
introduce(data)
create_report(data)
create_report(data, y = "fastgrowth")
