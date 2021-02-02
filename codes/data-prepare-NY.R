# Data preparation

#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(stargazer)
library(Hmisc)

getwd()
setwd("D:/CEU/winter semester/data-analysis-3/da3_assignment1/")

# set data dir, load theme and functions
source("codes/theme_bg.R")
source("codes/da_helper_functions.R")

data_in <- "data/clean/"
data_out <- "data/out/"

# things are going to be displayed at the precision of 3 digits
options(digits = 3)


#-------------------------------------------------------
# Import data
data <- read_csv(paste(data_in,"airbnb_ny_cleaned.csv", sep = ""))

table(data$property_type)

# data[data$property_type >1000,]

# Entire apartment - 15577
# Private room in apartment - 12007
# Private room in house - 1871
# Private room in townhouse - 995
# Entire condominium - 978
# Entire house - 900
# Entire townhouse - 593

tail(names(sort(table(data$property_type))), 7)

######

# keep if property type is Entire apartment, Private room in apartment, Entire condominium
data <- data %>%
  filter(property_type %in% c("Entire apartment", "Private room in apartment", "Entire condominium"))

data <- data %>%
  mutate(
    f_property_type = factor(property_type))

#Room type as factor
table(data$room_type)

data <- data %>%
  mutate(f_room_type = factor(room_type))

data$f_room_type2 <- factor(ifelse(data$f_room_type== "Entire home/apt", "Entire/Apt",
                                   ifelse(data$f_room_type== "Private room", "Private", ".")))


# neighbourhood_cleansed as factors
table(data$neighbourhood_cleansed)

data <- data %>%
  mutate(
    f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

table(data$bathrooms_text)

unique_bath <- unique(data$bathrooms_text)
unique_bath_c <- sort(unique_bath, decreasing = FALSE)

data$f_bathrooms_text <- factor(data$bathrooms_text, order = TRUE,
                             levels = c("0 shared baths", "0 baths", "Shared half-bath", "Half-bath", 
                                        "Private half-bath", "1 bath", "1 private bath", "1 shared bath", 
                                        "1.5 baths", "1.5 shared baths", "2 baths",  
                                        "2 shared baths", "2.5 baths", "2.5 shared baths", 
                                        "3 baths", "3 shared baths", "3.5 baths", 
                                        "3.5 shared baths", "4 baths","4 shared baths", 
                                        "4.5 baths", "5 shared baths", "6 baths", "8 baths"))



#---------------------------------------------------------------------------------------------------------------------------

table(data$host_response_rate)

## Create Numerical variables

# data <- data %>%
#   mutate(
#     usd_price_day = price,
#     p_host_response_rate = as.numeric(host_response_rate))

table(data$review_scores_cleanliness)

# add new numeric columns from certain columns
numericals <- c("accommodates","review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights","beds")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))

table(data$accommodates)
table(data$f_bathrooms_text)
table(data$beds)


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
dummies <- names(data)[seq(65,94)]
dummies
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# stove -- bbq

# rename columns

# column name
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
# column number
dnames_i <- match(dnames, colnames(data))

colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# keep columns if contain d_, n_,f_, p_, usd_ and some others
# 138 variables became 49 variables
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed, neighbourhood_group_cleansed, room_type,property_type)

 
# with price info only
data <- data %>%
  drop_na(price)

write_csv(data, paste0(data_out, "airbnb_ny_workfile.csv"))


library(skimr)
##################################
# DESCRIBE

#--------------------------------

data <- read_csv(paste(data_out,"airbnb_ny_workfile.csv", sep = ""))

# 28402 observations decreased to 14170
data <- data %>%
  filter(neighbourhood_group_cleansed == "Manhattan")

write_csv(data, paste0(data_out, "airbnb_manhattan_workfile.csv"))

N=nrow(data)
# N= 14170

#
#####################
### look at price ###
#####################
summary(data$price)
describe(data$price)

data <- data %>%
  mutate(ln_price = log(price))

# Remove extreme values + missing from prices (this case: only 3 missing values)
# why 1000? because Max price is 850. summary() function
data <- data %>%
  filter(price <1000)

# Histograms
# theme_bg() sourced from theme_bg.R
R_F14_h_lnprice <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price") +
  theme_bg()
R_F14_h_lnprice

ggsave(paste0(data_out, "R_F14_h_lnprice.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)
# This function closes the specified plot (by default the current device) and if it is an imguR device, uploads the plots for web hosting
dev.off()

R_F14_h_price <- ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("count") +
  xlab("Price") +
  theme_bg()
R_F14_h_price
ggsave(paste0(output, "R_F14_h_price.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)



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
R_14_s_n_accommodates

ggsave(paste0(data_out, "R_14_s_n_accommodates.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2)

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

# try log beds
data <- data %>%
  mutate(ln_beds = log(n_beds))

## bathrooms
gg_bath <- ggplot(data, aes(f_bathrooms_text)) +
  geom_bar() + 
  ylab("") +
  xlab("N of bathrooms") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_bg()

gg_bath
ggsave(paste0(data_out, "gg_bath.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

data %>%
  group_by(f_bathrooms_text) %>%
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


ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bg()

# Pool num of reviews to 3 categories: none, 1-51 and >51
# based on number of reviews plot
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))

data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())
# mean price of houses with 0 reviews 129, with 1-51 reviews 120, with 51+ reviews 109

# Regression 1: log-price and number of reviews
reg4<-lm(ln_price ~ f_number_of_reviews, data=data)
summary(reg4)

# Regression 2: log-price and log number of reviews
reg5<-lm(ln_price ~ ln_number_of_reviews, data=data)
summary(reg5)


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
  filter(data$price <= 1000, ln_days_since>2)

# how many missing, mean, sd
skimr::skim(data$n_number_of_reviews)

ggplot(data = data, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(60,100)+
  xlim(0,20)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bg()

## review score effect
# HIGHER score, HIGHER price
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
# data$n_minimum_nights

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

lm(ln_price ~ f_minimum_nights,data=data)

categoricals <- c("f_property_type", "f_room_type")


for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}

# 8675 Entire apartment
# 561 Entire condominium
# 4934 Private room in apartment 

#                           mean_price     n
#   1 Entire home/apt             186.   9236
# 2 Private room                 88.4  4934

#####################################

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

write_csv(data, paste0(data_out, "airbnb_manhattan_workfile_adj.csv"))

#------------------------------------------------------------------------------------------------
