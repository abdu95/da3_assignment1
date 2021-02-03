# Predict airbnb apartment price in New York

# ------------------------------------------------------------------------------------------------------
#### SET UP

# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)

# load theme and functions
getwd()
setwd("D:/CEU/winter semester/data-analysis-3/da3_assignment1/")
source("codes/theme_bg.R")
source("codes/da_helper_functions.R")

options(digits = 3)

data_in <- "data/clean/"
data_out <- "data/out/"

########################################
# PART I.
########################################

# first, run data-prepare-NY.R

#############
# Load data #
#############

data <-
  read_csv("data/out/airbnb_manhattan_workfile_adj.csv") %>%
  mutate_if(is.character, factor)

######################
# Quick look at data #
######################

# Descriptive statistics and regressions

# columns and their values
glimpse(data)

# how many factor/numeric
# missing, unique values. 
skim(data)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values?
# 1. drop if no target (already did)
data <- data %>%
  drop_na(price)

# 2. imput when few, not that important
data <- data %>%
  mutate(
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathrooms_text=ifelse(is.na(f_bathrooms_text),"1 bath", f_bathrooms_text), #assume at least 1 bath
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
  )

# 3. drop columns when many missing not important
# to_drop <- c("usd_cleaning_fee", "p_host_response_rate")
# data <- data %>%
#   select(-one_of(to_drop))

# 4. Replace missing variables re reviews with zero, when no review + add flags
# we add flag 1 when we impute value
data <- data %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month)
  )

summary(data$price)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# 8 columns. seems alright, we can move on 

###################################
# Business logic- define our prediction problem
###################################

# Decision
# Size, we need a small and mid-size apartments, hosting 2-6 guests

# f_property_type = Apartment (not House)

# Filter for apartments which accommodate 2-6 persons
data <- data %>%
  filter(between(n_accommodates, 2, 6))
  
# something <- data

# this is our sample
skimr::skim(data)

# save workfile
write.csv(data, paste0(data_out, "airbnb_manhattan_work.csv"), row.names = F)

#####################################
# Look at some descriptive statistics
#####################################

#How is the average price changing in my district by `property_type`, `room_type` ?
data %>%
  group_by(f_property_type, f_room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

data %>%
  group_by(bathrooms_text) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

Hmisc::describe(data$price)
# missing 0
# lowest and highest

# NB all graphs, we exclude  extreme values of price
datau <- subset(data, price < 400)


# Distribution of price by type below 400

# Histograms
# price
g3a <- ggplot(data=datau, aes(x=price)) +
  geom_histogram_da(type="percent", binwidth = 10) +
  labs(x = "Price (US dollars)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  scale_x_continuous(expand = c(0.00,0.00),limits=c(0,400), breaks = seq(0,400, 50)) +
  theme_bg() 
g3a
#skewed to the right - long tail of high prices

save_fig("figure-airbnb-price", data_out, "small")


# lnprice
g3b<- ggplot(data=datau, aes(x=ln_price)) +
  geom_histogram_da(type="percent", binwidth = 0.2) +
  coord_cartesian(xlim = c(2.5, 6.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,6.6, 0.6)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_bg() 
g3b
# close to normal

save_fig("figure-airbnb-lnprice", data_out, "small")


## Boxplot of price by room type
g4 <- ggplot(data = datau, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3,
               color = c(color[2],color[1]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_room_type),
               color = c(color[2],color[1]), fill = c(color[2],color[1]),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price (US dollars)")+
  theme_bg()
g4
# entire home apartments are more expensive than private rooms

save_fig("figure-airbnb-room", data_out, "small")

# Boxplot
g5 <- ggplot(datau, aes(x = factor(n_accommodates), y = price,
                        fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1], color[3])) +
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50))+
  theme_bg() +
  theme(legend.position = c(0.3,0.8)        )
g5
# can accomodate more people - more expensive

# table(data$f_property_type)

save_fig("figure-airbnb-accom", data_out, "small")

########################################
# PART II.
########################################


#####################
# Setting up models #
#####################

# setting up models of increasing complexity 
# we want to have a model as complex and as useful for us
# not a model that too complex that overfits the data at hand
# we want to have a model that is complex enough to capture all the relations which we observed in the data which are useful to predict new listing

# LASSO - variable selection in data-driven, automated way
# now, manually defining variables

# out of sample prediction

# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", "n_days_since", "flag_days_since")

# Factorized variables
basic_add <- c("f_bathrooms_text")

reviews <- c("f_number_of_reviews","n_review_scores_rating", "flag_review_scores_rating")
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")

# Dummy variables: Extras -> collect all options and create dummies
# value = TRUE means 1. for columns that starts with d_, we have only values 0 and 1
amenities <-  grep("^d_.*", names(data), value = TRUE)

#################################################
# Look for interactions
################################################

#Look up room type interactions

p1 <- price_diff_by_variables2(data, "f_room_type", "d_wifi", "Room type", "WiFi")
p2 <- price_diff_by_variables2(data, "f_room_type", "f_property_type", "Room type", "Property type")
#Look up sound system
p4 <- price_diff_by_variables2(data, "f_room_type", "d_soundsystem", "Room type", "Sound system")
#Look up property type
p5 <- price_diff_by_variables2(data, "f_property_type", "d_parking", "Property type", "Parking")
p6 <- price_diff_by_variables2(data, "f_property_type", "d_refrigerator", "Property type", "Refrigerator")

g_interactions <- plot_grid( p1, p4, p5, p6,  nrow=2, ncol=2)
g_interactions

# dummies suggested by graphs
# graphs showed that interaction of 2 variables matters
X1  <- c("f_property_type*d_parking",  "f_room_type*d_wifi")

# Additional interactions of factors and dummies
X2  <- c("d_conditioning*f_property_type", "d_soundsystem*f_property_type", "d_wifi*f_property_type")
X3  <- c(paste0("(f_property_type + f_room_type) * (",
                paste(amenities, collapse=" + "),")"))


# Create models in levels models: 1-8
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))

#################################
# Separate hold-out set #
#################################

# split the sample into 2 parts, sparing 20% of the sample to "holdout set"
# holdout set is being held out, not used at all. not for model training, not for model selection
# it is used only at the end of our process, to mimic the situation of the live data (that we have never seen before while modelling)

# 80% data for model estimation and model selection (work data)

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducible
set.seed(20180123)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set = 2373 obs
data_holdout <- data %>% filter(holdout == 1)

#Working data set = 9495 obs
data_work <- data %>% filter(holdout == 0)

##############################
#      cross validation      #
##############################

# 5 folds

## N = 5
n_folds=5
# Create the folds
set.seed(20180124)

# generate random integers between 1 and 5
# how many integers? the same as length of data = 9495
folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))

# 1899 ones, twos, threes, fours and fives
table(folds_i)
# Create results
model_results_cv <- list()

# from 1 to 8 because we have 8 models


for (i in (1:8)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared
  
  # exactly same data-points assigned to first model (first fold) and second model
  # the only thing you want to be different in comparison is model, not the data in which you estimate the performance
  # each of the observations is assigned to one of the five folds
  
  # Do the k-fold estimation
  for (k in 1:n_folds) {
    # test is where we generated 1
    # gives vector of indices where in the folds_i vector, I have 1
    # so to say: give me positions (index) of element in folds_i vector, element == 1 (k)
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
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

names(model_results_cv)
# we can compare the performance for each folds

# we will take average of these as a measure of out of sample performance
model_results_cv[["modellev1"]][["rmse_test"]]
model_results_cv[["modellev2"]][["rmse_test"]]

# more user friendly
# formula - very complex

model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)

skim(data_train$ln_days_since)


# imap() - mapper
t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1

# this is the result of cross validation of all 8 models

# .funs = mean --> we take the mean of all these vectors
# rmse_test = 103.2. This is average RMSE of modellev1 across all 5 test sets
model_results_cv[["modellev1"]][["rmse_test"]] %>% mean() 

# rmse_train: within sample, we do better and better as model gets more complex
# out of sample (rmse_test), most complex model is performing worse, error high
# we would select model7 because rmse_test is lowest
# I think this t1 is outcome of all our work so far, becuase it helped us to pick the model

# BIC or test? try to choose simpler model

# data in folds is same?! how come?

column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                  "Test RMSE")


# Nice table produced and saved as .tex without \begin{table}
# -R2, BIC on full work data-n.
# -In sample rmse: average on training data; avg test : average on test data
t14_2 <- t1 %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2) <- column_names
print(xtable(t14_2, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(data_out, "table_fit_level.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)

# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_y_continuous(name = "RMSE") +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4)) +
  #scale_colour_discrete(guide = 'none') +
  theme_bg()
model_result_plot_levels
save_fig("airbnb-model-result-levels", data_out, "small")

#################################
#           LASSO               #
#################################

# LASSO - select features so that the out of sample prediction is good

# take model 8 (and find observations where there is no missing data)may
vars_model_7 <- c("price", basic_lev,reviews,poly_lev,X1,X2,amenities)
vars_model_8 <- c("price", basic_lev,reviews,poly_lev,X1,X2,amenities,X3)

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# "alpha" = c(1) --> LASSO parameter
# lambda --> how much weight do we put on this penalty term, how much do we care about having a simple model
# the larger lambda --> the fewer features will be kept in linear model
# we let data to speak (state) which lambda is best for our problem
# we use cross validation to select among lambdas

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_8, "price"), collapse = " + ")))

set.seed(1234)

# LASSO enclosed in "glmnet" package
# in LASSO, we add penalty term to the linear regression to minimize sum of squared errors
# penalize large coeffecients: sum of the absolute value of coefficients
# at the end, some of the features will be entirely dropped from linear model

lasso_model <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

# we have estimated 20 different LASSO models
# using cross validation, out of sample performance varied

print(lasso_model$bestTune$lambda)
# it turns out best lambda is 0.8 

# extract coefficients (from the output of final model)
lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

print(lasso_coeffs)
# many of the coefficients are equal to 0 (dropped from linear model)
# automated feature selection that is carried out by LASSO

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])

# 85.8 --> RMSE (same as RMSE of model7 -- model that was selected by CV)


########################################
# PART III.
########################################



###################################################
# Diagnsotics #
###################################################
model3_level <- model_results_cv[["modellev3"]][["model_work_data"]]
model7_level <- model_results_cv[["modellev7"]][["model_work_data"]]


# look at holdout RMSE
model7_level_work_rmse <- mse_lev(predict(model7_level, newdata = data_work), data_work[,"price"] %>% pull)**(1/2)
model7_level_holdout_rmse <- mse_lev(predict(model7_level, newdata = data_holdout), data_holdout[,"price"] %>% pull)**(1/2)
model7_level_holdout_rmse

###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################


# Target variable
Ylev <- data_holdout[["price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
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
summary(predictionlev_holdout)


# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 350, yend =350), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_bg() 
level_vs_pred
# compare predicted value with actual value (using holdout set)
# dots are mainly close to 45 degree line, which is good
# there is a lot of noice, a lot of spread

save_fig("figure-level-vs-pred", data_out, "small")


# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

summary(predictionlev_holdout_pred)
# 80% predicting interval

# mean = 149 dollar price
# 149 dollar price

predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(n_accommodates) %>%
  dplyr::summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
                   conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

kable(x = predictionlev_holdout_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = FALSE,
      linesep = "", col.names = c("Accomodates","Prediction","Pred. interval lower",
                                  "Pred. interval upper","Conf.interval lower","Conf.interval upper")) %>%
  cat(.,file= paste0(data_out, "modellev7_holdout_summary.tex"))


F14_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(n_accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (US dollars)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
F14_CI_n_accomodate

# summarizes predictive interval
# this is only bar chart, with interval, how I can say exact prediction?
# or exact prediction interval?

# interval is wide --> there is a lot of uncertainty
# we take holdout set. 
# we select all data points from holdout where n_accomodates = 2
# average of point predicted values. green - averages of prediction intervals for each of these observations

# first bar (low point): prediction interval's lower end negative. why? this is calculated via formula. how much uncertainty there is in the estimates
save_fig("figure-accomodate", data_out, "small")


# we predicted only 4 houses with price more than 300 USD. 
# But actually, there are much more houses  with price more than 300 USD. 
# This is quite usual phenomenon as it it difficult for regression models to predict extreme values, unless there are many of them in a very large dataset.

#NOT USED
# Density chart (not in book)
g3 <- ggplot(data = datau, aes(x=price)) +
  geom_density(aes(color=f_room_type, fill=f_room_type),  na.rm =TRUE, alpha= 0.3) +
  labs(x="Price (US dollars)", y="Density", color = "") +
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3]),
                     labels=c("Entire home/apt","Private room", "Shared room")) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1], color[3]),
                    labels=c("Entire home/apt","Private room", "Shared room")) +
  theme_bg() 
theme(legend.position = c(0.7,0.7),
      legend.direction = "horizontal",
      legend.background = element_blank(),
      legend.box.background = element_rect(color = "white"))
g3


# Barchart  (not in book)
plot4 <- ggplot(data = datau, aes(x = factor(n_accommodates), color = f_room_type, fill = f_room_type)) +
  geom_bar(alpha=0.8, na.rm=T, width = 0.8) +
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1],  color[3])) +
  labs(x = "Accomodates (Persons)",y = "Frequency")+
  theme_bg() 
theme(legend.position = "bottom")
plot4

