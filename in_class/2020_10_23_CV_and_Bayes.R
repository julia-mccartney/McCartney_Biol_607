#'-------------------------------------
#'@title: CV anad Bayes
#'@date: 10/23/2020
#'-------------------------------------

#libraries
library(readr)
library(ggplot2)
library(dplyr)
library(purrr)
library(boot) # for cv
library(rsample) # for CV
library(modelr) # for CV
library(tidyr)

#load data

roaches <- read_csv("data/chap17f5_4CockroachNeurons.csv")

# Let's look at that data ####

# plot the data with a fit linear regression

ggplot(data = roaches, 
       aes(x = temperature,
           y = rate)) +
  geom_point() +
  stat_smooth(method = "lm", fill = NA) + # remove bands
  stat_smooth(method = "lm", fill = NA,
              formula = y ~ 1,
              color = "red", linetype = 2) # intercept only model

# Cross Validation ####


# Simplest case for CV is one point 

roaches_no_15 <- roaches[-15,] #data
roaches_15 <- roaches[15,] # test data

# CV in a nutshell
# 1. fit a model with your training data
roach_lm_no_15 <- lm(rate ~ temperature, data = roaches_no_15)
roach_int_no_15 <- lm(rate ~ 1, data = roaches_no_15)

# 2. evaluate the out of sample deviance (MSE) for your test data

# Get that MSE

mse(model = roach_lm_no_15, data = roaches_15)
# in units of sum of squares (use rmse for data units)
rmse(model = roach_lm_no_15, data = roaches_15)

rmse(model = roach_int_no_15, data = roaches_15)
# Much bigger number!


# K-fold CV ####

# make a folded data set object
roach_five_fold <- vfold_cv(data = roaches, v = 5)

# aside
#roach_five_fold$splits[[1]]
#analysis(roach_five_fold$splits[[1]]) # training dataset
#assessment(roach_five_fold$splits[[1]]) # test dataset

# fit a model to each fold
set.seed(2020)
roach_five_fold2 <- roach_five_fold %>% # start with our tibble
  mutate(mods = map(splits, # create a new column, mods, which we
                    #make with map iterating over all our spits
                    ~lm(rate ~ temperature, 
                        #for each split, fit a model
                        # using the training data set
                        data = analysis(.x)))) # new list column

roach_five_fold2$mods[[2]]

# extract the out of sample rmse for each model 
# at each fold
# using a function called map2!

#intro to map2
x <- 1:10
y <- 11:20

map2_dbl(x,y, ~.x + .y)

# back to our data

# start with our tibble
roach_five_fold3 <- roach_five_fold2 %>% 
  # create a new column rmse, wich we make with map2
  #iterating over all splits AND fit models
  mutate(rmse = map2_dbl(splits, mods, 
                         ~rmse(model = .y, 
                               data = assessment(.x))))
  
# get av rmse (kfold cv score)
mean(roach_five_fold3$rmse)
# average out of sample deviation from 5 fold model
  
  
# LOOCV ###

# start by making a leave one out tibble
roach_loo <- roaches %>% 
  loo_cv() %>% 

# fit the temp and intercept only models for each 
# loo split
  mutate(temp_mod = map(splits,
                        ~lm(rate ~ temperature, 
                           data = analysis(.))),
         int_mod = map(splits,
                       ~lm(rate ~ 1, 
                          data = analysis(.))))

# compare the two models
# Pivot time!

# Get the rmse of each model and each model type 
# for each LOO split

# start with our tibble
roach_loo2 <- roach_loo %>% 
  # pivot to put ALL the models in one column
  pivot_longer(cols = c(temp_mod, int_mod),
               names_to = "model_name",
               values_to = "fit_model") %>% 
  # get our rmse just like before with map2!
  mutate(rmse = map2_dbl(splits, fit_model,
                     ~rmse(data = assessment(.x),
                           mod = .y)))

# the answer!
roach_loo2 %>% 
  group_by(model_name) %>% 
  summarize(loo_rmse = mean(rmse))

# what can we do with this?

# plot it!
ggplot(data = roach_loo2,
       aes(x = id, y = rmse, color = model_name)) +
  geom_point() +
  scale_x_discrete(labels = NULL)


# Using boot::cv.glm() for LOO or k-fold ####

roach_glm <- glm(rate ~ temperature, data = roaches,
                 family = gaussian(link = "identity"))

loo_roach <- cv.glm(data = roaches,
                    glmfit = roach_glm,
                    K = nrow(roaches))
summary(loo_roach)
# interested in delta

# what is our loo cv score
loo_roach$delta[[1]]

# AIC ####

roach_lm <- lm(rate ~ temperature, data = roaches)
roach_int <- lm(rate ~ 1, data = roaches)
roach_sq <- lm(rate ~ poly(temperature, 2), # gives 
               #linear and ^2 term
               data = roaches)
roach_cub <- lm(rate ~ poly(temperature, 3), data = roaches)


AIC(roach_lm)
AIC(roach_int)
# not great for multiple models

#this is better
library(AICcmodavg)

mod_list <- list(roach_lm, roach_int, roach_cub, roach_sq)
name_vec <- c("linear", " int", "cube", "quad")
aictab(cand.set = mod_list, modnames = name_vec)

# easier to compare AICs across models

# What are the coefs?
broom::tidy(roach_cub)

# AIC is NOT AICc!!

# I'm a Bayesic Bitch ####

# pick a color, any color!
#blue - the best color


