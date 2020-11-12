#'-----------------
#' T-tests as a linear model
#'
#' Link to lab: https://biol607.github.io/lab/comparing_two_means.html
#'@date: 5 Nov 2020
#'-----------------

# Libraries ####
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car) # companion to applied regression - by John Fox
library(emmeans) # expected means


# Loading Data ####

blackbird <- read_csv("data/12e2BlackbirdTestosterone.csv")

# looking at Paired data
# Data where we have paired samples
# (two plots next to eachother, two cells from the same body,
# two measurements before and after an intervention, etc)

str(blackbird)

# Asking - is my data different than 0? Basic t test

# Visualize and with a paired test, see assumptions
ggplot(blackbird, aes(x = dif)) + geom_histogram(bins = 8)
# doesn't look great....
# but that's why it's already log transformed!

ggplot(blackbird, aes(x = `dif in logs`)) + geom_histogram(bins = 10)

# vis before and after
b_tidy <- blackbird %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(-id,
               names_to = "When",
               values_to = "Antibody") %>% 
  filter(When %in% c("Before", "After"))

ggplot(b_tidy,
       aes(x = When, y = Antibody, group = id)) + 
  geom_point() +
  geom_line()


# fit a linear model
# dif ~ b0 + error
blackbird_mod <- lm(dif ~ 1, data = blackbird)

# assumptions
plot(blackbird_mod, which = 1)
plot(blackbird_mod, which = 2)
shapiro.test(residuals(blackbird_mod))
# despite vis didn't look good, the model looks good!

# Are my means different?
summary(blackbird_mod)
# see that there is no sig difference - fail reject a null hypothesis

library(brms)

blackbird_brm <- brm(dif ~ 1, data = blackbird,
                     family = gaussian(link = "identity"),
                     chains = 3)

# Credible intervals
fixef(blackbird_brm, probs = c(0.025, 0.1, 0.9, 0.975))
plot(blackbird_brm)

blackbird_post <- as.data.frame(blackbird_brm)
head(blackbird_post)

# how much is greater than 0 
sum(blackbird_post$b_Intercept >0)/nrow(blackbird_post)

# AIC
blackbird_mod_really_null <- lm(dif ~ 0, data = blackbird)

AIC(blackbird_mod)
AIC(blackbird_mod_really_null)


# Comparing Two Separate Means ####

salmon <- read_csv("data/12e4BrookTrout.csv") %>% 
  janitor::clean_names()

# this is simlar to an unpaired t test

ggplot(salmon,
       aes(x = brook_trout,
       y = mean_chinook_survival)) +
  geom_boxplot()


# fit a model that asks did survival differ between the two?
# first need to transform the presence of brook trout to 0 and 1

# fit a model with dummy encoded categorical variables
# OR - ONE HOT ENCODING!
# y ~ b0 + b1 * trout + error
# b1 = difference between groups
# b0 = mean of whatever group is 0

# This is the model!!!
salmon_mod <- lm(mean_chinook_survival ~ brook_trout, 
                 data = salmon)

# What's happening inside
model.matrix(mean_chinook_survival ~ brook_trout, 
             data = salmon)

model.matrix(mean_chinook_survival ~ brook_trout -1, 
             data = salmon)


# Evaluate the model
summary(salmon_mod)
# intercept - is one of the groups different from 0?
# brook_trout+ is it different from brook_trout-?


# oopes assumptions
plot(salmon_mod, which = 1)
# difference in variance between two groups
# what do we do??
# transform, weighted least squares, etc

# Levene's test from car
# is the variance different between groups?
leveneTest(salmon_mod)
#aaaand that's no good?

# non-parametric test - we analyze ranks (will loose some power)
salmon_rank <- lm(rank(mean_chinook_survival) ~ brook_trout,
                  data = salmon)
summary(salmon_rank)

# let's go big and model variance
# y ~ b0 + b1 * trout + error
# e = g0 + g1*trout

library(nlme)
salmon_gls <- gls(mean_chinook_survival ~ brook_trout, 
                  data = salmon,
                  weights = varIdent(form = ~1|brook_trout))

summary(salmon_gls)

# same thing, different library
library(glmTMB)
salmon_tmb <- glmmTMB(mean_chinook_survival ~ brook_trout,
                      dispformula = ~ brook_trout,
                      data = salmon)

# same thing, but bayes

# here we are making variance a function of a predictor
salmon_brm_mod <- brmsformula(mean_chinook_survival ~ brook_trout,
                              sigma ~ brook_trout)

salmon_brm <- brm(salmon_brm_mod, data = salmon, 
                  chains = 3)
# will loose df and some power, but more accurately representing the data
fixef(salmon_brm)
plot(salmon_brm)


# What are our means?
# with emmeans

salmon_em <- emmeans(salmon_brm, ~ brook_trout)
# HPD - highest probability density
emmeans(salmon_mod, ~ brook_trout)

plot(salmon_em)


