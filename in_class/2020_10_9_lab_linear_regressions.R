#'---------------------------------
#'
#'@title: Exploring Linear Regression
#'
#'@author: Julia McCartney
#'
#'@date: 10/9/2020
#'
#'-----------------------------

#General Regression Workflow

# Libraries ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)


# Loading in data ####
mosquito <- read_csv("data/17q24DEETMosquiteBites.csv")
bodyfat <- read_csv("data/17q04BodyFatHeatLoss Sloan and Keatinge 1973 replica.csv")
mortality <- read_csv("data/17q02ZooMortality Clubb and Mason 2003 replica.csv")
seals <- read_csv("data/17e8ShrinkingSeals Trites 1996.csv")

# What's here?

str(seals)
summary(seals)
#visdat
skimr::skim(seals)

# Visualize the data ####

seal_plot <- ggplot(data = seals, aes(x = age.days, y = length.cm)) + 
  geom_point(alpha = 0.5)
seal_plot



# Use the fit model to test assumptions ####

# model formulae are in the y ~ x format
seal_lm <- lm(data = seals, length.cm ~ age.days)
seal_lm

#Does the distribution of our predictions match our data?
seal_sims <- simulate(seal_lm, nsim = 20) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "sims",
    values_to = "length.cm"
  )
ggplot() +
  geom_density(data = seal_sims,
               mapping = aes(x = length.cm, group = sims),
               size = 0.2) +
  geom_density(data = seals,
                mapping = aes(x = length.cm),
                size = 2, color = "blue") 
# It matches pretty well!

plot(seal_lm)

# Is there a relationship between fitted and residual values?
plot(seal_lm, which = 1)
# want to do this in ggplot? use the package ggfortify
ggfortify::autoplot(seal_lm, which = 1, ncol = 1)


# Did we satisfy normality and homoskedacticity(variance is constant across the dataset)
#using a qqplot and levene test

residuals(seals_lm) %>% hist() #easy, but not so great for small datasets

plot(seal_lm, which = 2)

#residuals(seal_lm) %>%  shapiro.test() # could in theory, 
#but sample size is too large :(

# look for outliwers with leverage
plot(seal_lm, which = 4) # over 1 is a problem maybe?
plot(seal_lm, which = 5) # only if very worried -> influence of outliers
# ^ nothing of concern here!

# Explaining leverage
dat <- tibble(x = c(1:10, 100),
              y = c(rnorm(10, x), 50))
ggplot(data = dat,
       aes(x = x,
           y = y)) +
  geom_point() + stat_smooth(method = "lm")
# Extreme outlier is way out there and is pushing the line down

fit <- lm(y ~ x, data = dat)
plot(fit, which = 4) # cooks D is > 300!
plot(fit, which = 5)

ggplot(data = dat[-11,],
       aes(x = x,
           y = y)) +
  geom_point() + stat_smooth(method = "lm")
fit_noout <- lm(y ~ x, data = dat[-11,])

coef(fit)
coef(fit_noout)

# Evaluate the model ####
library(broom)

#F-test 
# Did we explain any variation in the data other than noise?
# Null hypothesis - if nothing mattered, then our model should have just
# as much explanatory power as the noise we observe - ratio 
# of variance(model)/var(error = F ratio)
# If we get a small probability value, we reject the null

anova(seal_lm)

anova(seal_lm) %>% 
  tidy()

# T-test of parameters 
# If we divide a parameter by it's SE (precision) = t
# We can use that to see if we can reject the hypothesis that our 
# parameter = 0

# tidy output from broom
summary(seal_lm)
tidy(seal_lm)
glance(seal_lm)

# just hte r2
summary(seal_lm)$r.squared
summary(seal_lm)$coef

# Visualize the fit model ####

seal_plot + 
  stat_smooth(method = "lm") # show error around our fit (ci is fit interval)

fit_seals <- predict(seal_lm,
                     interval = "confidence") %>% 
  as_tibble() %>% 
rename(lwr_ci = lwr,
       upr_ci= upr)

seals <- cbind(seals, fit_seals)
head(seals)

ggplot(seals,
       aes(x = age.days,
           ymin = lwr_ci, ymax = upr_ci,
           y = fit)) + 
  geom_ribbon() + # plot the fit interval
  geom_line(color = "blue")
  
# Prediction interval

predict_seals <- predict(seal_lm,
                         interval = "prediction") %>% 
  as_tibble() %>% 
  rename(lwr_pi = lwr,
            upr_pi = upr,
         pfit = fit)
seals <- cbind(seals, 
        predict_seals)  

ggplot(seals,
       aes(x = age.days,
           ymin = lwr_pi,
           ymax = upr_pi,
           y =pfit)) +
  geom_point(mapping = aes(y = length.cm)) +
  geom_ribbon(alpha = 0.3) + 
  geom_line(color = "blue", size = 2) 
  
# Let's visually compare the fit interval and the prediction intervals

ggplot(seals, aes(x = age.days,
                  y = length.cm)) +
  # prediction interval (if we were to pick a new value, 95% of time it will be in this range)
  geom_ribbon(aes(ymin = lwr_pi, 
                  ymax = upr_pi),
              alpha = 0.5) + 
  # fit interval - just coefficient error (precision)
  geom_ribbon(aes(ymin = lwr_ci,
                  ymax = upr_ci),
              alpha = 0.5,
              color = "red")
  
#--------------------------------------------------------------------------
# Faded examples ####

# Example 1

fat <- bodyfat

# initial visualization to determin if lm is appropriate
fat_plot <- ggplot(data = fat, aes(x = leanness, y = lossrate)) +
  geom_point()
fat_plot

fat_mod <- lm(lossrate ~ leanness, data = fat)

# assumptions
simulate(fat_mod, nsim = 100) %>% 
  pivot_longer(cols = everything(),
               names_to = "sim",
               values_to = "lossrate") %>% 
  ggplot(aes(x = lossrate)) +
  geom_density(aes(group = sim), lwd = 0.2) +
  geom_density(data = fat, color = "blue", lwd = 2)

plot(fat_mod, which = 1)
plot(fat_mod, which = 2)

# f tests of model
anova(fat_mod)

# t test of parameters
summary(fat_mod)

# plot with line
fat_plot +
  stat_smooth(method = lm, formula = y ~ x)

# Example 2

deet <- mosquito 

deet_plot <- ggplot(data = deet, aes(x = dose, y = bites)) +
  geom_point()

deet_plot

deet_mod <- lm(bites ~ dose, data = deet)

# assumptions
simulate(deet_mod, nsim = 100) %>% 
  pivot_longer(cols = everything(),
               names_to = "sim",
               values_to = "dose") %>% 
  ggplot(aes(x = dose)) +
  geom_density(aes(group = sim),
                lwd = 0.2) +
  geom_density(data = deet, color = "blue", lwd = 2)

plot(deet_mod, which = 1)
plot(deet_mod, which = 2)

# f test
anova(deet_mod)

# t test 
summary(deet_mod)

# plot with line

deet_plot +
  stat_smooth(method = lm, formula = y ~ x)


# Example 3 - Log-transform for nonlinearity

# MAY BE WRONG

deet_mod_log <- lm(log(bites) ~ dose, data = deet)

# assumptions 
simulate(deet_mod_log, nsim = 100) %>% 
  pivot_longer(cols = everything(),
               names_to = "sim",
               values_to = "log_dose") %>% 
  mutate(bites = exp(log_bites)) %>% 
  ggplot(aes(x = bites)) +
  geom_density(aes(group = sim), lwd = 0.2) +
  geom_density(data = deet, color = "blue", lwd = 2)

plot(deet_mod_log, which = 1)
plot(deet_mod_log, which = 2)

# f test
anova(deet_mod_log)


# t test
summary(deet_mod_log)

# plot with line 
deet_plot + 
  scale_y_continuous(trans = "log") +
  stat_smooth(method = lm, formula = y ~ x)

# Example 4 

zoo_mod <- mortality

zoo_mod <- lm(mortality, data = zoo_mod)

# assumptions
simulate(zoo_mod, nsim = 100) %>% 
  pivot_longer(cols = everything(),
               names_to = "sim",
               values_to = "mortality") %>% 
  ggplot(aes(x=mortality)) +
  geom_density()









