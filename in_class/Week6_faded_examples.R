# Week 6 faded examples

library(ggplot2)
library(MASS)
library(profileModel)
library(dplyr)
# Example one

fat <- read.csv("data/17q04BodyFatHeatLoss Sloan and Keatinge 1973 replica.csv")

fat_plot <- ggplot(data=fat, 
                   aes(x=leanness,
                       y = lossrate)) +
  geom_point()
fat_plot

fat_mod <- glm(lossrate ~ leanness,
               family = gaussian(link = "identity"),
               data = fat)

# Assumptions
fat_fit <- predict(fat_mod)
fat_res <- residuals(fat_mod)

qplot(fat_fit, fat_res)

qqnorm(fat_res)
qqline(fat_res)

plot(profile(fat_mod))

# LRT test of model
fat_mod_null <- glm(lossrate ~ 1,
                    family = gaussian(link = "identity"),
                    data = fat)

anova(fat_mod_null, fat_mod, test = "LRT")

#t-test of parameters
summary(fat_mod)

# Example 2

deet <- read.csv("data/17q24DEETMosquiteBites.csv")

deet_plot <- ggplot(data = deet,
                    aes(x=dose, y = bites)) +
  geom_point()

deet_plot

# fit that model
deet_mod <- glm(bites ~ dose,
                family = gaussian(link = "identity"),
                data = deet)

# Assumptions
deet_fit <- predict(deet_mod)
deet_res <- residuals(deet_mod)

qplot(deet_fit, deet_res)

qqnorm(deet_res)
qqline(deet_res)

plot(profile(deet_mod))

# f-tests of model
deet_mod_null <- glm(bites ~ 1,
                     family = gaussian(link = "identity"),
                     data = deet)

anova(deet_mod_null, deet_mod, test = "LRT")

# t-test of parameters
summary(deet_mod)


# Example 3

zoo <- read.csv("data/17q02ZooMortality Clubb and Mason 2003 replica.csv")

zoo_plot <- ggplot(data = zoo,
                   aes(x = mortality,
                       y = homerange)) +
  geom_point()

# fit that model
zoo_mod <- glm(homerange ~ mortality,
               family = gaussian(link = "identity"),
               data = zoo)

# assumptions
zoo_fit <- predict(zoo_mod)
zoo_res <- residuals(zoo_mod)

qplot(zoo_fit, zoo_res)

qqnorm(zoo_res)
qqline(zoo_res)

plot(profile(zoo_mod))

# LRT-tests of model
zoo_mod_null <- glm(homerange ~ 1,
                    family = gaussian(link = "identity"),
                    data = zoo)

anova(zoo_mod, zoo_mod_null, test = "LRT")

# z-test
summary(zoo_mod)
