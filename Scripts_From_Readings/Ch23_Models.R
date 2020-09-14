#' ------------------------------------------------
#' @title : Chapter 23 Model Basics
#' @author : Julia McCartney
#' @date : 9/13/2020
#' 
#' ------------------------------------------------

library(tidyverse)
library(modelr)
options(na.action = na.warn)

# A simple Model ####

ggplot(sim1, aes(x,y)) + 
  geom_point() #plot the data to look at it


models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5) # run many models using slope and intercept as parameters
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()  #plot the above

model1 <- function(a, data) { # write a function for the model family
  a[1] + data$x * a[2]
}

model1(c(7,1.5), sim1) # test function

#now how do we compute the distance between the points and the model??

measure_distance <- function(mod, data) { # use the root mean squared deviation
  diff <- data$y - model1(mod,data) # "We compute the difference between actual and predicted, 
  sqrt(mean(diff^2)) # square them, average them, and the take the square root."
}

measure_distance(c(7,1.5), sim1)

sim1_dist <- function(a1,a2) {
  measure_distance(c(a1,a2), sim1) # writing a function to run all the models from above
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1,a2,sim1_dist)) # and run it

models

# overlay th e 10 best models
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

# ok now they're doing this weird thing with a scatter plot - looking at a1 vs a2 (which i *think* represent slope and intercept?)
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))
# best 10 are highlighted in red


#now pick models based on a grid search
#searched based on what was best from the above plot


grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

#again best in red

#now overlay on original data again (from grid ppicking)
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10))
# looks really good!

# So this is the part where they tell us yes you can do this and people used to 
# suffer through it BUT there is a much simpler way to do it -___-

#' a numerical minimisation tool called Newton-Raphson search. The intuition 
#' of Newton-Raphson is pretty simple: you pick a starting point and look around 
#' for the steepest slope. You then ski down that slope a little way, and then 
#' repeat again and again, until you can’t go any lower.

#and so we go

best <- optim(c(0, 0), measure_distance, data = sim1) # we use optim to do this
best$par # yes it is actually this simple

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])
# bam, best model


# This ^ is a simple linear model!
# So we can also just do this:

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

#hey look at that! The output for sim1_mod is the same as for best$par!

# Simple Model Exercises ####


# Exercise 1

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

ggplot(sim1a, aes(x,y)) + 
  geom_point()

#there look to be some outliers, but nothing outrageous most of the time

ex1a_mod <- lm(y ~ x, data = sim1a)

ggplot(sim1a, aes(x,y)) + 
  geom_point() +
  geom_abline(slope = ex1a_mod$coefficients[2], intercept = ex1a_mod$coefficients[1])


# Exercise 2

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}

best2 <- optim(c(0, 0), measure_distance, data = sim1a)
best2$par
ex1a_mod$coefficients

# They don't quite line up, but are pretty close


# Exercise 3 











