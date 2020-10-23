#'--------------------------------------
#'@date: 10-16-2020
#'@author: Julia
#'@title: Likelihood and linear models
#'
#'--------------------------------------


# Libraries anad Setup ####
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)
library(profileModel)

# Import Data

seals <- read_csv("data/17e8ShrinkingSeals Trites 1996.csv")


# Likelihood of a single data point ####

# assume population is gaussian with a mean of 10
# and a SD of 3

# let's say we have a single data point
x <- 10

# our likelihood for any given hypothesis is the density of 
# our data given our hypothesis
# p(D | H)

# digression: how to plot a normal curve
norm <- data.frame(x=seq(-3,3,length.out = 100)) %>% 
  mutate(p=dnorm(x,mean=0, sd = 1))

qplot(x,p, data = norm, geom = "line")


# if we want the likelihood of a hypothesized mean of 15
# and sd of 3 with our data
dnorm(x, mean = 15, sd = 3)

# to get a maximum likelihood estimate, we need to test a
# lot of possible means
lik_vec <- dnorm(x, mean = seq(8,12, length.out=100), 
                 sd = 3)

max(lik_vec)
which(lik_vec == max(lik_vec))

# this is tedious :( and vectors get ugly with lots of which statements

# Let's do this better
lik_df <- data.frame(mean=seq(8,12, length.out = 100)) %>% 
  mutate(lik = dnorm(x, mean = mean, sd = 3))

ggplot(lik_df,
       aes(x = mean, y = lik)) +
  geom_point()

# Likelihood with multiple data points ####


# our population had a mean of 15 and sd of 3
# lets get a sample
set.seed(607)
samp <- rnorm(20, mean = 15, sd = 3)

# let's write a function
norm_lik <- function(m, s = 3){
  dnorm(samp, mean = m, sd = s) %>% 
    prod()
}
#Give me the probability of seeing each data point of samp, 
# given a mean and sd, and take the product to get the total 
# probability
norm_lik(4)
norm_lik(6)
norm_lik(14)

norm_loglik <- function(m, s = 3){
  dnorm(samp, mean = m, sd = s, log = TRUE) %>%  sum() # sum bc logs
}
norm_loglik(4)
norm_loglik(16)

# multiple points

lik_df_mult <- tibble(mean=seq(10,20, length.out = 100)) %>% 
  rowwise(mean) %>% 
  mutate(lik = norm_lik(mean),
         loglik = norm_loglik(mean)) %>% 
  ungroup()

ggplot(lik_df_mult,
       aes(x = mean, y = lik)) + 
  geom_point()


ggplot(lik_df_mult,
       aes(x = mean, y = loglik)) + 
  geom_point()

# get our MLE 
lik_df_mult %>%  filter(loglik == max(loglik)) %>% as.data.frame()

# get 95% CI - the points that are 1.92 away from the MLE's loglik
# remember, we want the quantile of the chisq divided by 2 so we get
# both tails
qchisq(0.95,df = 1)/2
#SE 
qchisq(0.68, df = 1)/2
# above is applicable to the LOG LIKELIHOOD ONLY (not normal lik)

plot(seq(0,6, lenght.out = 100, dchisq(seq(0,6,length.out = 100), df = 1), type = "l"))

lik_df_mult %>% 
  filter(loglik >= max(loglik) - qchisq(0.95,df = 1)/2) %>% 
  as.data.frame()
#  13.83 - 16.36 is the ci (the lowest and highest values)


# wo0dimensional likelihood surfaces ####

# Remember, our population has a mean of 15 and sd of 3
# What if we need to estimate both the mean and sd?!

# Good thing we wrote functions!!
#BUT we need to create a grid
# Using the crossing function from tidyr

# Crossing to make a grid
crossing(1:3, 4:6)

like_df_norm <- crossing(m = seq(10, 20, length.out = 100),
                         s = seq(1,5, length.out = 100))

# BUT gives us a massive df 


# let's make our likelihood surface
like_df_norm <- crossing(m = seq(10, 20, length.out = 300),
                         s = seq(1,5, length.out = 300)) %>% 
  group_by(m, s) %>% 
  mutate(lik = norm_lik(m,s),
         loglik = norm_loglik(m,s),
         deviance = -2*loglik) %>% 
  ungroup()
# grid sampling is slow

# visualize!
ggplot(data = like_df_norm,
       aes(x = m, y = s, fill = loglik)) +
  geom_raster() + 
  scale_fill_viridis_c() +
  labs(x = "Mean",
       y = "Standard Deviation")

# hard to see differentiation, let's cut it down

ggplot(data = like_df_norm %>%  filter(loglik > max(loglik) - 5),
       aes(x = m, y = s, fill = loglik)) +
  geom_raster() + 
  scale_fill_viridis_c() +
  labs(x = "Mean",
       y = "Standard Deviation")


cont_plot <- 
  ggplot(data = like_df_norm %>%  filter(loglik > max(loglik) - 3),
       aes(x = m, y = s, z = loglik)) +
  geom_contour_filled(bins = 20) + 
  labs(x = "Mean",
       y = "Standard Deviation") +
  guides(fill = "none")

# rayshader

#MLA
like_df_norm %>%  filter(deviance == min(deviance))
mean(samp)
sd(samp)
# results are approx the same


# Get our profiles by slicing across values of m or s

# we want to, over all values of a parameter of interest
# get the likelihood at optimized values of all other parameters

# for one value of m...
like_df_norm %>% 
  filter(m == m[16900]) %>% 
  filter(deviance == min(deviance))

lik_prof_m <- like_df_norm %>% 
  group_by(m) %>% 
  filter(deviance == min(deviance)) %>% 
  ungroup()

# profile for s
lik_prof_s <- like_df_norm %>% 
  group_by(s) %>% 
  filter(deviance == min(deviance)) %>% 
  ungroup()

# here's my profile 
ggplot(lik_prof_m %>%  filter(loglik > max(loglik)-4),
       aes(x = m, y = loglik)) + 
  geom_point()


# let's see those profile lines in action
cont_plot +
  geom_line(data = lik_prof_m %>%  filter(loglik > max(loglik) - 1.92),
            aes(x = m, y = s), color = "red") +
  geom_line(data = lik_prof_s %>%  filter(loglik > max(loglik) - 1.92),
            aes(x = m, y = s), color = "orange")



# Regression with likelihood ####

seals

#dirty secret, you can do MOST of this with lm
seal_lm <- lm(length.cm ~ age.days, data = seals)
logLik(seal_lm)

# if we want to be 'strict', we'll use glm
seal_mle <- glm(length.cm ~ age.days,
                data = seals,
                family = gaussian(link = "identity")) # we have a 
# normal error distribution, 1 to 1 relationship (link)

# assumptions!
plot(seal_mle, which = 1)
plot(seal_mle, which = 2)
hist(residuals(seal_mle))

# the new thing - make sure our profiles are well behaved!
# using MASS and profileModel

prof <- profileModel(seal_mle,
                     objective = "ordinaryDeviance")
summary(prof)

plot(prof) # looking for nice, parabolic, inverse curves
plot(prof, print.grid.points = TRUE)

prof2 <-profileModel(seal_mle,
                     objective="ordinaryDeviance",
                     quantile = qchisq(0.95,1))
plot(prof2)

#---
# let's do this with MASS
prof_mass <- profile(seal_mle)
plot(prof_mass)
confint(prof_mass) # get CIs

# tau is the signed square root of the deviance
# se a parabola should becoem a straight line
# if it's not, you have a problem


# Model evaluation
summary(seal_mle) # dispersion parameter for gaussian = variance
summary(seal_lm)

# Model Comparison
seal_null <- glm(length.cm ~ 1,
                 data = seals,
                 family = gaussian(link = "identity"))

# compare seal_mle and seal_null
anova(seal_null, seal_mle, test = "LRT") # actually analysis of deviance
# deviance is test statistic
# the two models are significantly different
anova(seal_mle, test = "LRT") # kind of automated(?)

# for plotting, we just use method = "glm" instead of
# method = "lm" with stat smooth

ggplot(seals,
       aes(x = age.days, y = length.cm)) +
  geom_point() + 
  stat_smooth(method = "glm",
              method.args = list(family = gaussian(link = "identity")))






