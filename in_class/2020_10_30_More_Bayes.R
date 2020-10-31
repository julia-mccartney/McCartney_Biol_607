#'----------------------------
#'@title: Bayes Lab (pt 2, the Electric Boogaloo)
#'@date: 30 Oct 2020
#'@author: Julia McCartney
#'----------------------------

#Snow day! 

# Lab: https://biol607.github.io/lab/08_bayes.html#2_fitting_a_line_using_bayesian_techniques

# Libraries ####

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Libraries for Bayes
library(brms)
options(mc.cores = parallel::detectCores() - 1) # -1 bc zoom!

#fun packages - furr --> purr but parallelized

# Load Data ####

seals <- read_csv("data/17e8ShrinkingSeals Trites 1996.csv")

# Fit a linear model with Bayes!!! #### 

seal_lm_bayes <- brm(length.cm ~ age.days,
                     family = gaussian(link = "identity"),
                     data = seals,
                     file = "brms_fits/seal_lm_bayes", # run once, don't loose
                     chains = 4,
                     seed = 607) 
# Would need to delete file to rerun

# Assess that our Golem isn't going to burn down Prague
library(bayesplot)
color_scheme_set(scheme = "red")
# Visually investigate our chains
plot(seal_lm_bayes)

plot(seal_lm_bayes, par = "b_Intercept")

mcmc_trace(seal_lm_bayes)

# Look at a diagnostic of convergence
# Gelman-Rubin statistic - Rhat
rhat(seal_lm_bayes) # only worried if off in the hundriths

rhat(seal_lm_bayes) %>% 
  mcmc_rhat()

# assess autocorrelation (which shouldn't be a problem)

mcmc_acf(seal_lm_bayes)


# check our MODEL assumptions ####

#pp_check()

# check the match between our data and our chains 
# for distributions of y

pp_check(seal_lm_bayes, "dens_overlay")

# Is our error normal? # Abour error and future predictions
pp_check(seal_lm_bayes, "error_hist") # Yes!
pp_check(seal_lm_bayes, "error_scatter")
pp_check(seal_lm_bayes, "error_scatter_avg")

# fitted v. residual # Did we miss anon-linearity?
seal_res <- residuals(seal_lm_bayes) %>% as_tibble()
seal_fit <- fitted(seal_lm_bayes) %>% as_tibble()

plot(y = seal_res$Estimate, x = seal_fit$Estimate)

# Inference with our models - what is here? ####
summary(seal_lm_bayes)

# Visualize our posteriors ####
library(tidybayes) # extractions and tidying
library(ggdist) # visualization

seal_coef_draws <- seal_lm_bayes %>% 
  gather_draws(b_Intercept,
               b_age.days,
               sigma)

# can do individually

ggplot(seal_coef_draws %>%  filter(.variable == "sigma"),
       aes(x = .value)) +
  stat_halfeye(.width = c(0.8, 0.95))

ggplot(seal_coef_draws %>%  filter(.variable == "b_Intercept"),
       aes(x = .value)) +
  stat_halfeye(.width = c(0.8, 0.95))

ggplot(seal_coef_draws %>%  filter(.variable == "b_age.days"),
       aes(x = .value)) +
  stat_halfeye(.width = c(0.8, 0.95))

# What is the probability that our slope is less than 0? Or any 
# parameter

seal_coef_draws %>% 
  group_by(.variable) %>% 
  summarize(prob_less_than_0 = 
              sum(.value<0)/ n(),
            ndraws = n())

# What is the probability that our slope is less than 0.0023?
seal_coef_draws %>% 
  filter(.variable == "b_age.days") %>% 
  summarize(prob_less_than0.0023 = 
              sum(.value < 0.0023)/ n())

# Let's visualize our line!

seal_plot <- ggplot(seals, 
       aes(x  = age.days, y = length.cm)) +
  geom_point()

# add a line
seal_coefs <- fixef(seal_lm_bayes)
seal_coefs_wide <- spread_draws(seal_lm_bayes,
                                b_Intercept,
                                b_age.days)

# show uncertainty in fit
seal_plot + 
  geom_abline(data = seal_coefs_wide,
              aes(slope= b_age.days,
                  intercept = b_Intercept),
              color = "blue",
              alpha = 0.1) +
  geom_abline(slope = seal_coefs[2,1],  
              intercept = seal_coefs[1,1],
              color = "red")

# show uncertainty in prediction

seal_predict <- posterior_predict(
  seal_lm_bayes,
  newdata = data.frame(age.days = c(2000,8000))
                                  ) %>% 
  as.data.frame()
# predicting the length for 

predict_data <- data.frame(age.days = c(1000, 8000)) %>% 
  add_predicted_draws(model = seal_lm_bayes)

seal_plot + 
  geom_line(data = predict_data,
            aes(x = age.days,
                y = .prediction,
                group = .draw), # KEY
            alpha = 0.1, 
            color = "lightblue") +
  geom_abline(slope = seal_coefs[2,1],  
              intercept = seal_coefs[1,1],
              color = "red")

seal_plot +
  stat_lineribbon(data = predict_data,
                  aes(x = age.days, y = .prediction),
                  alpha = 0.5,
                  .width = c(0.25, 0.5, 0.75, 0.9)) + # set intervals
  scale_fill_brewer()

# same thing with a fit

fit_data <- data.frame(age.days = c(1000, 8000)) %>% 
  add_fitted_draws(model = seal_lm_bayes)

seal_plot +
  stat_lineribbon(data = fit_data,
                  aes(x = age.days, y = .value),
                  alpha = 0.5,
                  .width = c(0.95, 0.99, 0.995, 0.9995)) + # set intervals
  scale_fill_brewer()
# Brief of LOO and WAIC ####
library(loo)

seal_waic <- waic(seal_lm_bayes)

seal_loo <- loo(seal_lm_bayes)
plot(seal_loo)


# let's say I had fit a model, seal_poly
seal_poly <- brm(length.cm ~ poly(age.days, 2),
                 data = seals,
                 family = gaussian(link = "idenity"))
poly_loo <- loo(seal_poly)

compare(seal_loo, poly_loo)
# compare allows us to compare model objects and evaluate which is
# better at prediction

seal_k10 <- kfold(seal_lm_bayes, k = 10)

# Futz with Priors ####

prior_summary(seal_lm_bayes)

# ?prior

seal_lm_prior <- brm(length.cm ~ age.days, 
                     data = seals, 
                     family = gaussian(link = "identity"),
                     prior = c(prior(coef = "age.days",
                                     prior = normal(100, 1))),
                     chains = 4)
fixef(seal_lm_bayes)
fixef(seal_lm_prior)

seal_prior_waic <- waic(seal_lm_prior)

loo_compare(seal_waic, seal_prior_waic)




