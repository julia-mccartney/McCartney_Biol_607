#'---------------------------------
#'
#'@title: Simulation of Sampling to answer
#'HOW BIG SHOULD MY SAMPLE SIZE BE?!?!?!?!
#'
#'@author: Julia McCartney
#'
#'@date: 9/21/2020
#'
#'-----------------------------

# Libraries ####
library(dplyr)
library(purrr)

# Random number generators #### 

rnorm(n=5, mean = 3, sd = 2)
hist(rnorm(1000, mean = 3, sd = 2))

#Or!
rnorm(n=5, mean = 3, sd = 2) %>% 
  hist()

rnorm(1000, mean = 3, sd = 2) %>% 
  density() %>% 
  plot()


# Random number generators are not truely random, they are pseudorandom!
# lets set the seed
set.seed(607)
rnorm(5, mean = 5, sd = 2)


# simulate flipping a coin - binomial
# How many heads do we get if we flip an unbiased coin 10 times?

rbinom(1,prob = 0.5, size = 10)

#repeat the 10 flips for 100 trials (100 times)
rbinom(100,prob = 0.5, size = 10)


# Get a random number from a uniform distribution
runif(n = 50, min = -5, max = 5)

#if you want random whole numbers...
runif( 10, min = 0, max = 10) %>% 
  round()

# Assuming we have intuition about a population, let's sample it! #### 
# Measuring length of some adult fish in cm

# Lets assume...
mean_pop <- 45
sd_pop <- 15

# lets set up our simulation
# What sample sizes do we need?

sample_sim <- data.frame(samp_size = 3:50)

# Now - simulate 
# if we wanted random draws 
samp_sim_one_replicate <- samp_sim %>% 
  group_by(sample_size)

# OR use row wise! like group by but works by row number
samp_sim_one_replicate <- sample_sim %>% 
  rowwise(samp_size) %>% # group by row number
  summarize(samp = rnorm(samp_size, 
                         mean = mean_pop,
                         sd = sd_pop))

# what did we get
head(samp_sim_one_replicate)
plot(samp ~ samp_size, data = samp_sim_one_replicate)


# Yay, we did it!
# but we only have one draw per sample size
# what if we want to estimate the mean at each sample size with 1000 replicates

samp_sim_means <- sample_sim %>% 
  rowwise(samp_size) %>% # group by row number
  summarize(samp_mean = replicate(1000,
                                   rnorm(samp_size, 
                         mean = mean_pop,
                         sd = sd_pop) %>% 
              mean())
            )

plot(samp_mean ~ samp_size, data = samp_sim_means)


# The comment first approach to simulation with dplyr ####
# what the heck am I doing?????

# basic steps
# 1. Define the mean and the sd of the population (from prelim sample, estimates, other work, etc)
# 2. Define sample sizes you want to look at
# 3. Define how many replicates you want to do
# 4. Randomly generate a population with the mean and sd at each sample size for x replicates


#' Assume some population parameters
#' create a data frame with a variety of plausibl sample sizes/properites
#' for each sample size (set of params)...
#' replicate calculating estimated parameters from a random draw some #
#' of times

# lets get simulated mean and sds to examine sample size ####

# create a data frame with a variety of plausibl sample sizes/properites
mean_pop <- 45
sd_pop <- 15

# create a data frame with a variety of plausibl sample sizes/properites
sim_results <- data.frame(samp_size = 3:50) %>% 
  #for each sample size (set of params)...
  rowwise(samp_size) %>% 
  # replicate calculating estimated parameters 
  #from a random draw some numbers
  # of times
  summarize(samp_mean = replicate(100, 
                                  mean(
                                    rnorm(samp_size, 
                                          mean_pop, 
                                          sd_pop))),
            samp_sd = replicate(100,
                                sd(
                                  rnorm(samp_size, 
                                         mean_pop, 
                                         sd_pop)))
                                  )
                      

sim_results <- data.frame(samp_size = 3:50) %>% 
  #for each sample size (set of params)...
  rowwise(samp_size) %>% 
summarize(map_df(1:100, ~data.frame(sim = .x,
                                    sampmean = mean(rnorm(samp_size, 
                                                          mean_pop, 
                                                          sd_pop))),
                 samp_sd = replicate(100,
                                     sd(
                                       rnorm(samp_size, 
                                             mean_pop, 
                                             sd_pop)))
                                      
                                    ))  
head(sim_results)

# Example Simulations - faded examples #### 
# faded examples - slowly will drop things from code, we need to fill in/fix
# Prep code

set.seed(42)
mean_pop <- 10
sd_pop <- 3
nsim <- 100
sampSim <- data.frame(samp_size = 3:50)

# Given Code 1

sampSim %>% 
  rowwise(samp_size) %>% 
  summarize(samp_mean =
              replicate(nsim,
                        rnorm(samp_size, mean_pop, sd_pop) %>% mean()))

# Code 2 - Median simulations 
median_sims <- sampSim %>% 
  rowwise(samp_size) %>% 
  summarize(samp_median =
              replicate(nsim,
                        rnorm(samp_size, mean_pop, sd_pop) %>% 
                          median()))

plot(samp_median ~ samp_size, data = median_sims)

# Code 3 - interquartile range

smp_iqr <- sampSim %>% 
  rowwise(samp_size) %>% 
  summarize(samp_iqr = 
              replicate(nsim,
                        rnorm(samp_size, mean_pop, sd_pop) %>% IQR()))

plot(samp_iqr ~ samp_size, data = smp_iqr)

# Sample and sift

sim_results_common_sim <- sampSim %>% 
  rowwise(samp_size) %>% 
  summarize()
  


