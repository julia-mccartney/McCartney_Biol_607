#'----------------------------------------------
#'
#'@title: Functions!!
#'@author: Julia
#'@date: 29 Sept 2020
#'
#'----------------------------------------------


# Faded examples of functions ####

# add one
add_one <- function(x){
  ret_value <- x + 1
  return(ret_value)
}

add_one(3)

# square root
square_root <- function(x){
  ret_value <- sqrt(x)
  return(ret_value)
}

square_root(16)


# Max minus min

max_minus_min <- function(x){
  ret_value <- max(x) - min(x)
  return(ret_value)
  
}
max_minus_min(c(4,7,1,6,8))


# Exercise 1 ####

# 1. Takes a vector and sums it up after it squares it
#     use c(4,5,6) to test (= 77)

square_sum <- function(x){
  squares <- x^2
  sumof <- sum(squares)
  return(sumof)
}
square_sum(c(4,5,6))

# 2. Takes a number and combine it into a string with the word “elephants” using paste().
#     1 elephants, 2 elephants, 15 elephants

how_many_elephants <- function(x){
  ret_val <- paste(x,"elephants", sep = " ")
  return(ret_val)
}

how_many_elephants(c(1,2,15))

# 3. Takes a number, a string, and a separator and combines them
#     my_function(3, “hello”, “-”) makes “3 - hello”


combination_of_numstring <- function(x, y, separator){
  ret_val <- paste(x,separator,y, sep = " ")
  return(ret_val)
}
combination_of_numstring(3,"hello", separator = "-")

#' EC. Write a function that takes a sample size, mean, SD, and number of sims, and 
#' returns a data frame with a mean and SE of said mean. Have it default to 100 sims.
#' WHY ARE WE DOING THIS TO OURSELVES?
library(purrr)
library(dplyr)

WHY <- function(samp_size, mean_val, SD, sims = 100){
  
  sim_samp <- replicate(sims,
            rnorm(samp_size, mean = mean_val, sd = SD))
  
  sim_mean <- mean(sim_samp)
  
  SE <- sd(sim_samp)
  
  return(data.frame(sim_mean, SE))
}

WHY(samp_size = 200, mean_val = 30, SD = 12, sims = 600)

# to save df
why_df <- WHY(samp_size = 200, mean_val = 30, SD = 12, sims = 600)
why_df


# How Jarrett would write it


mean_and_se_sim <- function(n, m, s, sims = 100){
  # generate simulated samples from a population
  samps <- replicate(sims, 
                     rnorm(n, m, s))
  # take the means of those samples
  means <- colMeans(samps) # must use bc output is matrix
  # calculate the mean of the means and sdm
  out <- data.frame(mean = mean(means), se_mean = sd(means))
  # return
  return(out)
}

set.seed(293847)
mean_and_se_sim(10, 5, 3)

WHY(10,5,3)
