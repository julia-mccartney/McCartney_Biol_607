#'----------------------------
#'@title: Data Transformation Code-Along
#'@author: Julia
#'@date: 12 Sept 2020
#'
#'----------------------------

# Load Packages ####

library(nycflights13)
library(tidyverse)

# nycflights13 ####

flights

# dyplyr basics ####

# Focusing on filter, arrange, select, mutate, summarise, and group_by

# Filter ####

filter(flights, month == 1, day ==1)

# Operators ####

# & is 'and'
# | is 'or'
# ! is 'not'

filter(flights, month == 11 | month == 12) # pull all flights that left in November OR December

#can also use %in% to get the same result
filter(flights, month %in% c(11, 12))

# Exercises ####

#' Find all flights that

#Had an arrival delay of two or more hours
filter(flights, arr_delay > 120 )

#Flew to Houston (IAH or HOU)
filter(flights, dest == c("IAH", "HOU"))

#Were operated by United, American, or Delta
filter()

#Departed in summer (July, August, and September)
#Arrived more than two hours late, but didn’t leave late
#Were delayed by at least an hour, but made up over 30 minutes in flight
#Departed between midnight and 6am (inclusive)
#Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenges?
  
#  How many flights have a missing dep_time? What other variables are missing? What might these rows represent?
  
#  Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)


