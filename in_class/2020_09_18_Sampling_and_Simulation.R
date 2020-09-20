#'------------------------------------------
#'Sampling and Simulation Lab
#'Julia McCartney
#'9-18-2020
#'https://biol607.github.io/lab/02_sim_samp.html#5_Bootstrapped_Standard_Errors
#'------------------------------------------

#Accuracy - lack of bias and representativeness
# lower ci not so swayed by outliers?


# Pipes ####

vec <- 1:10

# we want the log of the sqrt of the length of vec
len <- length(vec)
sq <- sqrt(len)
log(sq)

# Works but lengthy

# Could do nested functions

log(sqrt(length(vec)))

#or
log(
  sqrt(
    length(vec)
  )
)

# Welcome to the pipe!
library(magrittr)

1:10 %>% #want a vector of 1:10
  length() %>% # take its length
  sqrt() %>% # take sqrt of length
  log() # take the log

# Exercise set 1 ####

#1. . Use pipes to sum the log of 100:200.

100:200 %>% #vector 100:200
  log() %>% #take the log
  sum() # sum the log

#2. Use pipes to take the square root of the mean of 100 random uniform numbers.

runif(100, min = 0, max = 500) %>% 
  mean() %>% 
  sqrt()

#3. Let’s dive into the guts of R. Using the mtcars data frame, get 
#it’s summary and str that summary. What do you get back?

data("mtcars")

mtcars %>% 
  summary() %>% 
  str()

# Plotting with base plot ####

vals <- runif(n = 1000, min = -10, max = 10)

hist(vals)

# scatter plot
my_df <- data.frame(x=1:10, y = 1:10)

plot(y ~ x, data = my_df)

# tidyverse and dplyr ####

library(dplyr)

# mutate
mtcars2 <- mutate(mtcars, log_mpg = log(mpg))


# in base R, this also works
mtcars$log_mpg <- log(mtcars$mpg)
  
# can make multiple columns at once

mtcars2 <- mtcars %>% 
  mutate(log_mpg = log(mpg),
         sqrt_cyl = sqrt(cyl))  

# group_by
# Lets you group by a particular property and do something with it specifically

#I want to add a column that has the average mpg for each number of gears
head(mtcars2)

mtcars_group <- mtcars %>% 
  group_by(gear) %>% 
  mutate(ave_mpg = mean(mpg)) %>% 
  ungroup() # DONT FORGET TO UNGROUP
head(mtcars_group)

#tibbles are basically df but with a little bit more info on top  

# summarize
# We want to create a derived data set
# Lets say we want the avg and sd of mpg by gear AND ONLY THAT

mtcars_summary <- mtcars %>% 
  group_by(gear) %>% 
  summarize(avg_mpg = mean(mpg),
            sd_mpg = sd(mpg))

# filter (subsetting!)

#remove all data where the no of cyl = 3
mtcars_filter <- mtcars %>% 
  filter(cyl != 3)
#also filter(!(cyl=3))
  
# select (Use sparingly!)
# allows you to subset your columns, but gets rid of all other columns
  
#just MPG
mtcars %>% 
  select(mpg) %>% 
  head()

# how about everything BUT mpg?
mtcars %>% 
  select(-mpg) %>% 
  head()

#jsut a few columns
mtcars %>% 
  select(gear,carb,disp) %>% 
  head(
  )

#look into reorg?

#all columns with an m in them
mtcars %>% 
  select(contains("m"), cyl) %>% 
  head()

# Exercise set 2 ####

# DPLYR Exercises
#1. Add some columns to mtcars to plot the log of mpg by the 
#square root of hp.

mtcars_logplot <- mtcars %>% 
  mutate(log_mpg = log(mpg),
         sqrt_hp = sqrt(hp))

plot(log_mpg ~ sqrt_hp, data = mtcars_logplot)


#2. Get the average hp per gear and plot them against each other.

mtcars_hpAV <-  mtcars %>% 
  group_by(gear) %>% 
  mutate(hp_av = mean(hp)) %>% 
  ungroup()
#could have also used summarize instead of mutate and no ungroup
plot(hp_av ~ gear, data = mtcars_hpAV)
  
  
#3. Make a data fame for only 6 cylinder engines with only the 
#disp and carb columns. Create a boxplot of how carb influences disp.
# boxplot()

mtcars_6cyl <- mtcars %>% 
  filter(cyl == 6) %>% #not group by!
  select(disp, carb)
boxplot(disp ~ carb, data = mtcars_6cyl)

#how to use pipes with plots, so:
mtcars_6cyl <- mtcars %>% 
  filter(cyl == 6) %>% #not group by!
  select(disp, carb) %>% 
  boxplot(disp ~ carb, data = .) # the df being passed is a ., so we can use that place holder!

# for fun
#To understand !, for funnies, try 1:10 == 5 versus 1:10 != 5 to see the difference in output

1:10 == 5
1:10 != 5


# Simulation ####

#three functions to do this
#' 1 - base::replicate()
replicate(n=10, sum(1:100))

#' 2 purrr:map_dbl
library(purrr)
#have a lot of split/apply/combnine functions
#maps are 'typed'
#use ~ for complex function
map_dbl(1:10, ~sum(1:100))
map_dbl(1:10, ~sum(1:.x)) #.x is the stand-in variable here

#what if i want a df?
map_df(1:10, ~data.frame(x=.x,
                 y = sum(1:.x)))

#can couple this with random


# Exercise set 3 ####

#1. Use replicate() to repeatedly average the numbers 1:10 seven times.

replicate(n=7, mean(1:10))


#2. Do the same thing with map_dbl() - also what happens if you use other map functions?
  
map_dbl(1:7, ~mean(1:10))

map_df(1:7, ~data.frame(x = .x,
                        y = mean(1:10)))

#3. Start with a vector:
#my_vec <- c(11, 10, 10, 9, 10, 11, 10, 9, 10, 
#              12, 9, 11, 8, 11, 11, 10, 11, 10, 
#              11, 9)
#Use map_df() to make a data frame that, for the numbers 3 through 15, returns two columns. 
#One is the the average of the element of the vector 1 through the chosen number, the second is 
#the standard deviation. e.g. 10.2 for a mean - but the 10 will be .x for you!
  
my_vec <- c(11, 10, 10, 9, 10, 11, 10, 9, 10, 
            12, 9, 11, 8, 11, 11, 10, 11, 10, 
            11, 9)

map_df(3:15, ~data.frame(
                         av = mean(my_vec[1:.x]),
                         sd = sd(my_vec[1:.x])))

#This does not work:
#map_df(my_vec[3:15], ~data.frame(x = .x,
#                                 av = mean(1:.x),
#                                 sd = sd(1:.x)))

#because this makes a vector out of my_vec[3:15] and then uses that instead of 1:3, 1:5, etc, 
# it is 1:10, 1:9, etc... 


# for loops

x <- numeric(length = 10)
for(i in 1:10){
  x[i] <- sum(1:i)
}
x

#' 3





