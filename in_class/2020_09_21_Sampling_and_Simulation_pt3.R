#'---------------------------------
#'
#'@title: Bootstrapping of Sampling to answer
#'HOW BIG SHOULD MY SAMPLE SIZE BE?!?!?!?!
#'
#'@author: Julia McCartney
#'
#'@date: 9/24/2020
#'
#'-----------------------------

# libraries
library(dplyr)
library(purrr)

# Boostrapping is based on repeated draws of a SAMPLE (not pop)
# have sample - assume it is representative of what I would get 
#if you sampled again
options(digits = 8)
# Create a sample ####
set.seed(2020) # the hell seed
samp <- rnorm(40, mean = 10, sd = 3) # create sample - would normally have collected
samp

# One Bootstrap sample ####

#A bootstrap sample is resampled set of values
# with the same n, sampled with replacement

one_boot <- sample(samp,
                   size = length(samp),
                   replace = TRUE)
one_boot

# show that they are drawn from each other
one_boot %in% samp

# Let's get boostrapped medians ####

#need to simulate draws from sample

boot_med <- replicate(1000, #1000
                      sample(samp, # sample from samp
                             size=length(samp),
                             replace = TRUE) %>% 
                        median) # calculate median

# let's look at it!
hist(boot_med)

# the SE of the median is the SD of these bootstraps
sd(boot_median)

# 2/3 confidence interval is
mean(boot_med + sd(boot_med))
mean(boot_med - sd(boot_med))

median(samp)
median(boot_med)

# calculate means
boot_mean <- replicate(1000, #1000
                      sample(samp, # sample from samp
                             size=length(samp),
                             replace = TRUE) %>% 
                        median) # calculate median

sd(boot_mean)
sd(samp)/(sqrt(length(samp)))

# Exercise 1 ####
# Pair programming!

#1) First, make sure you feel comfortable calculating the bootstrapped SE of 
#the IQR from samp. Repeat what we did above with IQR instead of median.

# SE of the IQR from samp
boot_IQR <- replicate(1000, sample(samp, 
                                   size = length(samp),
                                   replace = TRUE) %>% IQR)
sd(boot_IQR)

#2) Now, write out in comments what you will do to end up with a data frame 
#that has a column of sample sizes and a column of IQRs calculated from sampling 
#our samp vector.


# Make a dataframe with a sample size column and an IQR column for sample
# sizes 3 through 40

#3) Code it
#df with column of sample sizes and IQR from samp

samp_IQR <- map_df(3:length(samp), ~ data.frame(sample_size = .x,
                                                IQR = IQR(samp[1:.x])))
samp_IQR

#4) Now, write out in comments how you would go from that data frame to one that 
# has the SE for the IQR at each sample size.

# Well we coded it instead 
samp_IQR <- map_df(3:length(samp), ~ data.frame(sample_size = .x,
                                                IQR = IQR(samp[1:.x]),
                                                SE =  sd(replicate(1000, sample(samp[1:.x]),
                                                                   size = .x,
                                                                   replace = TRUE) %>% IQR)))
samp_IQR

#test


# Answers from Jarrett


#1) First, make sure you feel comfortable calculating the bootstrapped SE of 
#the IQR from samp. Repeat what we did above with IQR instead of median.


boot_iqr <- replicate(1000,
                      sample(samp,
                           size = length(samp),
                           replace = TRUE) %>%  IQR)
sd(boot_iqr) # boostrapped standard error of the IQR
mean(boot_iqr)
IQR(samp)

#2) Now, write out in comments what you will do to end up with a data frame 
#that has a column of sample sizes and a column of IQRs calculated from sampling 
#our samp vector.

# start with a df with a sample size column
# for each sample size
# replicate 
# boostrap draws and get an IQR
# now I have 1K bootsrapped IQRs at different sample sizes 

# 3) Code it!

# start with a df with a sample size column
boot_IQRs <- data.frame(samp_size = 10:20) %>% 
  # for each sample size
  rowwise(samp_size) %>% 
  # replicate 1000
  summarize(boot_IQR = replicate(1000,
                                 #boostrap draws
                                 sample(samp,
                                        size = samp_size,
                                        replace = TRUE) %>% 
                                   #and get an iqr 
                                   IQR()))

boot_IQRs

boot_se_iqrs <- boot_IQRs %>% 
  group_by(samp_size) %>% 
  summarize(se_iqr = sd(boot_IQRs)) # not sure why this isn't working

plot(se_iqr ~ samp_size, data = boot_se_iqrs, 
     type = "l")
