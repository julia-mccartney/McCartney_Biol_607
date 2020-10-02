#'--------------------------------------
#'
#'@author: Julia
#'@title: Functions pt2 and Data importing
#'@date: 10/1/2020
#'
#'--------------------------------------

#Libraries
library(dplyr)
library(purrr)


# Be Modular!



# Exercise 1 ####

#1. Write a function that will get the mean, sd, median, and IQR of a sample 
#of a population.

sample_stats <- function(samp){
  m <- mean(samp)
  s <- sd(samp)
  med <- median(samp)
  iqr_samp <- IQR(samp)
  return(data.frame(m,s,med,iqr_samp))
}
test <- sample_stats(c(3,5,6,8,2,4,5))

#2. Write a function that uses this to get 1K resampled values to get the 
#statistic and its SE.
  
resample1K <- function(samp, samp_size = length(samp)){
  map_df(1:1000, ~data.frame(
    sample_stats(sample(samp, size = samp_size, replace = TRUE)))) 
}  
    
    
sum_stats <- function() 
  # summarize(se_mean = sd(m),
            #  se_sd = sd(s),
              #se_med = sd(med),
             # se_iqr = sd(iqr_samp))



resample1K(samp = c(3,5,6,8,2,4,5), samp_size = 3:5)


#3. Wrap it all in dplyr magick to get these statistics for sample sizes 3:5, 
#means 2:5, and sd 1:3. Use tidyr::crossing to make the initial data frame.



#EC. Use ggplot2 to look at how the SE of these different statistics changes 
#based on population mean, sample size, and SD


