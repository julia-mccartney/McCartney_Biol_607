#'----------------------------
#'@title: The Grab Bag and Loading Data
#'@author: Julia
#'@date: 2 Oct 2020
#'@link_to_pad: https://pad.riseup.net/p/607-tidy-2020
#'----------------------------

# Good practice to start
setwd(here::here())

# Libraries ####
library(here) # "easy file referencing"

# Navigating R file system in R ####

# First let's make sure we are in the right place (where is my data?)
getwd()

# What files can R see right now?
list.files()

# What is in Data?
list.files("Data")

# The here package

# What directory should I be in for the project?
here()

# to make sure you are in the right place...
setwd(here::here())

# one more trick...
# sometimes you will see ./ and ../

# ./ means relative to the current wd
list.files("./data")

# ../ means go up one directory 
list.files("../")

# Reading in a File ####

# Doing it Old School

sheet1 <- read.csv("data/test_data_sheet1.csv")

# What to do once you read in a file? 
# Always make sure everything worked!!!

names(sheet1) # any spaces are now "."s 
str(sheet1)

# some people have NAs as . or - or NA, you can fix this as you read it in!

sheet1 <- read.csv("data/test_data_sheet1.csv", 
                   na.strings = c("NA", ".", "-")) # now all of those
# strings will be transformed to NA
summary(sheet1)

# Looking at our data at scale
library(visdat)
vis_dat(sheet1)

sheet1[2,2] <- NA
vis_dat(sheet1) # will help us to see missing data too

# to deal with missing data
library(naniar)
miss_var_summary(sheet1)

# skimming your data
library(skimr)
skim(sheet1)

# Idea - data check script!

# A quick summery
# 1. Read your data
# 2. Check the names to know they are
# 3. str your data!
# 4. summarize
# 5. vis_dat
# 6. skimr
# 7. If you have missing values, use naniar

# Read back check??

# Loading data the new school way
library(readr)

sheet2 <- read_csv("data/test_data_sheet2.csv", col_types = "ddc")
# read r allows you to specify col_types
# d - double, c - character, ddc tells read r 1st col is double, 
# second col is double ...
# gives us a tibble, not a data frame
sheet2
# Group ID has a space!
sheet2$`Group ID`

# to clean names 
library(janitor)
clean_names(sheet2) # made all lc, changed spaces to "_"

# pipe it 
sheet2 <- sheet2 %>% 
  clean_names()

sheet2 <- read_csv("data/test_data_sheet2.csv", 
                   col_types = "ddc") %>% 
  clean_names()


# Loading from excel or other things ####
# don't want to use .csv?
library(readxl)

sheet1_xl <- read_excel("data/test_data.xlsx",
                        sheet = "Sheet1") %>% 
  janitor::clean_names()

sheet1_xl

# For google sheets, use googlesheets4

# If you have HUGE data - Look at vroom::vroom() or data.table::fread

# Talking about data ####

# wide and long data
# in wide - every row is a subject, every column in a different measurement
# in long - everything is a column basically. Every row is one measurement


#---------------------------------------------------------------------------------------

# Wide and long dat awith tidyr! ####
#  To follow along https://biol355.github.io/Lectures/13_data_tidy.html

library(tidyr)
library(dplyr)
library(ggplot2)

# some data for mammals!
  mammals <- data.frame(site = c(1,1,2,3,3,3), 
                        taxon = c('Suncus etruscus', 'Sorex cinereus', 
                                  'Myotis nigricans', 'Notiosorex crawfordi', 
                                  'Suncus etruscus', 'Myotis nigricans'),
                        density = c(6.2, 5.2, 11.0, 1.2, 9.4, 9.6)
  )

  mammals
# mammals is long!
  
#what if we want wide mammals?
m_wide <- pivot_wider(mammals, 
                      names_from = taxon,
                      values_from = density)

# what do we have?
m_wide
vis_dat(m_wide)
  
# filling in NAs with a value (0)
m_wide_0 <- mammals %>% 
  pivot_wider(names_from = taxon, values_from = density,
              values_fill = list(density = 0))
  
vis_dat(m_wide_0)

# be suspicious of your pivot! make sure it did what you wanted it to
# what if oops! We had already pivoted and wanted to move forward
# fill in those 0s without pivoting?

mammals_0 <- mammals %>% 
  complete(site, taxon,
           fill = list(density = 0))
mammals_0

# Pivoting long ####
m_long_0 <- m_wide_0 %>% 
  pivot_longer(cols = -site,
               names_to = "species_name", 
               values_to = "density")

# alternate code
names(m_wide_0)

m_long_0 <- m_wide_0 %>% 
  pivot_longer(cols = `Suncus etruscus`:`Notiosorex crawfordi`,
               names_to = "taxon", 
               values_to = "x")


# why wide vs long?
ggplot (m_wide_0,
        aes(x = `Suncus etruscus`, y = `Notiosorex crawfordi`))+
  geom_jitter()

ggplot(m_long_0,
       aes(x = taxon, y = density)) +
  geom_violin() +
  stat_summary(color = color = "red")

# Exercise ####

# Using palmer penguins (is wide)
# make a long data frame for the av of all measurement 
#split by species island and sex

library(palmerpenguins)
penguins <- penguins

penguins_long <- penguins %>% 
  pivot_longer(cols = bill_length_mm:body_mass_g,
               names_to = "metrics",
               values_to = "values")
  
ggplot(penguins_long,
       aes(x = year, y = values, group = metrics)) +
  geom_point() + 
  facet_wrap(~metrics, scales = "free")
  
# I don't know what I am doing today haha

# Jarrett's answer

p_long <- penguins %>% 
  pivot_longer(cols = bill_length_mm:body_mass_g,
               names_to = "measure",
               values_to = "value")
# ok i did this right so far...
  
ggplot( data = p_long, 
        aes(x = species,
            y = value, 
            fill = sex)) + 
  geom_boxplot() +
  facet_wrap(~measure, scales = "free_y")
# ok so you overthought the graph and you basically were there
  