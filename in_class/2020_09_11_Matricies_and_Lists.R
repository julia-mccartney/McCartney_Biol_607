#'---------------------------------
#'
#'@title: Matricies, list, and more!
#'@date: 2020-09-11
#'@author: Julia McCartney
#'
#'---------------------------------

# Matrix ####

my_vec <- 1:50

my_matrix <- matrix(my_vec, ncol = 10, byrow = FALSE)
my_matrix
my_matrix[2,2]

my_matrix <- matrix(my_vec, ncol = 10, byrow = TRUE)
my_matrix
my_matrix[2,2]

# let's look more at indicies

#a_matrix[row, column]
my_matrix[2,2]
my_matrix[2,] # rows
my_matrix[,2] # columns

#how big is a matrix?

str(my_matrix)
summary(my_matrix) #gives an independent summary for each column 
length(my_matrix)

# matrix-specific (ish) functions
dim(my_matrix) # row/col again
nrow(my_matrix)
ncol(my_matrix)


# Exercise 1 ####

#' Exercise: Try creating a 10 x 10 matrix of random uniform numbers 
#' between 5 and 50, and get the row and column means. What’s the 
#' output of str()? (rowMeans() and colMeans())

mat1 <- matrix(runif(n = 100, min = 5 , max = 50), ncol = 10)
rowMeans(mat1)
colMeans(mat1)

str(mat1)

# Lists ####

my_list <- list(first = 1:10, 
                second = letters[10:25]) #assigning a name and value
my_list

str(my_list) #different classes and lengths!
#lists can be anything!!

# How do we reference different parts of a list

names(my_list)
my_list$first
my_list[first] # doesn't work
my_list["first"] # does work!
class(my_list["first"]) #...but it is still a list

#subset operator

my_list[["first"]]
class(my_list[["first"]]) #now an integer!

#also, not recommended but works:
my_list[1] # class: list 
my_list[[1]] # class : integer

# think about lists like a dictionary
# can nest lists and other things

# Exercise 2 ####

#' EXERCISE: Try this out. Create a list consisting of a vector of 
#' numbers, an NA, and a list which contains two vectors. What is 
#' there? Also, check out our old friends str and summary

list1 <- list(a = 1:70, b = NA, c = my_list)

str(list1)
summary(list1)

# nested and mixed lists!

big_list <- list(first = 1:10, 
                 second = NA, 
                 third = list(a = letters[1:5],
                              b = LETTERS[1:5],
                              c = list(one = 1,
                                       two = "2")))

big_list
big_list$first
big_list$third$c$one
big_list[[3]][[2]][[1]]
# Ok what is happening here?
# Identifying factors in the lists
# [[3]] - third part of the first list
# then in ^, [[2]] - second part of the 3rd list
#then in [[2]], take the first part of the second part of the 
#first part


# DATA FRAMES - what if lists and matricies had a baby? ####

View(my_matrix)
View(big_list)

data(mtcars)
str(mtcars)

mtcars$mpg
View(mtcars)

#tabular
mtcars[1,5]
mtcars[1,"drat"]
names(mtcars)
mtcars[3:10, c("mpg", "wt")]


# can use matrix functions for df
colMeans(mtcars)
summary(mtcars)


















