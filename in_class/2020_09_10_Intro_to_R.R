#'--------------------------------------------
#' 
#'  
#'@title: In class coding 
#'  2020-09-10
#' 
#' Now we have created a text box!
#' 
#' Putting a change log at the top is a good idea!
#' Changelog:
#' 
#' 
#' -------------------------------------------

#' ALWAYS BE FUTURE YOU'S FRIEND
#' 
#' use script files for reproducibility! 

#### Exercise 1 ####
#' Exercise: Setup a script file for today! Give it a useful
#' name! Save it in the right place! Makes some comments in
#' it to remind you what it’s all about.

# all done!


#### Basic Math ####

1+1
2^39
39 + (43/3)
93*302

#can use options() (??) to change number of decimals seen

#### Complex Functions ####

sqrt(4)
log(20)
log10(20)


#### How to get help ####

?log # looks for that function and pulls help page

??"logarithm" #searches for string

log(x= 10, base = 10) #careful
log(10, base = 10) #common
log(10,10) #living life on the edge
# all the same, but what does the last one mean??? 

#from Jared?
floor(log(2.72))/ceiling(log(2.71))
?floor

#### Exercise 2 ####

#' Think of a simple mathematical function. Find its 
#' helpfile and then implement it in R. Paste what you 
#' did here.

??"median"

median(1:7)

# Start of 2nd day 2020 09 11

# Variables ####
log

# Some constants are preset in R
pi
exp(1)

foo <- sqrt(2)
foo
sqrt(2) -> foo # also works but not common
foo = sqrt(2) # technically works but not a great idea

# why not?
# 1. History: <-
# 2. Unambiguity: = does not have a direction, can get confusing
# 3. Understand when we are using functions
#    = is reserved for use in specifying function args
# (shortcut: opt + -) but why even do that though??


foo^2
foo +2 
log(foo)
#Now foo functions like any other number!

# variable naming!
#' 
#' Have meaningful names!
#' be careful to not overwrite functions or look like you have one
#' be descriptive, but also easy to read!
#' Dont start with a number
#' Use snake or camel case if multiple words 
#' naming.like.this is not great because '.' have another function
#'   in R

# more than a number ####
# Classes of objects

# numeric
class(foo) 

# characters
text_string <- "This is text"
class(text_string) #characters

#boolean
TRUE
FALSE
true 
class(TRUE) # boolean/logical

TRUE + 0 #TRUE and FALSE underlied by numbers
foo == 1 # equality is a ==
foo < 1 
foo <= 1
foo >= 1

# What about missing values?
NA
class(NA)
NA + 0

# related special variables
NaN #not a number! not missing, just not a number
Inf
-Inf

class(NaN) #numeric (?)
class(Inf)
Inf +1 

# Exercise 3 ####
#' EXERCISE: Make a variable. Now make a variable out of some 
#' math equation. Try adding variables of different classes 
#' together - what happens?

test_num <- 34
test_eqvar <- 45+99*48
string <- "string"

string + NaN
test_num + foo
foo + NaN

1L
class(1L) # class - integer


# Vectors!!! ####

my_vec <- c(1,2,4,5,3,7,64,9,0,21,4)
my_vec
class(my_vec)

class(c("a", "b", "c"))

# Useful character vectors that are hard coded in

letters
LETTERS

# what is the 12th letter of the alphabet?
letters[12]

# a range of values
# use :
1:10

# The first 10 letters of the alphabet?
letters[1:10]

# the letters of the alphabet corresponding to my_vec
letters[my_vec]

# (for functions)
# [for indexing an object]

# Exercise 4 ####

#' EXERCISE: Make two vectors and add them together. First try it 
#' with numbers. Then try vectors of different object types. What 
#' happens?

vec1 <- c(1,2,5,3,6,7,8)
vec2 <- c(2,5,7,4,3,7,9)

vec2 + vec1

let1 <- c("3","4", "t")
let2 <- c("f", "g", "sd")

let2 + let1

let2 + vec2

mix1 <- c("g", 3, TRUE, "KK", 45)

# vectors are great and there are many functions that we use
# to make them!

1:10

seq(from = 1, to = 10, by = .1)
seq(from = 1, to = 10, length.out = 100)

# random numbers

my_unif <- runif(n = 100, min= 13.5, max = 200)

# many functions work on vectors

sum(my_unif)
mean(my_unif)


# str and summarize: what is in that object? ####


str(my_unif) #kind of gives structure (class, length, head)
class(my_unif)
length(my_unif)
head(my_unif)

# sometimes problems are not obvious
na_vec <- c(1:100, NA, 10:100)

str(na_vec) #can't see the problem like this! it looks fine

# summary

summary(na_vec)

# Exercise 5 ####

#' EXERCISE: Create a vector of any class. str() and summarize() it.
#'  Now, create two vectors of different object types. Combine them.
#'   What do these two useful functions tell you what happened?

vec3 <- c(3,65,74,3,21,4,67,84,77,454)
str(vec3)
summary(vec3)

vec4 <- c(letters[1:20])
str(vec4)
summary(vec4)

vec5 <- c(vec3, vec4)
str(vec5)
summary(vec5)

########################################################
# End of basic R!
########################################################
