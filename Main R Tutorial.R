# Data Analysis with R
# Rick Scavetta
# 03.09.2018
# QBM R workshop for MSc

# Clear workspace (environment)
rm(list = ls())

# Load packages
library(tidyverse)

# Basic R syntax:
n <- log2(8) # 2 to the power of what = 8
n
log2(8)

# A Simple Case Study
# Access a built-in dataset:
PlantGrowth

# Explore our data:
# What are the group?
levels(PlantGrowth$group)

# How many groups?
nlevels(PlantGrowth$group)

# Two broad types of variables:
# Continuous (aka quantitative) i.e. weight
# Categorical (aka qualitative, discrete, factor) i.e. group
# Categorical has "groups" or "levels"

# Descriptive statistics:
# Mean of ALL values
mean(PlantGrowth$weight)

# group-wise descriptive stats:
# use ctrl + shift + m to get %>%
# %>%  is the "pipe operator" (say "and then...")
PlantGrowth %>%
  group_by(group) %>% # comment
  summarise(avg = mean(weight),
            stdev = sd(weight),
            n = n()) -> PGSummary

# only use -> in the tidyverse context

# Making plots: using ggplot2 functions
# 3 parts - Data, Aesthetics, Geometries
# Aesthetics: aes(), MAPPING data onto a visual scale (axis)
# e.g. x, y, ymin, ymax, col, shape, size, ...
# Geometries: How will the data look?
# 1 - All data points
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_jitter(width = 0.2, alpha = 0.65)
# alpha controls transparency

# 2 - summary statistics:
# 2a - mean and sd:
ggplot(PGSummary, aes(x = group,
                      y = avg,
                      ymin = avg - stdev,
                      ymax = avg + stdev)) +
  geom_pointrange()

  # geom_col() +
  # geom_errorbar(width = 0.2)

# 2b - box plots:
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot()

# Do Stats:
# Group differences:
# First, build a linear model
# y ~ x means "y as described by x"
# response ~ predictor
plant.lm <- lm(weight ~ group, data = PlantGrowth)
plant.lm

# shortcut to t-tests: use summary
summary(plant.lm) # of a linear model

# summary() is just a generic function
summary(PlantGrowth) # of a dataset (i.e. dataframe)

# So how to do a direct t-test?
sleep
# The data is "paired" i.e. same individual on each treatment:
t.test(extra ~ group, data = sleep, paired = TRUE)



# One-way ANOVA:
anova(plant.lm) # gives ANOVA table

# One last way: Compare all pair-wise t-test
# Tukey Post-hoc test:

# to do this, set up ANOVA in a different way:
# use aov() instead of lm()
plant.aov <- aov(weight ~ group, data = PlantGrowth)

summary(plant.aov) # of an aov object give ANOVA table

TukeyHSD(plant.aov) # All pair-wise t-tests:

# Element 2: Functions
# Everything that happens, is because of a function

# Arithmetic operators
# +, -, *, /, ^

34 + 6

# this is actually a function
`+`(34, 6)

# Order or operations
# BEDMAS - brackets, exp, div, mult, add, sub
2 - 3/4 # 1.25
(2 - 3)/4 # -0.25

# Make some objects
n <- 34
p <- 6

# use them like numbers
n + p

# Form of functions:
# fun_name(fun_args)

# fun_args can be:
# named or unnamed (positional matching)
log2(8)
log2(x = 8)
log(x = 8, base = 2)
log(8, 2) # positional matching
log(8, base = 2) # combination, typical

# Some basic and common functions:
# Combine/concatenate: unnamed arguments
xx <- c(3, 8, 9 , 23)
xx

myNames <- c("healthy", "tissue", "quantity")
myNames

# Sequential numbers: seq()
seq(from = 1, to = 100, by = 7)
foo1 <- seq(1, 100, 7)

# of course, we can use objects in functions:
foo2 <- seq(1, n, p)

# trick: a regular interval of 1
# the colon operator
1:10
seq(1, 10, 1)

# Two major types of math functions:
# 1 - Transformation functions (e.g. log)
# output length == input length
# EVERY value is treated the same way
log(foo1)

# 2 - Aggregration functions (e.g. sum, mean)
# output lenght typically 1 (or a few) number(s)
# e.g.
mean(foo1)
median(foo1)
sum(foo1)

# Exercise 6.1
foo2
foo2 + 100 # trans
foo2 + foo2 # trans
sum(foo2) + foo2 # agg followed by trans
1:3 + foo2 # trans

1:4 + foo2

############### Key Concept in R
############### Vector Recycling!

# it's why this works:
# z-scores:
(foo1 - mean(foo1))/sd(foo1)

# Short cut:
scale(foo1)

# Exercise 6.2: linear model transformation
# y = mx + b
m <- 1.12
b <- -0.4
xx

m * xx + b

# What if... I had two m values
m2 <- c(0, 1.12)
# I want to get 8 numbers as output
0 * xx + b
1.12 * xx + b
# but... I get only one series
m2 * xx + b

# How can I reiterate a function over all values
# (or subsets) of a data set?

# 1 - Make your own function
equation <- function(x) {
  1.12 * x + (-0.4)
}

equation(xx)

# Exercise 6.4 - defining a function
# With default values
lin <- function(x, m = 1.12, b = -0.4) {
  m * x + b
}

# m & b in our function are NOT from the environment
rm(m) # delete m
rm(b) # delete b

lin(xx) # Only one arg, m & b use defaults
lin(xx, 5, 60) # All args
lin(xx, b = 10) # Only two args, m uses default

# Examples of functions with no arguments
ls()
list.files()

lin(xx, m = c(0, 1.12)) # still not working,
# We need to reiterate over the m values

# 2 - Using map() from the purrr package (part of tidyverse)
# "map" each "element" of m2 onto a function:
# use . for a place-holder
# use ~ to define which function
map(m2, ~ lin(xx, .))

# Element 3: Objects
# Anything that exists, is an object
# Common data storage:

# Vectors - 1 Dimensional, Homogenous data types
# e.g.
foo1 # 15 elements
foo2 # 6 elements
myNames # 3 elements

# 4 most common user-defined Atomic Vector Types
# Logical - binary, boolean (TRUE/FALSE or T/F)
# Integer - whole numbers
# Double - numbers with decimals
# Character (aka strings)

# "Numeric" refers to both interger and double
# not really common: raw, complex

test <- c(1:10, "bob")
test

# to find out the type use typof()
typeof(test)
typeof(foo1)

# make a couple more objects
foo3 <- c("Liver", "Brain", "Testes", "Muscle",
          "Intestine", "Heart")
typeof(foo3)
foo4 <- c(T, F, F, T, T, F)
typeof(foo4)

# Lists: 1 Dimensional, Heterogenous data types
# e.g.
typeof(plant.lm)

# Take a look inside and getting values:
# 1 - Use attributes (like metadata):
attributes(plant.lm)

# 2 - Use accessor functions:
names(plant.lm)
class(plant.lm)

# 3 - Use the $ for names elements:
plant.lm$coefficients # A named numeric vector of 3 elements
plant.lm$residuals # A numeric vector, 30 elements

# Data Frame - 2 Dimensional, Heterogenous
# A special class of type list
# A collection of vertical vectors of the same lengths
# e.g.
PlantGrowth
typeof(PlantGrowth)
class(PlantGrowth)
# Columns == variables
# Rows == observations

# Make one from scratch:
foo.df <- data.frame(foo4, foo3, foo2)
foo.df

attributes(foo.df) # 3-element long char vector
myNames
names(foo.df) <- myNames
attributes(foo.df)

# Access names elements using $ notation:
foo.df$quantity

# Some typical functions:
# Examine data:
summary(foo.df)
str(foo.df) # structure
glimpse(foo.df) # from tidyverse, dplyr package
dim(foo.df) # (row, col)
nrow(foo.df)
ncol(foo.df)
# don't use length()
length(foo.df) # gives the number of elements in the list

# Two most common problems in R:
# 1 - Wrong data type (in vectors)
# solution - examine is.*() and coerce as.*()
# replace * with type
test
mean(test)
is.numeric(test)
is.na(test)
test <- as.numeric(test)
mean(test, na.rm = T)

# 2 - Wrong structure or format
# solution - rearrange data

# Element 4: Logical Expressions
# Asking and combining (Yes/No) questions
# Relational operators
# == test for equivalence
# != test for non-equivalence
# >, <, >=, <=
# !x, negation of x, where x is a logical vector
# ALWAYS results in a logical vector

n
p
n > p
n < p
!foo4

# Logical operators: Combine Yes/No questions
# & AND - a TRUE in EVERY question
# | OR - a TRUE in at least ONE question
# %in% WITHIN

# Examples with different data types:
# old school: subset()
# new way: dplyr::filter()

# Logical variables
# All healthy
foo.df %>%
  filter(healthy)

# All unhealthy
foo.df %>%
  filter(!healthy)

# Numeric variables (Int of Dbl)
# Below 10
foo.df %>%
  filter(quantity < 10)

# Exactly 31
foo.df %>%
  filter(quantity == 31)

# Range between 10 - 20
foo.df %>%
  filter(quantity > 10 & quantity < 20)

# Meaningless
foo.df %>%
  filter(quantity > 10 | quantity < 20)

# Tail ends (beyond [10,20])
foo.df %>%
  filter(quantity < 10 | quantity > 20)

# Impossible
foo.df %>%
  filter(quantity < 10 & quantity > 20)

# Character variables
# NO pattern matching
# Heart Samples:
foo.df %>%
  filter(tissue == "Heart")

# Liver and Heart Samples:
# Cheap and easy way :)
foo.df %>%
  filter(tissue == "Heart" | tissue == "Liver")

# More efficient: vector recycling
# This doesn't work:
foo.df %>%
  filter(tissue == c("Heart", "Liver"))

# But...
foo.df %>%
  filter(tissue == c("Liver", "Heart"))

# So the real way...
# These are equivalent:
foo.df %>%
  filter(tissue %in% c("Heart", "Liver"))
foo.df %>%
  filter(tissue %in% c("Liver", "Heart"))

# Element 5: Indexing
# Finding information according to position using []

# Vectors:
foo1
foo1[6] # The sixth value
foo1[p] # The pth value, p == 6
foo1[3:p] # 3rd to pth values
foo1[p:length(foo1)] # pth to last value

# use combinations of
# integers, objects, functions, etc...

# But, the exciting part is ... logical vectors!
# i.e. the result of logical expressions
# all values less than 50
foo1[foo1 < 50]

# Data frames: 2 dimensions so use [rows, cols]
foo.df[3, ] # 3rd row, ALL cols
foo.df[ ,3] # ALL rows, 3rd col by number
foo.df[ ,"quantity"] # ALL rows, 3rd col by name

# no comma is a short cut to access columns
foo.df[3]
foo.df["quantity"]

# But compare this to:
foo.df[ ,3]

# R switched to a vector! To prevent this
# use a tibble
foo.df <- as_tibble(foo.df)

# The data frame always remains a data frame:
foo.df[ ,3]

# Can I have a comma with a 1D vector
foo1[,6]
foo1[6,]
# Error!

# Exercises:
# use [] or filter()
# or... even subset() (common but old)

# 1 - 3rd to the 6th rows, only quantity
foo.df[3:6,3]
foo.df[3:6,"quantity"] # a nicer way
foo.df$quantity[3:6] # as a vector

# 2 - Everything except the healthy column
foo.df[,2:3]
foo.df[,-1]
foo.df[,names(foo.df) != "healthy"]
foo.df[,c("tissue", "quantity")]
# also...
foo.df[,-(c(1,3))] # exclude more than one column

# 3 - Tissues that have a quantity less than 10
foo.df[foo.df$quantity < 10, "tissue"]
foo.df$tissue[foo.df$quantity < 10]

# 4 - Which tissue has the highest quantity?
max(foo.df$quantity) # gives actual value
which.max(foo.df$quantity) # Where is it?
foo.df$tissue[which.max(foo.df$quantity)] # index it

# Element 8: Factor Variables (with levels)
# aka categorical, discrete, qualitative (with groups)

# Factor is a special class of type integer
# with labels:
# e.g.
PlantGrowth$group
typeof(PlantGrowth$group) # "integer"
class(PlantGrowth$group) # "factor"

# you can see it here:
str(PlantGrowth)

# some problems:
foo3 # character
foo.df$tissue # factor

str(foo.df)

# convert to a character:
as.character(PlantGrowth$group) # The labels for each level
as.integer(PlantGrowth$group) # The actual value of the level

# Element 9: Tidy Data
source("PlayData.R")

# Make the data tidy using the tidyr package
# Part of the tidyverse
# gather() with 4 arguments:
# 1 - data
# 2&3 - key, value (the names of the OUTPUT columns)
# 4 - either the ID or the MEASURE variables
gather(PlayData, key, value, -c(type, time)) # give ID vars
gather(PlayData, key, value, c(height, width)) # give MEASURE vars

# Assign to new Data Frame
PlayData.t <- gather(PlayData, key, value, -c(type, time))

# To do transformations, it's easiest to have two columns:
# Scenario 1: According to measure and type
PlayData.t %>%
  spread(type, value) -> scenario1
scenario1$A/scenario1$B

# Scenario 2: According to measure and time
PlayData.t %>%
  spread(time, value) -> scenario2
scenario2$`1`/scenario2$`2`

# Scenario 3: According to type and time
# Already possible with the raw data
PlayData$height/PlayData$width

# Element 10: The dplyr functions
# Go to the SILAC protein project for examples

# dplyr functions:
# 2e - summarise(), for Aggregration functions
# 2 - the group_by() adverb
# Apply aggregration functions:
# Scenario 1: According to measure and type
PlayData.t %>%
  group_by(key, type) %>%
  summarise(avg = mean(value))

# Scenario 2: According to measure and time
PlayData.t %>%
  group_by(key, time) %>%
  summarise(avg = mean(value))

# Scenario 3: According to type and time
PlayData.t %>%
  group_by(time, type) %>%
  summarise(avg = mean(value))

#########
# Regular Expressions
# Find patterns in strings or numbers
source("genes.R")
# This makes:
genes

# Find motif: "GGGCCC"
genes == "GGGCCC"
# wrong - This is not pattern matching

# grep - global regular expression, print
grep("GGGCCC", genes) # which gene contains the sequence

# Make use of Regular Expressions:
# Exactly as above, but using special characters
# "{3}" means exactly 3
grep("G{3}C{3}", genes)
grepl("G{3}C{3}", genes)

library(stringr)
str_detect(genes, "G{3}C{3}") # logical vector, like grepl
str_locate(genes, "G{3}C{3}") # logical vector, like grepl

# Less strict: "." means "anything"
grep("G.{4}C", genes)

# Some examples:
genes <- c("alpha4", "p53", "CDC53", "Agft-4", "cepb2")
genes

# Which genes that begin with c (either C, c)
str_extract(genes, regex("^c.*", ignore_case = TRUE))

# Some last examples:
# Indexing and data types review and expansion
# Vectors
foo1[5] # 5th value

# Data frames
foo.df[3,] # 3rd row

# Make a list and index it:
myList <- list(A = 1:3,
               B = 56:85,
               C = foo.df,
               D = plant.lm)
# typeof(myList)
# typeof(plant.lm)

myList$C # view a part using $

# What about []? this always results in a list!
myList[3] # using numbers
myList["C"] # using names

# If you want the actual object as itself, use [[]]
myList[[3]] # using numbers
myList[["C"]] # using names

# Access columns directly:
myList[[3]]$tissue # typical
myList[3]$C$tissue # :/ kinda confusing

# Matrices
# 2D vector - only one data type!
myMatrix <- matrix(1:12, ncol = 3)
myMatrix

myMatrix[,3]
colSums(myMatrix)
rowSums(myMatrix)

# how to find peaks in an ordered vector:
# e.g.
yy <- c(1:5,0:-4,0:7,3:1)

myDF <- data.frame(xx = seq_along(yy),
                   yy = yy)
plot(myDF)

# find peaks in
diff(sign(diff(myDF$yy)))
# Peaks produce -2
# Minima produce 2

# So where are they?
which(diff(sign(diff(myDF$yy))) == -2) + 1 # peaks
which(diff(sign(diff(myDF$yy))) == 2) + 1 # minima


