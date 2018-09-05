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





