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
summary(plant.lm)

# One-way ANOVA:
anova(plant.lm)

# One last way: Compare all pair-wise t-test
# Tukey Post-hoc test:

# to do this, set up ANOVA in a different way:
# use aov() instead of lm()
plant.aov <- aov(weight ~ group, data = PlantGrowth)
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
