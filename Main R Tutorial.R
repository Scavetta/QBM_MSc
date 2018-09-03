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
ggplot(PlantGrowth, aes(x = group, y = weight))




