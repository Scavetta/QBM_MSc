# SILAC analysis
# Rick Scavetta
# 04.09.2018
# QBM R workshop for MSc

# Clear workspace (environment)
rm(list = ls())

# Load packages
library(tidyverse)

# read in the data
protein.df <- read.delim("Protein.txt", stringsAsFactors = FALSE)

# examine the data:
# summary(protein.df)
# ncol(protein.df)
# dim(protein.df)
glimpse(protein.df)
# str(protein.df)

# print the data frame to the screen:
# protein.df

# use readr version:
# protein.df <- read_tsv("Protein.txt")

# Convert the data frame to a tibble:
# print the data frame to the screen:
protein.df <- as_tibble(protein.df)
protein.df
class(protein.df)

# Examine and Remove contaminats
protein.df %>% 
  filter(Contaminant == "+") -> prot.con

# total cont
nrow(prot.con)

# percentage cont
nrow(prot.con)/nrow(protein.df)*100

# Get a table
table(protein.df$Contaminant)/nrow(protein.df)
summary(protein.df$Contaminant)

# Using a logical vector to do math
# TRUE == T == 1
# FALSE == F == 0
sum(protein.df$Contaminant == "+")

# Remove contaminants:
protein.df %>%
  filter(Contaminant != "+") -> protein.df

# Plot a histogram or a density plot of each ratio:
ggplot(protein.df, aes(x = Ratio.H.M)) +
  geom_histogram()

# Transformations
# Log10 of intensities
protein.df$Intensity.H <- log10(protein.df$Intensity.H)
protein.df$Intensity.M <- log10(protein.df$Intensity.M)
protein.df$Intensity.L <- log10(protein.df$Intensity.L)

# Add intensities
protein.df$Intensity.H.M <- protein.df$Intensity.H + protein.df$Intensity.M
protein.df$Intensity.M.L <- protein.df$Intensity.M + protein.df$Intensity.L

# Log2 of ratios
protein.df$Ratio.H.M <- log2(protein.df$Ratio.H.M)
protein.df$Ratio.M.L <- log2(protein.df$Ratio.M.L)

# What is the shift?
shift.H.M <- mean(protein.df$Ratio.H.M, na.rm = T)
shift.M.L <- mean(protein.df$Ratio.M.L, na.rm = T)

# Adjust values:
protein.df$Ratio.H.M <- protein.df$Ratio.H.M - shift.H.M
protein.df$Ratio.M.L <- protein.df$Ratio.M.L - shift.M.L

# Plot a histogram or a density plot of each transformed ratio:
ggplot(protein.df, aes(x = Ratio.H.M)) +
  geom_histogram()

ggplot(protein.df, aes(x = Ratio.M.L)) +
  geom_histogram()

# Examine Data, Exercises 9.2 - 9.4:
# Get specific Uniprot IDs
# Using filter(), Exercise 9.2
protein.df %>% 
  filter(Uniprot %in% paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE")) %>% 
  select(Uniprot, Ratio.M.L, Ratio.H.M)
# Using [], Exercise 10.1
protein.df[protein.df$Uniprot %in% 
             paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE"), 
           c("Uniprot", "Ratio.M.L", "Ratio.H.M")]

# Get low p-value proteins:
# Using filter(), Exercise 9.3
protein.df %>% 
  filter(Ratio.H.M.Sig < 0.05) -> sig.H.M
# Using [], Exercise 10.2
protein.df[protein.df$Ratio.H.M.Sig < 0.05 & 
             !is.na(protein.df$Ratio.H.M.Sig), ]

# Get extreme log2 ratio proteins:
# Using filter(), Exercise 9.4
protein.df %>% 
  filter(Ratio.H.M > 2.0 | Ratio.H.M < -2.0)
# Using [], Exercise 10.3
protein.df[(protein.df$Ratio.H.M > 2.0 | 
             protein.df$Ratio.H.M < -2.0) & 
             !is.na(protein.df$Ratio.H.M), ]

# Proteins for top 20 HM and ML ratios
# Exercise 10.4
protein.df %>% 
  arrange(desc(Ratio.M.L)) %>% 
  filter(row_number()<21)

protein.df %>% 
  top_n(20, Ratio.M.L) -> topML

protein.df %>% 
  top_n(20, Ratio.H.M) -> topHM

# Intersection of top20 lists:
# Exercise 10.5
intersect(topML, topHM) %>% 
  select(Uniprot, Ratio.H.M, Ratio.M.L)

# Exercises 13.1 & 13.2:
# Make a plot coloured according to sig values:
protein.df$Ratio.H.M.Sig.Cat <- cut(protein.df$Ratio.H.M.Sig,
                                    c(0, 1e-11, 1e-4, 0.05, 1))
# optionally, add c("<1e-11", "<1e-04", "<0.05", "NS") to cut().

protein.df$Ratio.M.L.Sig.Cat <- cut(protein.df$Ratio.M.L.Sig,
                                    c(0, 1e-11, 1e-4, 0.05, 1))

glimpse(protein.df)

ggplot(protein.df, aes(x = Ratio.H.M, y = Intensity.H.M, col = Ratio.H.M.Sig.Cat)) +
  geom_point(alpha = 0.5)

ggplot(protein.df, aes(x = Ratio.M.L, y = Intensity.M.L, col = Ratio.M.L.Sig.Cat)) +
  geom_point(alpha = 0.5)

# Pattern Matching with Regular Expressions: Exercises chapter 18
desc <- protein.df$Description
# A character vector

# Which contain methyl
str_extract(desc, regex(".*methyl.*", ignore_case = TRUE)) # long, but clear
str_extract(desc, "methyl") # easier, but only lower case
str_extract(desc, ".*(M|m)ethyl.*") # short RegEx for both upper and lower case

str_extract(desc, "(M|m)ethyl.*ase") # greedy, "methylase and lysyl-hydroxylase"
str_extract(desc, "(M|m)ethyl.*?ase") # ungreedy, "methylase"

# Until the end of the name? More complex :/

# What rows contain “methyl”?
grep("(M|m)ethyl", desc)
str_which(desc, "(M|m)ethyl")
which(str_detect(desc, "(M|m)ethyl"))

# How many?
length(grep("(M|m)ethyl", desc))

# Does case (in)sensitivity make a difference?
identical(str_detect(desc, "methyl"), str_detect(desc, "Methyl"))

# Exercises 18.2 & 18.3:
protein.df %>% 
  filter(str_detect(Description, regex("ubiquitin", ignore_case = T))) %>% 
  select(Uniprot, Ratio.M.L, Ratio.H.M) %>% 
  filter(complete.cases(.)) %>%
  ggplot(aes(Ratio.M.L, Ratio.H.M)) +
  geom_point() +
  labs(title = "Only Ubiquitins")

