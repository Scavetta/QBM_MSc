# SILAC analysis
# Rick Scavetta
# 04.09.2018
# QBM R workshop for MSc

# Clear workspace (environment)
rm(list = ls())

# Load packages
library(tidyverse)

# read in the data
protein.df <- read.delim("Protein.txt")

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
protein.df %>% 
  filter(Uniprot %in% paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE")) %>% 
  select(Uniprot, Ratio.M.L, Ratio.H.M)

# Get low p-value proteins:



# Get extreme log2 ratio proteins:







