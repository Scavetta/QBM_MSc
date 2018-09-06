# Examples with dplyr functions

# Clear workspace:
rm(list = ls())

# Load packages
library(tidyverse)

# Element 10 from workshop: the dplyr package
protein.df <- read.delim("Protein.txt")

# Convert to a tibble
protein.df <- as_tibble(protein.df)

# 3 main component to dplyr: Grammar of data analysis
# 1 - the pipe operator %>% (puncuation)
# e.g. Takes values on left and sticks it 
# into the first position of function on the right.
read.delim("Protein.txt") %>% 
  as_tibble() -> protein.df

# 2 - 5 verbs
# 2a - filter() (works on rows)
# 2b - arrange() (lowest to highest "ascending")
# 2c - select() (works on columns)
protein.df %>% 
  filter(Contaminant != "+") %>%
  arrange(Ratio.H.M) %>%
  select(Uniprot, Ratio.M.L, Ratio.H.M)

# 2c+ - Helper functions (see table in book)
protein.df %>% 
  filter(Contaminant != "+") %>%
  arrange(Ratio.H.M) %>%
  select(Uniprot, starts_with("R"), -ends_with("Sig"))

# Applying functions
# 2d - mutate(), for Transformation functions
protein.df %>% 
  filter(Contaminant != "+") %>%
  mutate(Ratio.H.M = log2(Ratio.H.M),
         Ratio.M.L = log2(Ratio.M.L))

# Better, use the helper functions:
protein.df %>% 
  filter(Contaminant != "+") %>%
  mutate_at(vars(starts_with("R"), -ends_with("Sig")), log2) %>% 
  mutate_at(vars(starts_with("I")), log10) %>% 
  mutate(Intensity.H.M = Intensity.H + Intensity.M,
         Intensity.M.L = Intensity.M + Intensity.L) -> preview

glimpse(preview)

# 2e - summarise(), for Aggregration functions
# Return to main tutorial

