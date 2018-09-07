# SILAC analysis, full tidy version


# Clear workspace:
rm(list = ls())

# Load packages
library(tidyverse)

# Exercise 17.1 Import data
read.delim("Protein.txt", stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  filter(Contaminant != "+") -> protein.df

# Exercise 17.2: Process Intensities
protein.df %>% 
  mutate_at(vars(starts_with("I")), log10) %>%    # Calculate log10
  mutate(H.M = Intensity.H + Intensity.M,                       # Add log10s and rename columns
         M.L = Intensity.M + Intensity.L) %>% 
  select(Uniprot, H.M, M.L) %>%                     # Take columns of interest
  gather(Ratio, Intensity, -Uniprot) -> onlyInt                 # gather and save

# Exercise 17.3: Process Ratios
protein.df %>% 
  select(Uniprot, starts_with("R"), -ends_with("Sig")) %>%        # Calculate log2
  gather(Ratio, Expression, -Uniprot) %>%                                    # Gather
  filter(Ratio != "Ratio.H.L") %>%                                        # Remove uninteresting H.L Ratio
  mutate(Ratio = recode_factor(Ratio,                             # Relabel ratios to match the Int data frame
                                  `Ratio.M.L` = "M.L",
                                  `Ratio.H.M` = "H.M")) %>% 
  group_by(Ratio) %>%                                             # group according to ratios (2 groups)
  mutate(Expression = log2(Expression),                                # log2 transform
         Expression = Expression - mean(Expression, na.rm = T)) -> onlyRatio       # Apply shift

# Exercise 17.4: Process Sig values and merge:
protein.df %>% 
  select(Uniprot, ends_with("Sig")) %>%                              # Take columns of interest
  gather(Ratio, Significance, -Uniprot) %>%                                   # Gather
  filter(Ratio != "Ratio.H.L.Sig") %>%                                        # Remove uninteresting H.L Ratio
  mutate(Ratio = recode_factor(Ratio,                             # Relabel ratios to match the Int data frame
                               `Ratio.M.L.Sig` = "M.L",
                               `Ratio.H.M.Sig` = "H.M"),
         SigCat = cut(Significance,                                       # Make colour labels for sig values
                         c(-Inf, 1e-11, 1e-4, 0.05, Inf),
                         c("<1e-11", "<0.0001", "<0.05", "NS"))) %>%
  full_join(onlyRatio) %>%                                           # Merge with the log2 ratios
  full_join(onlyInt) %>%                                           # Merge with the Intensities
  filter(complete.cases(.), Uniprot != "") %>%                    # Take only observations that have complete data and non-empty Uniprot
  arrange(desc(Significance)) -> allData                                     # Order according to sig so that low sig are plotted first

# Exercise 17.5: Plotting
ggplot(allData, aes(Expression, Intensity, col = SigCat)) +
  geom_point(alpha = 0.5, shape = 16) +
  scale_colour_manual(values = c("red","orange","blue", "black")) +
  facet_grid(. ~ Ratio) +
  labs(x = "Log2 Ratios", y = "Intensity", col = "p-value")
