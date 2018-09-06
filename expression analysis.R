# Effects of meditation on some pro-inflammatory genes
# Rick Scavetta

# Clear workspace
rm(list = ls())

# Load packages
library(tidyverse)

# Import data
medi <- read.delim("Expression.txt")

# Gather & separate
medi %>% 
  as_tibble() %>% 
  gather(key, value) %>% 
  separate(key, c("treatment", "gene", "time"), "_") -> medi.t


# look at group:
medi.t %>% 
  group_by(gene, treatment, time) -> example
example
attributes(medi.t)
attributes(example)

# Calculate Summary Statistics
medi.t %>% 
  group_by(gene, treatment, time) %>% 
  filter(!is.na(value)) %>%
  summarise(average = mean(value),
            n = n(),
            stdev = sd(value),
            SEM = stdev/sqrt(n)) -> medi.sum
posn.d <- position_dodge(0.4)

medi.sum %>% 
  ggplot(aes(x = time, y = average,
             ymin = average - stdev,
             ymax = average + stdev,
             col = treatment)) +
  geom_pointrange(position = posn.d) +
  geom_line(aes(group = treatment), position = posn.d) +
  facet_grid(. ~ gene) +
  scale_colour_manual(values = c("#51D2B5", "#F05719"))

# Alternative geoms:
  # geom_point() +
  # geom_errorbar(width = 0.2)

# Apply inferential statistics: ANCOVA
# medi.t %>% 
#   filter(gene == "RIPK2", !is.na(value)) %>% 
#   aov(value ~ treatment + time, data = .) %>% 
#   summary()

# For only one gene: Output is a list
medi.t %>% 
  filter(gene == "RIPK2", !is.na(value)) %>% 
  lm(value ~ treatment + time, data = .) %>% 
  anova()

# For each gene:
library(broom)
medi.t %>% 
  filter(!is.na(value)) %>% 
  group_by(gene) %>% 
  do(tidy(anova(lm(value ~ treatment + time, data = .)))) -> medi.ancova

medi.ancova


savePlot <- function(x) {
  p <- x %>% 
    ggplot(aes(x = time, y = value,
               col = treatment)) +
    geom_point(position = posn.d) +
    scale_colour_manual(values = c("#51D2B5", "#F05719"))
  ggsave(paste0(names(x),".png"))
}

medi.t %>% 
  filter(!is.na(value)) %>% 
  group_by(gene) %>% 
  do(savePlot(.))
