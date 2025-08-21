########################################################
# Diversity indices from the LDA topic modelling results 
# Bruno Paese
# Last updated: July 28, 2025
########################################################

## Clean console and global environment --------------------------------------
cat("\014")
rm(list = ls())

## Packages, global variables, and functions ---------------------------------
library(tidyverse)
library(here)

## Load user-written functions
source(here("code","03_Toolbox.R"))

# Load and prepare data ------------------------------------------------------
load(here("output","results_tm.Rdata"))

# Group by year and calculate the yearly mean posterior probability for each topic
# The mean posterior probability is considered to be the proportions of documents we can expect to have.
yearly_prob <- theta_year %>%
  group_by(year) %>%
  summarise(across(.cols = 2:27, .fns = mean))

# Diversity indicees ---------------------------------------------------------
## Gini-Simpson measures of diversity
# Calculate the Gini-Simpson entropy and diversity indices
gini_simpson_index <- yearly_prob %>%
  rowwise() %>%
  mutate(gs_index = gini_simpson(c_across(2:27))) %>%
  select(year, gs_index)
gini_simpson_index <- gini_simpson_index %>%
  mutate(gs_diversity = 1/(1-gs_index))

# Plot the Gini-Simpson entropy index
graph_out <- ggplot(data = gini_simpson_index, aes(x = year, y = gs_index)) +
  geom_point() +
  theme_bw() +
  labs(x = "Year",
       y = "GS") +
  theme(axis.text.y = element_text(size = 10))
graph_out
ggsave(filename = here("output","figures","gs_entropy.pdf"), plot = graph_out)
rm(graph_out)

# Plot the Gini-Simpson diversity index
# graph_out <- ggplot(data = gini_simpson_index, aes(x = year, y = gs_diversity)) +
#   geom_point() +
#   theme_bw() +
#   labs(x = "Year",
#        y = "Gini-Simpson Diversity Index") +
#   theme(axis.text.y = element_text(size = 10))
# graph_out
# ggsave(filename = here("output","figures","gs_diversity.pdf"), plot = graph_out)
# rm(graph_out)

## Shannon-Wiener measures
# Calculate the Shannon-Wiener entropy and diversity indices
shannon_wiener_index <- yearly_prob %>%
  rowwise() %>%
  mutate(h_index = shannon(c_across(2:27))) %>%
  select(year, h_index)
shannon_wiener_index <- shannon_wiener_index %>%
  mutate(h_diversity = exp(h_index))

# Plot the Shannon-Wiener entropy index
graph_out <- ggplot(data = shannon_wiener_index, aes(x = year, y = h_index)) +
  geom_point() +
  theme_bw() +
  labs(x = "Year",
       y = "H") +
  theme(axis.text.y = element_text(size = 10))
graph_out
ggsave(filename = here("output","figures","h_entropy.pdf"), plot = graph_out)
rm(graph_out)

# Plot the Shannon-Wiener diversity index
# graph_out <- ggplot(data = shannon_wiener_index, aes(x = year, y = h_diversity)) +
#   geom_point() +
#   theme_bw() +
#   labs(x = "Year",
#        y = "Shannon-Wiener Diversity Index") +
#   theme(axis.text.y = element_text(size = 10))
# graph_out
# ggsave(filename = here("output","figures","h_diversity.pdf"), plot = graph_out)
# rm(graph_out)