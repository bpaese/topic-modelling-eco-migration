################################################################################################
# LDA topic modelling on the abstracts of scientific work from Web of Science 
# Bruno Paese
# Last updated: August 31, 2025
# Code built following Wouter van Atteveldt and Kasper Welbers (2020)
# Source: https://github.com/ccs-amsterdam/r-course-material/blob/master/tutorials/r_text_lda.md
################################################################################################

## Clean console and global environment --------------------------------------
cat("\014")
rm(list = ls())

## Packages, global variables, and functions ---------------------------------
library(here)
library(tidyverse)
library(bibliometrix)
library(huxtable)
library(quanteda)
library(topicmodels)
library(readtext)
library(textstem)
library(topicmodels)
#library(ldatuning) # This package was built under R version 4.4.3, but it was discontinued under version 4.5.0. The package became unusable after we updated R.
library(viridis) # Package to provide color palettes

## Load global variables
load(here("output","globalvariables.Rdata"))

## Load user-written functions
source(here("code","03_Toolbox.R"))

## Load and prepare data -----------------------------------------------------
data <- convert2df(file = here("input", "dissertation_egei_4.txt"), dbsource = "wos", format = "plaintext")
data <- data %>%
  rownames_to_column() %>%
  rename(document = rowname) %>%
  mutate(PY = ifelse(PY == 2025, 2024, PY)) # Some of our documents that were Early Access with publication year equal to 2024 were assigned to a specific volume and issue by the time we retrieved the data for the purpose of our topic modeling. Hence, their years changed to 2025. This line of code is to change their years back to 2024, keeping the data aligned with the previous analyses. 

# Create a data frame with abstracts only
abstracts <- data %>%
  select(document, AB) %>%
  na.omit() %>%
  rename(abstract = AB) %>%
  as.data.frame()

## Text pre-processing and analysis using the package quanteda ---------------
corp <- corpus(abstracts, docid_field = "document", text_field = "abstract", unique_docnames = TRUE)
summary(corp)
corp

# Define the multi-word expressions we want to treat as single tokens
multi_words <- c("human capital", "brain drain", "brain gain", "foreign direct investment", "european union", "united states", "united kingdom", "climate change", "health care", "asylum seeker")

# Create dictionaries to handle British vs. American spelling (we want to consider only words in American spelling), and different spellings/categories of terms with the same meaning (synonyms and acronyms)
british_to_american <- dictionary(list(
  labor = c("labour", "labor"),
  color = c("color", "colour"),
  neighbor = c("neighbour", "neighbor"),
  center = c("center", "centre")
))
synonyms_acronyms <- dictionary(list(
  european_union = c("european_union", "eu"),
  united_states = c("united_states", "usa", "u.s.a", "u.s.", "u.s"),
  europe = c("europe", "european"),
  united_kingdom = c("united_kingdom", "uk"),
  china = c("china", "chinese", "china's"),
  fdi = c("foreign_direct_investment", "fdi"),
  entrepreneur = c("entrepreneurship", "entrepreneur", "entrepreneurial"),
  region = c("region", "regional"),
  education = c("education","educational"),
  agriculture = c("agriculture","agricultural"),
  health_care = c("health_care", "healthcare")
))

# Vector of words to exclude
words_exclude <- c("migration", "migrant", "migrants", "mobility", "c", "elsevier", "b.v", "ltd", "inc", "de", "la", "article", "paper", "study", "research", "analysis", "per", "use", "can", "jel", "et", "en", "les")

# Tokenize, remove stop words, and replace British spelling words for American ones
tokens_clean <- corp %>%
  quanteda::tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE) %>%
  tokens_remove(c(stopwords("en"), words_exclude)) %>%
  tokens_compound(pattern = phrase(multi_words)) %>%
  tokens_lookup(dictionary = british_to_american, exclusive = FALSE) %>%
  tokens_lookup(dictionary = synonyms_acronyms, exclusive = FALSE) %>%
  tokens_tolower() # Good practice to lowercase before lemmatization

# Apply lemmatization
tokens_lemma <- tokens_clean %>%
  as.list() %>%
  lapply(lemmatize_words) %>%
  as.tokens()

# Create a document-feature matrix (dfm) and trim infrequent terms
dfm <- dfm(tokens_lemma) %>%
  dfm_trim(min_docfreq = 10) # Trim terms that appear in less than 10 documents
dfm

# Top 20 features (words) in our document-feature matrix
topfeatures(dfm, 20)

## Determine the optimal number of topics using the package ldatuning --------
# number_topics <- FindTopicsNumber(
#   stemmed_dfm,
#   topics = seq(from = 2, to = 100, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 123),
#   mc.cores = 6L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(number_topics) # Plot the results
# summary(number_topics)
K <- 26 # Number of topics, validated by the function FindTopicsNumber()

## LDA topic modelling using the package topicmodels -------------------------
dtm <- convert(dfm, to = "topicmodels") # Convert the document-feature matrix to a format that can be used by the topicmodels package
controls_LDA <- list(seed = 13041995, alpha = 1/K, estimate.beta = TRUE, verbose = 0, iter = 2000) # Setting up the control parameters for estimation of our LDA model
topic_model <- LDA(dtm, method = "Gibbs", k = K,  control = controls_LDA) # Training the LDA model

## Visualization of LDA results ----------------------------------------------
## Posterior distribution of terms per topic
get_terms(1,10)

## Top documents per topic
get_docs(1,10)

## Outputs ------------------------------------------------------------------
# Figures -------------------------------------------------------------------
## Word distribution for each topic (top ten terms per topic)
terms_topics <- tidy(topic_model, matrix = "beta") # Beta is the posterior probability of a word belonging to a topic
top_terms <- terms_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Topics 1 to 13
graph_out <- top_terms %>%
  filter(topic <= 13) %>%
  mutate(term = reorder(term,beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 18)) +
      ylab("") +
      xlab("") +
      coord_flip()
graph_out
#ggsave(filename = here("output","figures","top_terms_figure1.pdf"), plot = graph_out, width = 11.69, height = 8.27, units = "in")
rm(graph_out)

# Topics 14 to 26
graph_out <- top_terms %>%
  filter(topic > 13) %>%
  mutate(term = reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  ylab("") +
  xlab("") +
  coord_flip()
graph_out
#ggsave(filename = here("output","figures","top_terms_figure2.pdf"), plot = graph_out, width = 11.69, height = 8.27, units = "in")
rm(graph_out)

## Trend in topics
topics_docs <- tidy(topic_model, matrix = "gamma") # Gamma is the posterior probability of a document belonging to a topic
years_docs <- data %>%
  select(document, AB, PY) %>%
  na.omit() %>%
  select(-AB) %>%
  rename(year = PY)
topic_trends <- left_join(topics_docs, years_docs, by = "document")
topic_trends <- topic_trends %>%
  group_by(year, topic) %>%
  summarize(mean_gamma = mean(gamma), .groups = "drop")

# Topics 1 to 13
graph_out <- topic_trends %>%
  filter(topic <= 13) %>%
  ggplot(aes(x = year, y = mean_gamma, color = factor(topic), linetype = factor(topic))) +
    geom_line(linewidth = 0.4) +
    scale_x_continuous(breaks = seq(1991, 2024, by = 4)) +
    scale_color_viridis(option = "H", discrete = TRUE, name = "Topic") +
    scale_linetype_manual(values = linetypes, name = "Topic") +
    labs(color = "Topic") +
    xlab("") +
    ylab("Mean posterior probability") +
    theme_bw() +
    theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
#ggsave(filename = here("output","figures","topic_trends_1.pdf"), plot = graph_out)
rm(graph_out)

# Topics 14 to 26
graph_out <- topic_trends %>%
  filter(topic > 13) %>%
  ggplot(aes(x = year, y = mean_gamma, color = factor(topic), linetype = factor(topic))) +
  geom_line(linewidth = 0.4) +
  scale_x_continuous(breaks = seq(1991, 2024, by = 4)) +
  scale_color_viridis(option = "H", discrete = TRUE, name = "Topic") +
  scale_linetype_manual(values = linetypes, name = "Topic") +
  labs(color = "Topic") +
  xlab("") +
  ylab("Mean posterior probability") +
  theme_bw() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
#ggsave(filename = here("output","figures","topic_trends_2.pdf"), plot = graph_out)
rm(graph_out)

# Topics with strong orientation toward internal migration
graph_out <- topic_trends %>%
  filter(topic == 1 | topic == 15 | topic == 16 | topic == 17) %>%
  ggplot(aes(x = year, y = mean_gamma, color = factor(topic), linetype = factor(topic))) +
  geom_line(linewidth = 0.4) +
  scale_x_continuous(breaks = seq(1991, 2024, by = 4)) +
  scale_color_viridis(option = "H", discrete = TRUE, name = "Topic") +
  scale_linetype_manual(values = linetypes, name = "Topic") +
  labs(color = "Topic") +
  xlab("") +
  ylab("Mean posterior probability") +
  theme_bw() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
#ggsave(filename = here("output","figures","topic_trends_1.pdf"), plot = graph_out)
rm(graph_out)

# Topics with strong orientation toward international migration
graph_out <- topic_trends %>%
  filter(topic == 4 | topic == 6 | topic == 7 | topic == 9 | topic == 10 | topic == 11 | topic == 18 | topic == 20 | topic == 21 | topic == 23 | topic == 25) %>%
  ggplot(aes(x = year, y = mean_gamma, color = factor(topic), linetype = factor(topic))) +
  geom_line(linewidth = 0.4) +
  scale_x_continuous(breaks = seq(1991, 2024, by = 4)) +
  scale_color_viridis(option = "H", discrete = TRUE, name = "Topic") +
  scale_linetype_manual(values = linetypes, name = "Topic") +
  labs(color = "Topic") +
  xlab("") +
  ylab("Mean posterior probability") +
  theme_bw() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
#ggsave(filename = here("output","figures","topic_trends_1.pdf"), plot = graph_out)
rm(graph_out)

## Graphs of the mean posterior probability of topics
# i.e., topics with the highest probability of being associated with a document in the whole corpus (7.387 documents) over the entire period
# Posterior probability of the topics for each document (the values shown on this table are the same gammas presented on the dataset topics_docs)
theta <- posterior(topic_model)$topics

# Mean posterior probability
topic_prob <- tibble(colMeans(theta))
topic_prob <- topic_prob %>%
  rownames_to_column() %>%
  rename(topic = rowname, post_prob = `colMeans(theta)`) %>%
  mutate(topic = as.numeric(topic))

# Mean posterior probability of the entire corpus over the period 1991-2024
graph_out <- ggplot(data = topic_prob, aes(x = topic, y = post_prob)) +
  geom_col(fill = "#0072B2") +
  theme_bw() +
  scale_y_continuous(limits = c(0, max(topic_prob$post_prob))) +
  scale_x_continuous(breaks = seq(1,26)) +
  labs(x = "Topics",
       y = "Mean posterior probability") +
  theme(axis.text.y = element_text(size = 10))
graph_out
#ggsave(filename = here("output","figures","topic_prob_corpus.pdf"), plot = graph_out)
rm(graph_out)

# Add year to the theta data frame
theta_year <- theta %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(document = rowname) %>%
  left_join(years_docs, by = "document") %>%
  relocate(year, .after = document)

## Topics with the highest probability of being associated with a document over the period 1991-2002
theta_1991 <- theta_year %>%
  filter(year >= 1991 & year <= 2002) %>%
  select(-year) %>%
  column_to_rownames(var = "document")

# Means of the posterior probability
topic_prob_1991 <- tibble(colMeans(theta_1991))
topic_prob_1991 <- topic_prob_1991 %>%
  rownames_to_column() %>%
  rename(topic = rowname, post_prob = `colMeans(theta_1991)`) %>%
  mutate(topic = as.numeric(topic))

# Mean posterior probability of 475 documents over the period 1991-2002
graph_out <- ggplot(data = topic_prob_1991, aes(x = topic, y = post_prob)) +
  geom_col(fill = "#0072B2") +
  theme_bw() +
  scale_y_continuous(limits = c(0, max(topic_prob_1991$post_prob))) +
  scale_x_continuous(breaks = seq(1,26)) +
  labs(x = "Topics",
       y = "Mean posterior probability") +
  theme(axis.text.y = element_text(size = 10))
graph_out
#ggsave(filename = here("output","figures","topic_prob_1991-2002.pdf"), plot = graph_out)
rm(graph_out)

## Topics with the highest probability of being associated with a document over the period 2003-2013
theta_2003 <- theta_year %>%
  filter(year >= 2003 & year <= 2013) %>%
  select(-year) %>%
  column_to_rownames(var = "document")

# Means of the posterior probability
topic_prob_2003 <- tibble(colMeans(theta_2003))
topic_prob_2003 <- topic_prob_2003 %>%
  rownames_to_column() %>%
  rename(topic = rowname, post_prob = `colMeans(theta_2003)`) %>%
  mutate(topic = as.numeric(topic))

# Mean posterior probability of 1.874 documents over the period 2003-2013
graph_out <- ggplot(data = topic_prob_2003, aes(x = topic, y = post_prob)) +
  geom_col(fill = "#0072B2") +
  theme_bw() +
  scale_y_continuous(limits = c(0, max(topic_prob_2003$post_prob))) +
  scale_x_continuous(breaks = seq(1,27)) +
  labs(x = "Topics",
       y = "Mean posterior probability") +
  theme(axis.text.y = element_text(size = 10))
graph_out
#ggsave(filename = here("output","figures","topic_prob_2003-2013.pdf"), plot = graph_out)
rm(graph_out)

## Topics with the highest probability of being associated with a document over the period 2014-2024
theta_2014 <- theta_year %>%
  filter(year >= 2014 & year <= 2024) %>%
  select(-year) %>%
  column_to_rownames(var = "document")

# Means of the posterior probability
topic_prob_2014 <- tibble(colMeans(theta_2014))
topic_prob_2014 <- topic_prob_2014 %>%
  rownames_to_column() %>%
  rename(topic = rowname, post_prob = `colMeans(theta_2014)`) %>%
  mutate(topic = as.numeric(topic))

# Mean posterior probability of 5.038 documents over the period 2014-2024
graph_out <- ggplot(data = topic_prob_2014, aes(x = topic, y = post_prob)) +
  geom_col(fill = "#0072B2") +
  theme_bw() +
  scale_y_continuous(limits = c(0, max(topic_prob_2014$post_prob))) +
  scale_x_continuous(breaks = seq(1,26)) +
  labs(x = "Topics",
       y = "Mean posterior probability") +
  theme(axis.text.y = element_text(size = 10))
graph_out
#ggsave(filename = here("output","figures","topic_prob_2014-2024.pdf"), plot = graph_out)
rm(graph_out)

# Tables ---------------------------------------------------------------------
## Tables with the 15 most likely terms per topic
# Topics 1 to 13
top_terms_table1 <- as_huxtable(terms(topic_model, 15), add_colnames = TRUE)[,1:13]
top_terms_table1 <- top_terms_table1 %>%
  set_align(row = everywhere, col = everywhere, "center") %>%
  set_bold(row = 1, col = everywhere) %>%
  set_bottom_border(row = 1, col = everywhere) %>%
  set_top_border(row = 1, col = everywhere) %>%
  set_right_border(row = everywhere, col = everywhere) %>%
  set_left_border(row = everywhere, col = 1) %>%
  set_bottom_border(row = nrow(top_terms_table1), col = everywhere) %>%
  set_font_size(6) %>%
  set_width(1.54) %>%
  set_caption("Fifteen most likely terms from topics 1 to 13.") %>%
  set_caption_pos("bottom") %>%
  set_label("tab:top_terms_1")
#quick_pdf(top_terms_table1, file = here("output","tables","top_terms_table1.pdf")) # Export tables to PDF
#cat(to_latex(top_terms_table1), file = here("output","tables","top_terms_table1.tex")) # Export tables to Latex

# Topics 14 to 26
top_terms_table2 <- as_huxtable(terms(topic_model, 15), add_colnames = TRUE)[,14:26]
top_terms_table2 <- top_terms_table2 %>%
  set_align(row = everywhere, col = everywhere, "center") %>%
  set_bold(row = 1, col = everywhere) %>%
  set_bottom_border(row = 1, col = everywhere) %>%
  set_top_border(row = 1, col = everywhere) %>%
  set_right_border(row = everywhere, col = everywhere) %>%
  set_left_border(row = everywhere, col = 1) %>%
  set_bottom_border(row = nrow(top_terms_table2), col = everywhere) %>%
  set_font_size(6) %>%
  set_width(1.54) %>%
  set_caption("Fifteen most likely terms from topics 14 to 26.") %>%
  set_caption_pos("bottom") %>%
  set_label("tab:top_terms_2")
#quick_pdf(top_terms_table2, file = here("output","tables","top_terms_table2.pdf"))
#cat(to_latex(top_terms_table2), file = here("output","tables","top_terms_table2.tex"))
