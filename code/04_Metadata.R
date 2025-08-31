######################################################################
# Metadata from the Web of Science data using the bibliometrix package
# Bruno Paese
# Last updated: August 31, 2025
######################################################################

# Clean console and global environment ---------------------------------------
cat("\014")
rm(list = ls())

## Packages ------------------------------------------------------------------
library(here)
library(tidyverse)
library(stargazer)
library(bibliometrix)
library(huxtable)

## Load data -----------------------------------------------------------------
#data <- convert2df(file = here("input", "dissertation_egei_4.bib"), dbsource = "wos", format = "bibtex")
data <- convert2df(file = here("input", "dissertation_egei_4.txt"), dbsource = "wos", format = "plaintext")
data <- data %>%
  rownames_to_column() %>%
  rename(document = rowname) %>%
  mutate(PY = ifelse(PY == 2025, 2024, PY)) # Some of our documents that were Early Access with publication year equal to 2024 were assigned to a specific volume and issue by the time we retrieved the data for the purpose of our topic modeling. Hence, their years changed to 2025. This line of code is to change their years back to 2024, keeping the data aligned with the previous analyses. 

# Remove documents without abstract
data <- data %>%
  filter(!(is.na(AB)))

# Create subsets for the periods 1991-2002, 2003-2013, and 2014-2024
data1991 <- data %>%
  filter(PY >= 1991 & PY <= 2002)
data2003 <- data %>%
  filter(PY >= 2003 & PY <= 2013)
data2014 <- data %>%
  filter(PY >= 2014 & PY <= 2024)

## Metadata ------------------------------------------------------------------
## Bibliometric information
# Whole dataset
biblio_analysis <- biblioAnalysis(data, sep = ";")
descriptive_summary <- summary(object = biblio_analysis, k = 15, pause = FALSE)
#citations_results <- citations(data, field = "author", sep = ";")

## Annual production of scientific documents
annual_production <- as.data.frame(descriptive_summary$AnnualProduction)
annual_production <- annual_production %>%
  rename(`Year`=`Year   `)

## Most frequent author's keywords, excluding the word migration
author_keywords <- as.data.frame(descriptive_summary$MostRelKeywords)[,1:2]
author_keywords <- author_keywords %>%
  filter(!(`Author Keywords (DE)     ` == "MIGRATION              ")) %>%
  arrange(desc(Articles)) %>%
  mutate(Articles = as.numeric(Articles)) %>%
  rename(`Author keywords`=`Author Keywords (DE)     `)
author_keywords <- author_keywords[1:12,]

# sources <- as.data.frame(descriptive_summary$MostRelSources[1:12,])
# sources <- sources %>%
#   arrange(desc(Articles)) %>%
#   mutate(Articles = as.numeric(Articles)) %>%
#   rename(Sources=`Sources       `)
# 
# authors <- as.data.frame(descriptive_summary$MostProdAuthors[,1:2])
# authors <- authors %>%
#   mutate(Articles = as.numeric(Articles)) %>%
#   rename(Authors=`Authors       `)

# Most productive countries
countries <- as.data.frame(descriptive_summary$MostProdCountries[1:10,c(1,2,4,5)])
countries <- countries %>%
  mutate(Articles = as.numeric(Articles),
         SCP = as.numeric(SCP),
         MCP = as.numeric(MCP)) %>%
  mutate(Country = str_trim(Country) %>% str_to_upper())
citations_country <- descriptive_summary$TCperCountries[1:10,]
citations_country <- citations_country %>%
  rename(Country=`Country     `) %>%
  mutate(`Total Citations`=as.numeric(`Total Citations`),
         `Average Article Citations`=as.numeric(`Average Article Citations`)) %>%
  mutate(Country = str_trim(Country) %>% str_to_upper())
countries <- countries %>%
  left_join(citations_country, by = "Country")
rm(citations_country)

## Figures -------------------------------------------------------------------
## Yearly scientific production
graph_out <- ggplot(data = annual_production) +
  geom_col(mapping = aes(x = `Year`, y = Articles), fill = "#0072B2") +
  scale_x_discrete(breaks = seq(1991, 2024, by = 4)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100)) +
  xlab("Year") +
  ylab("Count") +
  theme_light() +
  theme(plot.title = element_text(family = NULL, face = "bold"))
graph_out
ggsave(filename = here("output","figures","annual_production.pdf"), plot = graph_out)
rm(graph_out)

## Most relevant author's keywords (without migration)
graph_out <- ggplot(data = author_keywords, aes(x = reorder(`Author keywords`, Articles), y = Articles)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +  # Flip to make horizontal
  xlab("") +
  ylab("Occurrences") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10))  # Improve readability
graph_out
ggsave(filename = here("output","figures","most_relevant_AK_overall.pdf"), plot = graph_out)
rm(graph_out)

## Most relevant sources
# graph_out <- ggplot(data = sources, aes(x = reorder(Sources, Articles), y = Articles)) +
#   geom_col(fill = "#0072B2") +
#   coord_flip() +  # Flip to make horizontal
#   xlab("") +
#   ylab("Number of documents") +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 10))  # Improve readability
# graph_out
# ggsave(filename = here("output","figures","most_relevant_sources.pdf"), plot = graph_out, width = 16, height = 8, units = "cm")
# rm(graph_out)

## Most relevant authors
# graph_out <- ggplot(data = authors, aes(x = reorder(Authors, Articles), y = Articles)) +
#   geom_col(fill = "#0072B2") +
#   coord_flip() +  # Flip to make horizontal
#   xlab("") +
#   ylab("Number of documents") +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 10))  # Improve readability
# graph_out
# ggsave(filename = here("output","figures","most_relevant_AU.pdf"), plot = graph_out, width = 16, height = 8, units = "cm")
# rm(graph_out)

## Tables --------------------------------------------------------------------
## Most productive countries
countries_table <- as_huxtable(countries)
countries_table[1,] <- c("Country", "Number of Documents", "SCP", "MCP", "Total citations", "Average document citation")
countries_table <- countries_table %>%
  set_align(row = everywhere, col = everywhere, "center") %>%
  set_bold(row = 1,col = everywhere) %>%
  set_bottom_border(row = 1, col = everywhere) %>%
  set_top_border(row=1,col=everywhere) %>%
  set_number_format(row = everywhere, col = 2:5, fmt_pretty()) %>%
  set_number_format(row = everywhere, col = 6, 2) %>%
  set_number_format(row = everywhere, col = 1, NA) %>%
  set_bottom_border(row=nrow(countries_table),col=everywhere) %>%
  set_font_size(6) %>%
  set_height(8) %>%
  set_caption("The ten most productive countries in terms of total number of documents published, collaboration indices SCP and MCP, total number of citations, and average citation per document, 1991-2024.") %>%
  set_caption_pos("bottom") %>%
  set_label("tab:most_prod_countries")
quick_pdf(countries_table, file = here("output","tables","most_prod_countries.pdf")) # Export tables to PDF
cat(to_latex(countries_table), file = here("output","tables","most_prod_countries.tex")) # Export tables to Latex
