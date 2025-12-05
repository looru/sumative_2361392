#!/usr/bin/env Rscript

library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(readr)


df <- read_tsv("data/processed/pmid_year_title_abstract.tsv",
               show_col_types = FALSE)

data("stop_words")


title_words <- df %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "\\d"))    


top_words <- title_words %>%
  count(year, word, sort = TRUE) %>%
  group_by(year) %>%
  slice_max(n, n = 10) %>%
  ungroup()


ggplot(top_words, aes(x = year, y = n, fill = word)) +
  geom_col(position = "stack") +
  labs(
    title = "Top words in article titles over time",
    x = "Year",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
