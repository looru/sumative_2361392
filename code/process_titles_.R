#!/usr/bin/env Rscript

library(dplyr)
library(stringr)
library(readr)
library(tidytext)
library(SnowballC)   
library(tibble)


df <- read_tsv("data/processed/pmid_year_title_abstract.tsv",
               show_col_types = FALSE)


title_tokens <- df %>%
  select(PMID, title) %>%
  unnest_tokens(word, title)       


data("stop_words")

title_clean <- title_tokens %>%
  anti_join(stop_words, by = "word") %>%   
  filter(!str_detect(word, "\\d"))        


title_clean <- title_clean %>%
  mutate(stem = wordStem(word)) %>%  
  select(PMID, word, stem)


write_tsv(title_clean, "data/processed/title_tokens_clean.tsv")

cat("Processed tidytext title tokens written to:\n")
cat("  data/processed/title_tokens_clean.tsv\n")
