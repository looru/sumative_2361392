#!/usr/bin/env Rscript

library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(reshape2)   # for acast()

# ----------------- Load data -----------------
df <- read_tsv("data/processed/pmid_year_title_abstract.tsv",
               show_col_types = FALSE)

data("stop_words")

# keep rows with a valid year
df <- df %>%
  filter(year != "") %>%
  mutate(year = as.integer(year))

# ----------------- Choose years to compare -----------------
# Pick the top 4 years with the most articles (adjust n if you like)
years_to_use <- df %>%
  count(year, sort = TRUE) %>%
  slice_head(n = 4) %>%
  pull(year)

cat("Years used in comparison cloud:", paste(years_to_use, collapse = ", "), "\n")

df_sub <- df %>%
  filter(year %in% years_to_use)

# ----------------- Choose text source -----------------
# Option 1: use titles
text_df <- df_sub %>% select(PMID, year, text = title)

# Option 2: use abstracts instead (uncomment if preferred)
# text_df <- df_sub %>% select(PMID, year, text = abstract)

# ----------------- Tokenize and clean -----------------
tokens <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "\\d"))   # remove digits

# word frequencies by year
freq <- tokens %>%
  count(word, year, sort = TRUE)

# ----------------- Cast to word x year matrix -----------------
# rows = words, columns = years, values = counts
freq_mat <- freq %>%
  mutate(year = as.factor(year)) %>%
  acast(word ~ year, value.var = "n", fill = 0)

# ----------------- Comparison cloud -----------------
set.seed(123)

# Optional: save to file instead of just plotting to the screen
png("figures/comparison_cloud_by_year.png",
    width = 900, height = 600)

comparison.cloud(
  term.matrix   = freq_mat,
  max.words     = 200,
  random.order  = FALSE,
  colors        = brewer.pal(n = ncol(freq_mat), name = "Dark2"),
  title.size    = 1.5
)

dev.off()

cat("Saved comparison cloud to: figures/comparison_cloud_by_year.png\n")
