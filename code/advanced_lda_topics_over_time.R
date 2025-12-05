#!/usr/bin/env Rscript

# --------------------------------------------------------------------------------------
# The Advanced Latent Dirichlet Allocation (LDA) topic model on article abstracts
# --------------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(tidytext)
library(stringr)
library(ggplot2)
library(topicmodels)
library(tidyr)


# Loading cleaned data

df <- read_tsv("data/clean/pmid_year_title_abstract.tsv",
               show_col_types = FALSE)

data("stop_words")

df <- df %>%
  filter(year != "" & !is.na(year) & abstract != "") %>%
  mutate(
    year = as.integer(year),
    PMID = as.character(PMID)   # for joining with gamma later
  )


# Tokenizing abstracts

abstract_tokens <- df %>%
  select(PMID, year, abstract) %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "\\d")) %>%   # remove digits
  count(PMID, year, word, sort = TRUE)

# 
# Creating DTM for LDA

dtm <- abstract_tokens %>%
  cast_dtm(PMID, word, n)


# Fitting LDA model
#    (k = 5 topics, adjust if needed)

k <- 5
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# Tidying LDA outputs

beta  <- tidy(lda_model, matrix = "beta")   # term-topic probabilities
gamma <- tidy(lda_model, matrix = "gamma")  # topic-document probabilities


# Attaching year to gamma
# -----------------------------
gamma_year <- gamma %>%
  left_join(df %>% select(PMID, year),
            by = c("document" = "PMID"))

---
# Computing mean topic proportion per year

topic_trends <- gamma_year %>%
  group_by(year, topic) %>%
  summarise(mean_gamma = mean(gamma), .groups = "drop")


# Assigning human–readable topic names
#    (based on my interpretation of top terms)

topic_trends <- topic_trends %>%
  mutate(topic_name = case_when(
    topic == 1 ~ "Clinical assessment",
    topic == 2 ~ "Gaming disorder",
    topic == 3 ~ "Smartphone/Social media use",
    topic == 4 ~ "Mental health effects",
    topic == 5 ~ "Epidemiology & risk factors",
    TRUE       ~ paste("Topic", topic)
  ))


# Plotting temporal topic trends

p_trends <- ggplot(topic_trends,
                   aes(x = year, y = mean_gamma,
                       color = topic_name, group = topic_name)) +
  geom_line(linewidth = 1.3) +
  geom_point(linewidth = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Changes in digital addiction and gaming topics over time",
    x = "Year",
    y = "Average topic probability",
    color = "Topic"
  ) +
  theme_minimal(base_size = 14)

ggsave("figures/lda_topics_over_time.png",
       p_trends, width = 10, height = 6, dpi = 300)

cat("Saved temporal topic trends plot to:\n  figures/lda_topics_over_time.png\n")


# Top terms per topic (with names)

top_terms <- beta %>%
  mutate(topic_name = case_when(
    topic == 1 ~ "Clinical Assessment",
    topic == 2 ~ "Gaming Disorder",
    topic == 3 ~ "Smartphone/Social Media Use",
    topic == 4 ~ "Mental Health Effects",
    topic == 5 ~ "Epidemiology & Risk Factors",
    TRUE       ~ paste("Topic", topic)
  )) %>%
  group_by(topic, topic_name) %>%
  slice_max(beta, n = 12) %>%
  ungroup()

write_tsv(top_terms, "figures/lda_topic_top_terms.tsv")
