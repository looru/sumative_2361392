#!/usr/bin/env Rscript

# ============================================================
# LDA topic modelling + topic trends over time (named topics)
# ============================================================

library(dplyr)
library(tidytext)
library(readr)
library(topicmodels)
library(ggplot2)
library(tidyr)
library(stringr)

# Checking output directories existence
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)
if (!dir.exists("data/clean")) dir.create("data/clean", recursive = TRUE)

# Loading data
df <- read_tsv("data/clean/pmid_year_title_abstract.tsv",
               show_col_types = FALSE)

data("stop_words")

df <- df %>%
  filter(!is.na(abstract), abstract != "",
         !is.na(year), year != "") %>%
  mutate(
    year = as.integer(year),
    PMID = as.character(PMID)
  )

# Tokenizing abstracts
abstract_tokens <- df %>%
  select(PMID, abstract) %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words, by = "word") %>%       # remove stopwords
  filter(!str_detect(word, "\\d")) %>%         # remove digits
  count(PMID, word, sort = TRUE)

# Casting to Document-Term Matrix (DTM)
dtm <- abstract_tokens %>%
  cast_dtm(PMID, word, n)

# Fitting LDA model (k = 5 topics)
set.seed(1234)
k <- 5
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# --------------------------------------------
# 1. Top terms per topic (With names)
# --------------------------------------------

beta <- tidy(lda_model, matrix = "beta")  # term-topic probabilities

top_terms <- beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

# Human-readable topic names
top_terms_named <- top_terms %>%
  mutate(topic_name = case_when(
    topic == 1 ~ "Clinical Assessment",
    topic == 2 ~ "Gaming Disorder",
    topic == 3 ~ "Smartphone/Social Media Use",
    topic == 4 ~ "Mental Health Effects",
    topic == 5 ~ "Epidemiology & Risk Factors",
    TRUE       ~ paste("Topic", topic)
  ))

# Save table
write_tsv(top_terms_named,
          "data/clean/lda_topic_top_terms_named.tsv")

# Plot faceted top terms
p_topics <- ggplot(top_terms_named,
                   aes(x = reorder_within(term, beta, topic_name),
                       y = beta,
                       fill = topic_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_name, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "LDA topic Model: How topics evolve over years",
    x = "Term",
    y = "Beta (Term Probability Within Topic)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

ggsave("figures/lda_topics_named.png",
       p_topics, width = 10, height = 7, dpi = 300)

# ------------------------------------------------------------
# 2. Topic prevalence over years (With names)
# ------------------------------------------------------------

gamma <- tidy(lda_model, matrix = "gamma")  # document-topic probabilities

gamma_year <- gamma %>%
  left_join(df %>% select(PMID, year),
            by = c("document" = "PMID")) %>%
  filter(!is.na(year))

topic_trends <- gamma_year %>%
  group_by(year, topic) %>%
  summarise(mean_gamma = mean(gamma), .groups = "drop") %>%
  mutate(topic_name = case_when(
    topic == 1 ~ "Clinical assessment",
    topic == 2 ~ "Gaming disorder",
    topic == 3 ~ "Smartphone/Social media use",
    topic == 4 ~ "Mental health effects",
    topic == 5 ~ "Epidemiology & risk factors",
    TRUE       ~ paste("Topic", topic)
  ))

# Save topic trend table
write_tsv(topic_trends,
          "data/clean/lda_topic_trends_named.tsv")

# Plot topic trends over time
p_trends <- ggplot(topic_trends,
                   aes(x = year, y = mean_gamma,
                       color = topic_name,
                       group = topic_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Topic prevalence over time (Named topics)",
    x = "Year",
    y = "Average Topic Probability",
    color = "Topic"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title   = element_text(face = "bold"),
    legend.title = element_blank()
  )

ggsave("figures/lda_topic_trends_named.png",
       p_trends, width = 10, height = 6, dpi = 300)
