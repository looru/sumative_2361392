#!/usr/bin/env Rscript

library(dplyr)
library(tidytext)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(purrr)

# Checking output directory existence
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

# Loading data
df <- read_tsv("data/clean/pmid_year_title_abstract.tsv",
               show_col_types = FALSE)

data("stop_words")

df <- df %>%
  filter(year != "" & !is.na(year)) %>%
  mutate(year = as.integer(year))

# Words to track
words_of_interest <- c("addiction", "gaming", "internet", "smartphone")

# Tokenizing and counting
df_words <- df %>%
  select(year, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "\\d")) %>%
  filter(word %in% words_of_interest) %>%
  count(year, word)

# Ensuring full year × word grid
df_words_complete <- df_words %>%
  complete(
    year = full_seq(year, 1),
    word = words_of_interest,
    fill = list(n = 0)
  )

# Forecasting until 2030
future_years <- 2026:2030

# Fitting a model per word: n ~ year (Poisson regression works well for counts)
forecast_df <- df_words_complete %>%
  group_by(word) %>%
  do({
    model <- glm(n ~ year, data = ., family = poisson)
    preds <- predict(model,
                     newdata = data.frame(year = future_years),
                     type = "response")
    tibble(year = future_years,
           n = preds,
           predicted = TRUE)
  })

# Combining observed + predicted
df_combined <- df_words_complete %>%
  mutate(predicted = FALSE) %>%
  bind_rows(forecast_df)

# Plotting
plot_trends <- ggplot(df_combined,
                      aes(x = year, y = n, color = word, group = word)) +
  geom_line(data = df_combined %>% filter(!predicted), size = 1.2) +
  geom_line(data = df_combined %>% filter(predicted),
            size = 1.2, linetype = "dashed") +
  geom_point(data = df_combined %>% filter(!predicted), size = 2) +
  labs(
    title = "Observed and predicted (to 2030) trends of key terms in article titles",
    x = "Year",
    y = "Frequency",
    color = "Keyword"
  ) +
  scale_x_continuous(breaks = pretty(c(df_words_complete$year, future_years))) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Saving plot to file
ggsave(
  filename = "figures/word_trends_forecast.png",
  plot = plot_trends,
  width = 10,
  height = 6,
  dpi = 300
)
