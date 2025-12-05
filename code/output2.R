library(dplyr)
library(tidytext)
library(ggplot2)
library(readr)
library(stringr)

# ----------------- Load data -----------------
df <- read_tsv("data/processed/pmid_year_title_abstract.tsv",
               show_col_types = FALSE)

data("stop_words")

# ensure year is numeric and valid
df <- df %>%
  filter(year != "" & !is.na(year)) %>%
  mutate(year = as.integer(year))

# ----------------- Words to track -----------------
words_of_interest <- c("gaming", "internet", "smartphone", "addiction")

# ----------------- Tokenize and clean -----------------
df_words <- df %>%
  select(year, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%     # remove stopwords
  filter(!str_detect(word, "\\d")) %>%       # remove digits
  filter(word %in% words_of_interest) %>%    # keep only target words
  count(year, word, sort = TRUE)

# ----------------- Plot trend -----------------
plot_trends <- ggplot(df_words, aes(x = year, y = n, color = word, group = word)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Trends of key terms in article titles over time",
    x = "Year",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank()
  )

# ----------------- Save as PNG -----------------
# THIS LINE MAKES IT PNG (NOT PDF)
ggsave(
  filename = "figures/word_trends.png",
  plot = plot_trends,
  width = 9,
  height = 5,
  dpi = 300
)

cat("PNG image saved to: figures/word_trends.png\n")
