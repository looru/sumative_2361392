## Applied Health Data Science Summative Assessment  
# Health Data Science Mini Project

This repository contains the code and outputs for my summative assessment on **text mining PubMed articles related to gaming and digital addiction** using **tidyverse**, **tidytext**, and **topic modelling (LDA)**.

---

## Project Overview

The project uses the PubMed E-utilities API to:

1. **Download article metadata and abstracts** for a search on  
   `"gaming disorder" OR "smartphone addiction" OR "internet addiction" OR "social media addiction"`.
2. **Parse and clean the XML** into a tidy tabular format.
3. **Process article titles and abstracts** using tidytext:
   - Tokenisation (one-token-per-row)
   - Stop-word and digit removal
   - Optional stemming
4. **Perform descriptive and advanced text analyses**, including:
   - Trends in key title words over time
   - Comparison word cloud of title words across years
   - **LDA topic modelling applied to abstracts**
   - Temporal trends in topic prevalence over publication years

Key outputs are stored in `data/processed/` and visualisations in `figures/`.

---

## Repository Structure

```text
.
├─ code/
│  ├─ download_raw.sh                 # Bash script: download PMIDs + article XML from PubMed
│  ├─ extract_articles.R              # Parse XML -> TSV (PMID, year, title, abstract)
│  ├─ process_titles.R                # Tidytext processing of titles (tokens, stopwords, digits)
│  ├─ comparison_cloud_year.R         # Comparison cloud + 4-segment word×year table
│  ├─ advanced_lda_topics_over_time.R # LDA topic model + temporal topic trends
│  └─ output1.R, output2.R            # Additional analysis / plotting scripts (if used)
│
├─ data/
│  ├─ raw/                            # Raw XML files downloaded from PubMed
│  └─ processed/                      # Cleaned TSV outputs
│       ├─ pmid_year_title_abstract.tsv
│       ├─ title_tokens_clean.tsv
│       ├─ comparison_cloud_by_year_table.tsv
│       ├─ lda_topic_top_terms.tsv
│       └─ ...
│
├─ figures/
│  ├─ word_trends.png                 # Trends of key title words over time
│  ├─ comparison_cloud_by_year.png    # Comparison cloud of title words by year
│  └─ lda_topics_over_time.png        # LDA topic proportions over time
│
├─ config.yaml                        # Config for Snakemake / environment
├─ env_2361392.yml                    # Conda environment file
├─ Snakefile                          # (Optional) pipeline definition using Snakemake
└─ scripts.R                          # Wrapper to run the full pipeline from R
