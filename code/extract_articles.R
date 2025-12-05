#!/usr/bin/env Rscript

library(xml2)
library(dplyr)
library(purrr)
library(stringr)
library(readr)


raw_dir  <- "data/raw"
out_dir  <- "data/clean"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, "pmid_year_title_abstract.tsv")

get_year <- function(article_node) {

 
  year_node <- xml_find_first(
    article_node,
    ".//Article/Journal/JournalIssue/PubDate/Year"
  )
  year <- xml_text(year_node)

  if (!is.na(year) && str_detect(year, "^[0-9]{4}$")) return(year)


  medline_node <- xml_find_first(
    article_node,
    ".//Article/Journal/JournalIssue/PubDate/MedlineDate"
  )
  medline_text <- xml_text(medline_node)
  year2 <- str_extract(medline_text, "\\b(19|20)[0-9]{2}\\b")

  ifelse(is.na(year2), "", year2)
}

parse_pubmed_file <- function(path) {

  doc <- read_xml(path)

  
  articles <- xml_find_all(doc, ".//PubmedArticle")
  if (length(articles) == 0L) articles <- list(doc)

  map_dfr(articles, function(a) {

   
    pmid <- xml_text(xml_find_first(a, ".//MedlineCitation/PMID"))
    if (is.na(pmid) || pmid == "")
      pmid <- xml_text(xml_find_first(a, ".//PMID"))

   
    title_raw <- xml_text(xml_find_first(a, ".//Article/ArticleTitle"), trim = TRUE)

    title <- title_raw %>%
      str_replace_all("<[^>]+>", "") %>% 
      str_squish()

    
    year <- get_year(a)

    
    abstract <- xml_find_all(a, ".//Abstract/AbstractText") %>%
      xml_text(trim = TRUE) %>%
      paste(collapse = " ") %>%
      str_squish()

    tibble(
      PMID     = pmid     %||% "",
      year     = year     %||% "",
      title    = title    %||% "",
      abstract = abstract %||% ""
    )
  })
}


`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x


safe_parse_pubmed_file <- function(path) {
  tryCatch(
    parse_pubmed_file(path),
    error = function(e) {
      message("Skipping file due to parse error: ", path)
      tibble(
        PMID     = character(),
        year     = character(),
        title    = character(),
        abstract = character()
      )
    }
  )
}



xml_files <- list.files(
  raw_dir,
  pattern = "^article-data-.*\\.xml$",
  full.names = TRUE
)

cat("Found", length(xml_files), "XML files\n")

if (length(xml_files) == 0L) {
  stop("ERROR: No XML files found in ", raw_dir,
       " matching pattern 'article-data-*.xml'")
}


articles <- map_dfr(xml_files, safe_parse_pubmed_file)


articles_clean <- articles %>%
  filter(title != "") %>%             
  distinct(PMID, .keep_all = TRUE)  

cat("After cleaning, kept", nrow(articles_clean), "articles\n")


write_tsv(articles_clean, out_file)
