# This is someone else's work - not my code

# # Load packages

library(tibble)
library(tidyr)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)


# Is it legal to scrape from the website?

robotstxt::paths_allowed("https://forums.tesla.com/") # Yes
robotstxt::paths_allowed("https://hotcopper.com.au/discussions/asx---by-stock/")

# Custom functions for scraping the data

scrape_page_info <- function(page_url){
  
  html <- read_html(page_url)
  
  topics <- html %>%
    html_nodes(".title a") %>%
    html_text()
  
  topic_urls <- html %>%
    html_nodes(".title a") %>%
    html_attr(name = "href") %>%
    paste0("https://forums.tesla.com", .)
  
  created_info <- html %>%
    html_nodes(".created") %>%
    html_text() %>%
    str_squish()
  
  tibble(topics, created_info, topic_urls) %>%
    separate(col = created_info, into = c("date_of_creation", "thread_author"), sep = " by ")
  
}

scrape_thread_info <- function(thread_html){
  
  thread_html %>%
    html_nodes(".clearfix") %>%
    html_text() %>%
    str_squish() %>%
    str_replace(pattern = "^(.*?(\\|.*?){1})\\|", replacement = "\\1") %>%  # Remove second "|"
    str_replace(pattern = "(^.*?\\d{4})", replacement = "\\1 \\|") %>% # Add "|" after first date
    enframe(name = NULL, value = "content") %>%
    separate(col = "content", into = c("author", "date", "content"), sep = "\\|")
  
}


scrape_thread <- function(url){
  
  html <- read_html(url)
  
  n_pages <- html %>%
    html_node("#article_content > div.panel-pane.pane-node-comments > div > div.item-list > ul > li.pager-last.last > a") %>%
    html_attr("href") %>%
    str_extract(pattern = "(\\d+)$") %>%
    as.numeric()
  
  df_page_1 <-
    html %>%
    html_nodes(".clearfix") %>%
    html_text() %>%
    str_squish() %>%
    str_replace(pattern = "^(.*?(\\|.*?){1})\\|", replacement = "\\1") %>%  # Remove second "|"
    str_replace(pattern = "(^.*?\\d{4})", replacement = "\\1 \\|") %>% # Add "|" after first date
    enframe(name = NULL, value = "content") %>%
    separate(col = "content", into = c("author", "date", "content"), sep = "\\|")
  
  df_page_1$author[1] <- html %>%
    html_node(".username") %>%
    html_text()
  
  df_page_1$date[1] <- html %>%
    html_node(".submitted") %>%
    html_text() %>%
    str_squish() %>%
    str_extract(pattern = "\\w+\\s\\d+\\W\\s\\d{4}$")
  
  df_page_1$content[1] <- html %>%
    html_node(".clearfix") %>%
    html_text() %>%
    str_squish() %>%
    str_replace(pattern = "^.*\\d+\\,\\s\\d{4}\\s", replacement = "")
  
  extra_page_data <- NULL
  
  if(!is.na(n_pages)){
    extra_urls <- paste0(thread_url, "?page=", seq_len(n_pages-1))
    other_htmls <- lapply(extra_urls, function(x) read_html(x))
    df <- lapply(other_htmls, function(x){
      scrape_thread_info(x)[-1, ]
    })
    extra_page_data <- do.call(rbind, df)
  } 
  
  rbind(df_page_1, extra_page_data)
  
}

scrape_thread_possibly <- possibly(scrape_thread, otherwise = NA)


# Actual scraping

page_urls <- c("https://forums.tesla.com/forum/tesla-model-3", paste0("https://forums.tesla.com/forum/tesla-model-3?page=", 1:2))

master_data <- map_dfr(page_urls[1:2], function(url){
  scrape_page_info(url) %>%
    mutate(forum_data = map(topic_urls, scrape_thread_possibly))
})
