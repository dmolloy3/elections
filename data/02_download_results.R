# 2016 Results

library(tidyverse)

# House -------------------------------------------------------------------

# Scrape ------------------------------------------------------------------
# Get file paths
# Set website to download from
url <- "https://results.aec.gov.au/20499/Website/HouseDownloadsMenu-20499-Csv.htm"

page <- xml2::read_html(url)

links <- page %>% rvest::html_nodes(".file-csv") %>% rvest::html_attr("href")

# Choose which files to download
file_list <- links[]

# Create urls
stem <- str_c(dirname(url), "/")

file_urls <- str_c(stem, file_list, sep = "")

# Export ------------------------------------------------------------------

house_2016 <- map(file_urls, read_csv, skip = 1)

for (i in seq_along(house_2016)) {
  house_2016[[i]] <- house_2016[[i]] %>% rename_all(str_to_lower)
}

write_rds(house_2016, here::here("data", "house_2016.rds"))

# Senate ------------------------------------------------------------------

# Scrape ------------------------------------------------------------------
# Get file paths
# Set website to download from
url <- "https://results.aec.gov.au/20499/Website/SenateDownloadsMenu-20499-Csv.htm"

page <- xml2::read_html(url)

links <- page %>% rvest::html_nodes(".file-csv") %>% rvest::html_attr("href")

# Choose which files to download
file_list <- links[]

# Create urls
stem <- str_c(dirname(url), "/")

file_urls <- str_c(stem, file_list, sep = "")

# Export ------------------------------------------------------------------

senate_2016 <- map(file_urls, read_csv, skip = 1)

for (i in seq_along(senate_2016)) {
  senate_2016[[i]] <- senate_2016[[i]] %>% rename_all(str_to_lower)
}

write_rds(senate_2016, here::here("data", "senate_2016.rds"))
