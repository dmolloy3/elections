# Scrape the AEC website for shapefiles

library(tidyverse)

# Get file paths ----------------------------------------------------------
# Set website to download from
url <- "https://www.aec.gov.au/electorates/gis/gis_datadownload.htm"

page <- xml2::read_html(url)

links <- page %>% rvest::html_nodes("a") %>% rvest::html_attr("href")

# Keep only shapefiles
file_list <- links %>% str_subset("esri") %>% 
  # Fix WA link
  str_replace("^/Electorates/gis/", "")
# Subset to the most current versions
file_list <- file_list[c(1:7, 9)]

# Create urls
stem <- str_c(dirname(url), "/")

file_urls <- str_c(stem, file_list, sep = "")

file_dest <- str_c("data/", basename(file_list))


# Download ----------------------------------------------------------------
download.file(
  url = file_urls,
  destfile = file_dest, 
  cacheOK = FALSE
)

# Unzip -------------------------------------------------------------------
unzip_dest <- str_replace(file_dest, ".zip$", "")
# walk(unzip_dest, unlink)
pwalk(list(zipfile = file_dest, exdir = unzip_dest), unzip)

# Delete zips -------------------------------------------------------------
walk(file_dest, file.remove)

# Zip whole thing ---------------------------------------------------------
zip(here::here("data", "shapefiles.zip"), unzip_dest)

# Delete folders
walk(unzip_dest, unlink, recursive = T) 
