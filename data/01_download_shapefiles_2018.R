# Scrape the AEC website for shapefiles, then build and export them

# This script downloads the most recent shapefile for each state and combines them.
# Upside: most recent boundaries
# Downside: QLD is wonky because the AEC has included the ocean

library(tidyverse)
library(sf)

# Scrape ------------------------------------------------------------------
# Get file paths
# Set website to download from
url <- "https://www.aec.gov.au/electorates/gis/gis_datadownload.htm"

page <- xml2::read_html(url)

links <- page %>% rvest::html_nodes("a") %>% rvest::html_attr("href")

# Keep only shapefiles
file_list <- links %>%
  str_subset("esri") %>%
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

# Unzip
unzip_dest <- str_replace(file_dest, ".zip$", "")
pwalk(list(zipfile = file_dest, exdir = unzip_dest), unzip)

# Delete zips
walk(file_dest, file.remove)

# Load shapefiles ---------------------------------------------------------
# List of shapefiles
shp_files <- list.files(recursive = T) %>%
  str_subset(".shp$") %>%
  str_subset("SA1s", negate = T) # Needs dev version of stringr

# List of states
states <- shp_files %>%
  str_sub(6, 8) %>%
  str_remove("-") %>%
  str_to_upper()

# Get a list of shapefiles
state_shp_list <- shp_files %>%
  map(read_sf) %>%
  # Name the elements of the list
  set_names(states)

# Wrangle -----------------------------------------------------------------
# Transform each State's CRS so that they are all the same
for (i in seq_along(state_shp_list)) {
  state_shp_list[[i]] <- state_shp_list[[i]] %>% st_transform(3577)
}

# Collapse the list into a normal df
electorates <- sf::st_as_sf(data.table::rbindlist(state_shp_list, idcol = TRUE)) %>%
  rename(state = .id, division = Elect_div)

colnames(electorates) <- colnames(electorates) %>% str_to_lower()

electorates <- electorates %>% select(division, state, numccds, area_sqkm, geometry)

# Export and clean up -----------------------------------------------------
write_sf(electorates, here::here("data", "electorates_2018.gpkg"))

# Delete folders
walk(unzip_dest, unlink, recursive = T)

# Make sure it worked -----------------------------------------------------

test <- read_sf(here::here("data", "electorates_2018.gpkg")) %>% rename(geometry = geom)

p1 <- ggplot(test) +
  geom_sf(size = .02) +
  theme_void() +
  theme_minimal()

ggsave(here::here("static", "last_map.pdf"), p1)
