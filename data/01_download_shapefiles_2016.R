# Scrape the AEC website for shapefiles, then build and export them
# 2016 election

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
# Subset to the 2013 national file and 2016 NSW, ACT and WA files
# Why? https://www.aec.gov.au/electorates/gis/files/build-2016-national-dataset.pdf
file_list <- file_list[c(11, 7, 9, 12)]

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
  map(read_sf)

# Wrangle -----------------------------------------------------------------
# Transform each State's CRS so that they are all the same
for (i in seq_along(state_shp_list)) {
  state_shp_list[[i]] <- state_shp_list[[i]] %>% st_transform(3577)
}

# Clean up for merging
# States
for (i in c(1, 3, 4)) {
  state_shp_list[[i]] <- state_shp_list[[i]] %>%
    select(-c(1, 4:7, 9)) %>%
    mutate(state = states[[i]]) %>%
    select(Elect_div, state, Numccds, Area_SqKm, geometry) %>%
    # Drop Z coord (all 0s)
    st_zm()

  colnames(state_shp_list[[i]])[1:4] <- str_to_upper(colnames(state_shp_list[[i]]))[1:4]
}
# National
state_shp_list[[2]] <- state_shp_list[[2]] %>% select(c(1:3, 8, 10)) %>% filter(!(STATE %in% states[c(1, 3, 4)]))

# Collapse the list into a normal df
electorates <- sf::st_as_sf(data.table::rbindlist(state_shp_list)) %>% rename(division = ELECT_DIV)

colnames(electorates) <- colnames(electorates) %>% str_to_lower()

# Export and clean up -----------------------------------------------------
write_sf(electorates, here::here("data", "electorates_2016.gpkg"))

# Delete folders
walk(unzip_dest, unlink, recursive = T)

# Make sure it worked -----------------------------------------------------
test <- read_sf(here::here("data", "electorates_2016.gpkg")) %>% rename(geometry = geom)

p1 <- ggplot(test) +
  geom_sf(size = .02) +
  coord_sf(xlim = c(-1887432, 2121654)) +
  theme_void() +
  theme_minimal()

ggsave(here::here("static", "last_map.pdf"), p1)
