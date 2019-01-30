# Load AEC shapefiles

library(tidyverse)
library(sf)

# Read --------------------------------------------------------------------
unzip(here::here("data", "shapefiles.zip"))
# List of shapefiles
shp_files <- list.files(recursive = T) %>% 
  str_subset(".shp$") %>% 
  str_subset("SA1s", negate = T) # Needs dev version of stringr

# List of states
states <- shp_files %>% str_sub(6, 8) %>% 
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
  rename(state = .id)


  
p1 <- ggplot(electorates) + 
  geom_sf() + 
  theme_void() + 
  theme_minimal()

ggsave(here::here("static", "last_map.pdf"), p1)


# To do
# Go back and do Nat data as they want me to
# Prettify map

