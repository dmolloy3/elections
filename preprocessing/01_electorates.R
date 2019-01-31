# Preprocessing Results

library(tidyverse)
library(sf)

# Load --------------------------------------------------------------------
house_2016 <- read_rds(here::here("data", "house_2016.rds"))

shapefile_2016 <- read_sf(here::here("data", "electorates_2016.gpkg")) %>% rename(geometry = geom)

# Clean -------------------------------------------------------------------

electorates <- house_2016[[2]] %>%
  left_join(house_2016[[7]]) %>%
  select(
    division_id = divisionid,
    division = divisionnm,
    state = stateab,
    candidate_id = candidateid,
    given_name = givennm,
    surname,
    party = partynm,
    party_ab = partyab,
    previous_party = previouspartynm,
    previous_party_ab = previouspartyab
  ) %>%
  # Adjust names to match shapefile
  mutate(division = case_when(
    division == "McMillan" ~ "Mcmillan",
    division == "McPherson" ~ "Mcpherson",
    TRUE ~ division
  ))

map_2016_df <- shapefile_2016 %>% left_join(electorates)


# Test --------------------------------------------------------------------

p1 <- ggplot(map_2016_df, aes(fill = party_ab)) +
  geom_sf(size = .02) +
  coord_sf(xlim = c(-1887432, 2121654)) +
  theme_void() +
  theme_minimal()

ggsave(here::here("static", "last_map.pdf"), p1)

mapview::mapview(map_2016_df, zcol = "party_ab", legend = TRUE)
