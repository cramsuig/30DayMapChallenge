#!/usr/bin/R

# ------------------------------------------------------------------------------
# title: Data challenge 2: Natural Earth.
# purpose: Map of the Mexico state, Mexico city, and Morelos with elevation
#   as colour. Lake Texcoco and rivers around are drawn too.
# author:
#   - name: Ciro Ramírez-Suástegui
#     affiliation: La Jolla Institute for Immunology
#     email: ksuasteguic@gmail.com
# date: 2021-11-13
# ------------------------------------------------------------------------------
# https://www.w3.org/Style/Examples/007/fonts.en.html

packages_funcs <- c(
  "tidyverse",
  "sf",
  "osmdata",
  "elevatr",
  "raster",
  "rtweet"
)
source(here::here("2021", "code", "installations.R"))
day <- "2021-11-13"
fname <- here::here("2021", "img", day)

states <- opq(bbox = "Mexico", timeout = 60) %>%
  add_osm_feature(key = "admin_level", value = "4") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  dplyr::select(osm_id, name, geometry) %>%
  filter(name %in% places$states$states) %>% 
  cbind(., st_coordinates(st_centroid(.$geometry)))

map_url = "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_lakes_historic.geojson"
map_lake <- sf::st_read(map_url) %>%
  dplyr::filter(name == "Lake Texcoco") %>%
  dplyr::select(name, geometry)

map_url = "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_10m_rivers_north_america.geojson"
map_rivers <- sf::st_read(map_url) %>%
  sf::st_intersection(states) %>%
  dplyr::select(name, geometry) %>%
  cbind(., st_coordinates(st_centroid(.$geometry)))

mape <- elevatr::get_elev_raster(locations = states, z = 9, clip = "locations")
mape_df <- tibble::as_tibble(as.data.frame(mape, xy = TRUE)) %>% drop_na()
colnames(mape_df)[3] <- "Elevation"

graphics.off()
colours_i <- c("#264D59", "#43978D", "#F9E07F", "#F9AD6A", "#D46C4E")
p <- states %>% ggplot() +
  geom_raster(data = mape_df, aes(x = x, y = y, fill = Elevation)) +
  geom_sf(colour = "grey50", fill = NA) +
  scale_fill_gradientn(colours = colours_i) +
  geom_sf(data = map_lake, color = "#38afcd", fill = "#38afcd", alpha = 0.8) +
  geom_sf(data = map_rivers, color = "#38afcd", size = 1) +
  geom_text(data = map_rivers, aes(X, Y, label = name), size = 5, family = "Luminari", fontface = "bold.italic") +
  geom_text(aes(X, Y-0.1, label = name), size = 4, fontface = "bold") +
  labs(title = "Lake Texcoco in the early 1500s",
       subtitle = "... and rivers around.",
       caption = paste0("#30DayMapChallenge | Day 13, Natural Earth | ",
                        "@cramsuig\nData: Open Street Map, Natural Earth, Terrain Tiles")) +
  theme_void() +
  theme(
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.background = element_rect(color = NA, fill = "grey50"),
    plot.caption = element_text(face = "bold", size = 13, hjust = 1, vjust = 10, color = "gray95"),
    plot.title = element_text(family = "Zapfino", size = 30, hjust = 0.5, vjust = -0.2, color = "white"),
    plot.subtitle = element_text(family = "Zapfino", size = 14, hjust = 1, vjust = -0.2, color = "white")
  )

ggplot2::ggsave(
  filename = paste0(fname, ".png"),
  plot = p, width = unit(10, "cm"), height = unit(10, "cm")
)

rtweet::post_tweet(
  status = paste0(
    "#30DayMapChallenge Natural Earth\n\n",
    "code: ", gsub("\\.git", "/", usethis::git_remotes()), "blob/main/2021/code/", day, ".R"
  ),
  media = paste0(fname, ".png"),
  media_alt_text = paste("Map of the Mexico state, Mexico city, and Morelos",
                          "with elevation as colour. Lake Texcoco and",
                         "rivers around are drawn too.")
)
