#!/usr/bin/R

# ------------------------------------------------------------------------------
# title: raster and 3D.
# purpose: 3D surface elevation of the state of Guerrero, México.
# author:
#   - name: Ciro Ramírez-Suástegui
#     affiliation: La Jolla Institute for Immunology
#     email: ksuastegui@gmail.com
# date: 2021-11-11
# ------------------------------------------------------------------------------
# Thanks to:
# https://rspatialdata.github.io/elevation.html
# https://github.com/ivabrunec/30daymapchallenge/blob/main/scripts/day11_3D.R
# Might wanna look at:
# https://rpubs.com/ials2un/elevationfromR

packages_funcs = c(
  "tidyverse",
  "osmdata",
  "tylermorganwall/rayshader",
  "elevatr",
  "raster",
  "rtweet"
)
source(here::here("2021", "code", "installations.R"))
day <- "2021-11-11"
fname <- here::here("2021", "img", day)

# mapdata <- rgeoboundaries::geoboundaries("Mexico")
mapdata <- opq(bbox = "Guerrero, Mexico", timeout = 60) %>%
  add_osm_feature(key = "admin_level", value = "4") %>%
  osmdata_sf() %>%
  .$osm_multipolygons %>%
  dplyr::select(osm_id, name, geometry) %>%
  filter(name %in% "Guerrero")

mape <- elevatr::get_elev_raster(locations = mapdata, z = 9, clip = "locations")
mape_df <- tibble::as_tibble(as.data.frame(mape, xy = TRUE)) %>% drop_na()
colnames(mape_df)[3] <- "elevation"

p <- ggplot() +
  geom_raster(data = mape_df, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = mapdata, fill = NA, color = NA) +
  scale_fill_distiller() +
  labs(fill = NULL,
       title = "State of Guerrero, México",
       caption = paste0("#30DayMapChallenge | Day 10, raster | ",
                        "@cramsuig | Data: OpenStreetMap, AWS Open Data Terrain Tiles")) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = '#ffffff', color = NA),
        plot.background=element_rect(fill = '#ffffff', color = NA),
        plot.caption = element_text(face = "bold", size = 4, hjust = 0.5, color = "gray50"),
        plot.title = element_text(family = "Zapfino", size = 8, hjust = 0.5))

ggplot2::ggsave(
  filename = here::here("2021", "img", "2021-11-10.png"),
  plot = p +
    theme(plot.caption = element_text(face = "bold", size = 12, hjust = 0.5, color = "gray50"),
          plot.title = element_text(family = "Zapfino", size = 27, hjust = 0.5))
)

# https://github.com/ropensci/rtweet/issues/486
rtweet::post_tweet(
  status = paste0(
    "#30DayMapChallenge raster\n\n",
    "code: ", gsub("\\.git", "/", usethis::git_remotes()), "blob/main/2021/code/", day, ".R"
  ),
  media = here::here("2021", "img", "2021-11-10.png"),
  media_alt_text = "Surface elevation of the state of Guerrero, México."
)

rayshader::plot_gg(p, phi = 50, sunangle = 90, theta = 0, zoom = 0.55)
rayshader::render_snapshot(fname)
rayshader::render_highquality(paste0(fname, ".png")) # looks too dark :(

rtweet::post_tweet(
  status = paste0(
    "#30DayMapChallenge 3D\n\n",
    "code: ", gsub("\\.git", "/", usethis::git_remotes()), "blob/main/2021/code/", day, ".R",
    "\n\nDidn't have time to play more with the quality. Maybe another day."
  ),
  media = paste0(fname, ".png"),
  media_alt_text = "3D surface elevation of the state of Guerrero, México."
)
