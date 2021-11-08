#!/usr/bin/R

# ------------------------------------------------------------------------------
# title: 7th.
# purpose: This script generates maps for the 07 day of the challange.
# author:
#   - name: Ciro Ramírez-Suástegui
#     email: ksuastegui@gmail.com
# date: 2021-11-07
# ------------------------------------------------------------------------------
# Learnt some bits from:
# https://github.com/BlakeRMills/30DayMapChallenge/blob/main/Day%205%20(Open%20Street%20Maps)/Day%205%20(OpenStreetMap).R
# https://github.com/JoshuaCopping/30DayMapChallenge_2021/blob/main/code/Day_02.R

# format(Sys.time(), "%Y")
source(here::here("2021", "code", "installations.R"))
fname = here::here("2021", "img", "2021-11-07")

## Functions ## ----------------------------------------------------------------
get_places <- function(bounds, values_h = NULL){
  if(is.null(values_h)) values_h <- available_tags("highway")
  data_list <- lapply(X = bounds, FUN = function(x){
    opq(bbox = x) %>% # query
      add_osm_feature(key = "highway",
                      value = values_h) %>%
      osmdata_sf() %>% # get object
      .$osm_lines %>% # access this list
      dplyr::select(osm_id, name, geometry)
  })
  if(length(data_list) > 1) data_list %>% bind_rows else data_list[[1]]
}

## Parameters ## ---------------------------------------------------------------
# I had to explore the data before this... names are not as straightforward
# e. g.: https://www.openstreetmap.org/relation/62149#map=5/55.781/-5.962
places_list <- list(
  list(
    title = "México", country = "MX",
    states = list(bbox = "Guerrero, Mexico", alevel = "4", states = c("Guerrero", "Morelos")),
    cities = c("Las Vigas, Guerrero", "Chilpancingo, Mexico", "Cuernavaca, Morelos")
  ),
  list(
    title = "United Kingdom", country = "GB",
    states = list(bbox = "Devon, United Kingdom", alevel = "6", states = c("Devon")),
    cities = c("Kingsbridge", "Thurlestone", "Wembury")
  ),
  list(
    title = "United States", country = "US",
    states = list(bbox = "California, US", alevel = "4", states = c("California")),
    cities = c("San Diego"), values = c("motorway", "primary", "secondary", "tertiary")
  )
)


# sysfonts::font_files()$ps_name
pp <- list()
pp[["text"]] <-
  cowplot::ggdraw() +
  cowplot::draw_label(label = "Places I call(ed) home", size = 25, vjust = -2,
                      fontfamily = "Zapfino", fontface = "bold") +
  cowplot::draw_label(label = paste0("Circa 2011, when I was 15, I left my grandma's home.\n",
                                     "It has been a wild adventure!"), size = 15,
                      fontfamily = "Papyrus", color = "darkgoldenrod3") +
  cowplot::draw_label(label = paste0("#30DayMapChallenge | Day 7, Green | ",
                                     "Ciro Ramírez-Suástegui | Data: OSM"), size = 10,
                      hjust = 0.5, vjust = 10, color = "gray75")

for (places in places_list) {
  ## Data ## -------------------------------------------------------------------
  country_sf <- if(places$country != "US"){
    spData::world[which(spData::world$iso_a2 %in% places$country), ]
  }else{ spData::us_states }
  cat(crayon::green("Country:"), places$country, "\n")
  country <- tmap::tm_shape(country_sf) + tmap::tm_polygons()
  
  cat(crayon::green("States/counties:"), "\n"); str(places$states)
  states <- opq(bbox = places$states$bbox) %>%
    add_osm_feature(key = "admin_level", value = places$states$alevel) %>%
    osmdata_sf() %>%
    .$osm_multipolygons %>%
    dplyr::select(osm_id, name, geometry) %>%
    filter(name %in% places$states$states)
  
  cat(crayon::green("Cities:"), places$cities, "\n")
  cities <- get_places(places$cities, values_h = places$values)
  
  ## Plot ## -------------------------------------------------------------------
  # ggrastr::rasterise() # used on geom_*
  pp[[places$country]] <- country$tm_shape$shp %>%
    ggplot() +
    geom_sf(color = "darkgoldenrod4", fill = "darkolivegreen", alpha = 0.3) +
    geom_sf(data = states, color = "gold4", fill = "darkolivegreen", alpha = 0.5) +
    geom_sf(data = cities, color="darkolivegreen3") +
    theme_void()
}

# Make it pretty
for (places in places_list) {
  pp[[places$country]] <- pp[[places$country]] +
    labs(title = places$title,
         subtitle = paste(gsub(", Mexico|, Guerrero|, Morelos", "", places$cities), collapse = ", ")) +
    theme(
      plot.title = element_text(family = "Apple Chancery", face = "bold",
                                color = "goldenrod4", size = 20, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_blank()
    )
}

ppg <- cowplot::plot_grid(plotlist = pp[c("text", sapply(places_list, "[[", 2))]) +
  theme(
    plot.background = element_rect(fill = "#0c3018", colour = NA),
    panel.background = element_rect(fill = "#0c3018", colour = NA)
  )
 
ggplot2::ggsave(
  filename = paste0(fname, ".png"),
  plot = ppg,
  width = 10, height = 10
)
