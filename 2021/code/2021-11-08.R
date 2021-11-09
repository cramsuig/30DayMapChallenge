#!/usr/bin/R

# ------------------------------------------------------------------------------
# title: 8th.
# purpose: This script generates maps for the 8th day of the challenge.
#   Map of the continent of America showing lakes and rivers with a blue background.
# author:
#   - name: Ciro Ramírez-Suástegui
#     email: ksuastegui@gmail.com
# date: 2021-11-08
# ------------------------------------------------------------------------------

source(here::here("2021", "code", "installations.R"))

set.seed(27)
matrix_val <- ambient::normalise(ambient::noise_simplex(c(200, 200)))
# p1 <- ~raster::plot(
#   raster(matrix_val), col=RColorBrewer::brewer.pal(n = 9, name = "Blues"),
#   xaxt='n', yaxt='n', ann=FALSE, legend=FALSE
# )
img_file <- tempfile(fileext = ".png")
img <- ggplot(as.data.frame(raster(matrix_val), xy = TRUE, na.rm = TRUE), aes(x = x, y = y)) +
  geom_raster(aes(fill = layer), show.legend = FALSE) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 9, name = "Blues")) +
  theme_void() + theme(panel.spacing=unit(c(-1,-1,-1,-1), "cm"))
ggsave(img, filename = img_file, width = 700, height = 700)

img <- img_file %>%
  image_read() %>%
  image_resize("570x570")

data(rivers, package = "tmap") # data from, https://www.naturalearthdata.com
# 'world' is from spData, https://www.naturalearthdata.com/, https://data.worldbank.org/
lakes <- rnaturalearth::ne_download(
  scale = 110, type = 'lakes', category = 'physical') %>% 
  sf::st_as_sf()
# # This retrives the same data
# lakes_ofile = "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_lakes.zip"
# lakes_file <- paste0(tempdir(), "/lakes.zip")
# system(paste("wget", lakes_ofile, "-O", lakes_file))
# system(paste("unzip", lakes_file))
# lakes0 <- sf::st_read(paste0(dirname(lakes_file), "/ne_110m_lakes.shp"))
# all.equal(lakes, lakes0)
americas <- world[world$continent %in% c("North America", "South America"), ]
americas_rivers <- st_intersection(
  x = rivers,
  y = americas
)
americas_lakes <- st_intersection(
  x = lakes,
  y = americas
)
p <- americas %>% group_by(continent) %>% summarise(.) %>%
  ggplot() +
  geom_sf(fill = "gray90", color = "black", size = 0.2) +
  geom_sf(data = americas_rivers, color = "#0260ad", fill = "#0260ad", size = 1) +
  geom_sf(data = lakes, color = "#0260ad", fill = "#0260ad", size = 1) +
  coord_sf(xlim = st_bbox(americas)[c(1,3)]) +
  theme_void()

ppg <- ggdraw() +
  draw_image(img, scale = 1.3) +
  draw_plot(p) +
  cowplot::draw_label(label = "Americas' rivers\n\nand lakes", size = 21,
                      hjust = 1.2, vjust = 1.8,
                      fontfamily = "Zapfino", fontface = "bold", color = "#000000") +
  cowplot::draw_label(label = paste0("#30DayMapChallenge | Day 8, Blue | ",
                                     "@cramsuig | Data: naturalearthdata.com"), size = 10,
                      hjust = 0.5, vjust = 32)

ggplot2::ggsave(
  filename = here::here("2021", "img", "2021-11-08.png"),
  plot = ppg,
  width = unit(7, "cm"), height = unit(10, "cm")
)
