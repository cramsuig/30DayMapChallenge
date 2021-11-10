#!/usr/bin/R

# ------------------------------------------------------------------------------
# title: Monochrome.
# purpose: This script generates maps for the 9th day of the challenge.
#   GIF of the literacy percentages in the continent of Africa.
# author:
#   - name: Ciro Ramírez-Suástegui
#     email: ksuastegui@gmail.com
# date: 2021-11-09
# ------------------------------------------------------------------------------

source(here::here("2021", "code", "installations.R"))
day <- "2021-11-09"

# Get literacy data
url_i <- "https://api.worldbank.org/v2/en/indicator/SE.ADT.LITR.ZS?downloadformat=csv"
url_file <- tempfile(fileext = ".zip")
download.file(url = url_i, destfile = url_file)
utils::unzip(zipfile = url_file, exdir = dirname(url_file))
lit_file = paste0(dirname(url_file), "/API_SE.ADT.LITR.ZS_DS2_en_csv_v2_3158952.csv")
ddf_lit <- suppressMessages(readr::read_csv(lit_file, skip = 3, show_col_types = FALSE))
ddf_lit$...66 <- NULL;
years <- colnames(ddf_lit)[-c(1:4)]# <- paste0("y", colnames(ddf_lit)[-c(1:4)])
for (i in years) ddf_lit[[i]] <- as.double(ddf_lit[[i]])
# Trick to fill missing following years after a measurement
temp <- apply(ddf_lit[, years], MARGIN = 1, function(x)
  tibble(col = x) %>% tidyr::fill(col) )
ddf_lit[, years] <- suppressMessages(t(bind_cols(temp)))

# Matching names and adding data to object
ddf_lit$name_long <- stringr::str_replace_all(
  string = ddf_lit$`Country Name`,
  c("Cote" = "Côte", "Congo, Rep." = "Republic of Congo",
    "Congo, Dem. Rep." = "Democratic Republic of the Congo",
    "Egypt, Arab Rep." = "Egypt", "Gambia, The" = "The Gambia",
    "Eswatini" = "Swaziland")
)
# setdiff(africountries$name_long, ddf_lit$name_long)
# grep("Sahara", ddf_lit$`Country Name`, value = TRUE)
# intersect(ddf_lit$`Country Name`, africountries$name_long)
africountries_lit <- left_join(africountries, ddf_lit, by = "name_long")
africountries_lit_long <- tidyr::pivot_longer(
  africountries_lit, cols = years, names_to = "Year")
africountries_lit_long <- africountries_lit_long[order(africountries_lit_long$Year), ]
 
# tm_shape(africountries_lit) + tm_borders("black", lwd = .5) +
#   tm_shape(afrihighway) + tm_lines(col = "gray95")

# Checked but didn't use:
# # https://stackoverflow.com/questions/68450668/how-can-i-animate-points-on-a-spatial-map-with-gganimate-sf-and-ggplot2
colours_i <- RColorBrewer::brewer.pal(n = 9, name = "Greens")
p <- africountries_lit_long %>% ggplot() +
  geom_sf(aes(fill = `value`)) +
  scale_fill_gradientn(colors = colours_i, n.breaks = 10, na.value = "gray95") +
  theme_void() +
  theme(
    plot.title = element_text(family =  "Zapfino", face = "bold", size = 34,
                              vjust = -0.01, colour = tail(colours_i, 1)),
    plot.subtitle = element_text(family = "Papyrus", size = 23, hjust = 1,
                                 vjust = -0.1, colour = tail(colours_i, 1)),
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5,
                                vjust = 7, color = "gray50")
  ) + # animate! ---------------------------
  gganimate::transition_time(time = as.integer(Year)) +
  # gganimate::enter_recolor(fill = "gray90") +
  labs(title = 'Literacy rates in Africa', subtitle = 'Year: {frame_time}', fill = NULL,
       caption = paste0("#30DayMapChallenge | Day 9, Monochrome | ",
                        "@cramsuig | Data: afrimapr, worldbank.org"))

fname <- here::here("2021", "img", paste0(day, ".gif"))
duration_i <- max(c(10,as.integer(length(unique(africountries_lit_long$Year)) / 3)))
gganimate::anim_save(
  filename = fname, animation = p, #fps = 10,
  duration = duration_i, end_pause = 6,
  width = 700, height = 700
)

rtweet::post_tweet(
  status = paste0(
    "#30DayMapChallenge monochrome\n\n",
    "code: ", gsub("\\.git", "/", usethis::git_remotes()), "2021/code/", day, ".R",
    "\n\nI can finally sleep..."
  ),
  media = fname,
  media_alt_text = "GIF of the literacy percentages in the continent of Africa."
)

# Alternative:
# https://github.com/georgeryang/tidy-tuesday/blob/main/2021/2021-11-09/tt_learning_with_afrilearndata.R
# list.files(path = here::here("2021", "img", day),
#            pattern = '*.png', full.names = TRUE) %>% 
#   image_read() %>%
#   image_join() %>%
#   image_animate(fps=1) %>%
#   image_write(fname)