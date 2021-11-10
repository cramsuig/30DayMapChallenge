#!/usr/bin/R

# ------------------------------------------------------------------------------
# title: Installations.
# purpose: This script was used to install all the necessary packages.
# author:
#   - name: Ciro Ramírez-Suástegui
#     email: ksuastegui@gmail.com
# date: 2021-11-07
# ------------------------------------------------------------------------------

renv::activate(here::here("2021"))
packages_funcs = c(
  "dplyr",
  "ggplot2",
  "osmdata", # 2021-11-07
  "sf",
  "sp",
  "sysfonts",
  "remotes", "raster", "spData", "tmap", "leaflet",
  "ggrastr", # had to download https://www.xquartz.org
  "cowplot",
  "Nowosad/spDataLarge",
  "devtools", # 2021-11-08
  "thomasp85/ambient@v1.0.0",
  "gridGraphics",
  "ggpubr", "magick", "rnaturalearth", "rgdal",
  "afrimapr/afrilearndata", # 2021-11-09
  "afrimapr/afrihealthsites", # updated glue and tibble, not compiled systemfonts
  "ropensci/rtweet", # somehow I had to do it manually again :(
  "gganimate", "transformr"
)
i <- !gsub(".*\\/|@.*", "", packages_funcs) %in% installed.packages()[, "Package"]
if(any(i)) cat(crayon::red("Not installed:"), packages_funcs[i], sep = "\n ")
for (i in packages_funcs){
  cat("*", crayon::yellow(i), "\n")
  i_name <- gsub(".*\\/|@.*", "", i)
  if(!file.exists(i)){
    if (!requireNamespace(i_name, quietly = TRUE)){
      cat(" - installing")
      if(grepl("\\/", i)){
        remotes::install_github(i) # install from github if it's not on CRAN
        # install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
      }else{
        install.packages(i, repos = "https://cloud.r-project.org")
      }
    }
    suppressMessages(require(package = i_name, quietly = TRUE, character.only = TRUE))
  }else{ source(i) }
}
rm(i, i_name, packages_funcs)