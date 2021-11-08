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
  "cowplot"
)
i <- setdiff(packages_funcs, installed.packages()[, "Package"])
if(length(i)) cat(crayon::red("Not installed:"), i, sep = "\n ")
for (i in packages_funcs){
  cat("*", crayon::yellow(i, "\n"))
  if(!file.exists(i)){
    if (!requireNamespace(i, quietly = TRUE))
      install.packages(i, repos = "https://cloud.r-project.org")
    suppressMessages(require(package = i, quietly = TRUE, character.only = TRUE))
  }else{ source(i) }
}
cat("*", crayon::yellow("spDataLarge", "\n"))
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
if (!requireNamespace("spDataLarge", quietly = TRUE)){
  remotes::install_github("Nowosad/spDataLarge")
}else{ library(spDataLarge) }
