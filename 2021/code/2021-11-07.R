#!/usr/bin/R

# ------------------------------------------------------------------------------
# title: 7th.
# purpose: This script generates maps for the 07 day of the challange.
# author:
#   - name: Ciro Ramírez-Suástegui
#     email: ksuastegui@gmail.com
# date: 2021-11-07
# ------------------------------------------------------------------------------

# Environment and place
# format(Sys.time(), "%Y")
renv::activate(here::here("2021"))
fname = here::here("2021", "img", "2021-11-07.png")



ggplot2::ggsave(fname, height = 12, width = 12)