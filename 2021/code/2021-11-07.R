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
renv::activate()
fname = here::here(format(Sys.time(), "%Y"), "img", "2021-11-07.png")



ggplot2::ggsave(fname, fname, height = 12, width = 12)