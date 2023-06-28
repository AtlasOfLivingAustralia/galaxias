# script to build an example hex sticker

library(hexSticker)
library(ggplot2)

p <- ggplot() + theme_void()

# export as sticker
sticker(
  s_x = 1,
  subplot = p,
  s_y = 1.25,
  s_width = 0.8,
  s_height = 0.8,
  package = "correa",
  p_color = "#29a65d",
  p_y = 1,
  p_family = "mono",
  p_size = 6.5,
  # border
  h_fill = "#ffffff",
  h_color = "#29a65d",
  filename = "man/figures/logo.png",
  # url
  url = "ala.org.au",
  u_family = "mono",
  u_color = "black"
)