# script to build an example hex sticker

library(hexSticker)
library(showtext)
library(ggplot2)
font_add_google("Lato", family = "lato")
p <- ggplot() + theme_void()

# export as sticker
sticker(
  subplot = p,
  # package name
  package = "galaxias",
  p_color = "#c98502",
  p_y = 1,
  p_family = "lato",
  p_size = 9,
  # border
  h_fill = "#ffffff",
  h_color = "#000000",
  h_size = 1.5,
  filename = "man/figures/logo.png",
  # url
  url = "ala.org.au",
  u_family = "lato",
  u_color = "black",
  u_y = 0.12,
  u_size = 2.5
)