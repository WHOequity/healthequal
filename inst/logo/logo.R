# Create Hex Sticker
library(showtext)
library(hexSticker)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Inconsolata", "incon")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(
  # Subplot (image)
  subplot = "inst/figures/background.png",       # Image name
  s_y = 1,                          # Position of the sub plot (y)
  s_x = 1.01,                       # Position of the sub plot (x)
  s_width = .85,                   # Width of the sub plot
  s_height=0.1,                    # Height of the sub plot
  # Font
  package = "healthequal",            # Package name (will be printed on the sticker)
  p_size = 80,                       # Font size of the text
  p_y = 0.65,                        # Position of the font (y)
  p_x=1.05,                         # Position of the font (x)
  p_family = "incon",               # Defines font
  p_color = "#008dc9",
  p_fontface = "bold",
  # Spotlight
  spotlight = TRUE,                 # Enables spotlight
  l_y=0.6,                          # Position of spotlight (y)
  l_x=1.03,                          # Position of spotlight (x)
  l_width = 10,
  l_height = 3,
  l_alpha = 0.9,
  # Sticker colors
  h_size = 1.2,
  h_fill = "#f5f5f5",               # Color for background
  h_color = "#008dc9",              # Color for borders
  # Resolution
  dpi=1200,                         # Sets DPI
  # Save
  filename="inst/logo/logo.png"               # Sets file name and location where to store the sticker
)
