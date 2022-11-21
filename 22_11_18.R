
# Presettings -------------------------------------------------------------

library(here)
library(ggsvg)
library(sysfonts)
library(showtext)
library(jpeg)
library(ggimage)
library(rnaturalearth)
library(tidyverse)

sysfonts::font_add_google("Poppins", "text")
sysfonts::font_add(family = "title", regular = "data/quantifier-nbp-font/QuantifierNbp-2d6X.ttf")
showtext_auto()

font_colour <- "#ffffff"



# Data --------------------------------------------------------------------

data <- read_csv2(here("data", "bluedabeede.csv"))
note8 <- paste(readLines(here("data", "note_8.svg")), collapse = "\n") |>
  str_replace("fill:#000000", paste("fill:", font_colour))
note8rev <- paste(readLines(here("data", "note_8_rev.svg")), collapse = "\n") |>
  str_replace("fill:#000000", paste("fill:", font_colour))
note4rev <- paste(readLines(here("data", "note_4_rev.svg")), collapse = "\n") |>
  str_replace("fill:#000000", paste("fill:", font_colour))
note4 <- paste(readLines(here("data", "note_4.svg")), collapse = "\n") |>
  str_replace("fill:#000000", paste("fill:", font_colour))
trebleclef <- paste(readLines(here("data", "trebleclef.svg")), collapse = "\n") |>
  str_replace("fill:#000000", paste("fill:", font_colour))
flatsign <- paste(readLines(here("data", "flatsign.svg")), collapse = "\n") |>
  str_replace_all("fill:#000000", paste("fill:", font_colour))
background <- png::readPNG("data/gradient_background.png")
map_data <- rnaturalearth::ne_countries(continent = "europe", returnclass = "sf") |>
  filter(!name %in% c("Russia")) |>
  sf::st_cast("POLYGON") |>
  mutate(area = sf::st_area(geometry)) |>
  group_by(name) |>
  arrange(-area) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(highlight = if_else(name == "Germany", TRUE, FALSE))

data <- data %>%
  mutate(svg = sample(c(note8rev, note4rev), nrow(.), replace = TRUE)) |>
  mutate(svg = if_else(rank > 50, sample(c(note8, note4), 1), svg)) |>
  mutate(hjust = if_else(rank < 50, 0.29, 0.29)) |>
  mutate(vjust = if_else(rank < 50, 0.9, 0.15))


# Plots -------------------------------------------------------------------

# Inset map ---------------------------------------------------------------

map_plot <- map_data |>
  ggplot(aes(fill = highlight)) +
  geom_sf(colour = font_colour, show.legend = FALSE) +
  scale_fill_manual(values = c("#f1639500", "white")) +
  theme_void()


# Main plot ---------------------------------------------------------------

x_axis_limits <- lubridate::ymd(c("1999-06-20", "2000-02-08"))

plot <- data |>
  ggplot() +
  geom_point_svg(
    aes(date, rank, svg = I(svg), hjust = hjust, vjust = vjust),
    size = 10,
    colour = "green"
  ) +
  geom_point_svg(
    aes(x_axis_limits[1] + lubridate::days(6), 40),
    svg = trebleclef,
    size = 10
  ) +
  geom_point_svg(
    aes(x_axis_limits[1] + lubridate::days(17), 16),
    svg = flatsign,
    size = 10
  ) +
  geom_linerange(
    data = tibble(date = x_axis_limits[1], ymin = 1, ymax = 80),
    aes(x = date, ymin = ymin, ymax = ymax),
    colour = font_colour
  ) +
  geom_linerange(
    data = tibble(date = x_axis_limits[2], ymin = 1, ymax = 80),
    aes(x = date, ymin = ymin, ymax = ymax),
    colour = font_colour
  ) +
  scale_y_reverse(
    breaks = c(1, seq(20, 90, 20)),
    minor_breaks = NULL,
    expand = c(.7, .7)
  ) +
  scale_x_date(
    limits = x_axis_limits,
    date_breaks = "1 month",
    labels = scales::label_date(format = "%b\n%y"),
    expand = c(0, 0)
  ) +
  # scale_y_log10() +
  labs(
    title = "EIFFEL 65 - BLUE",
    subtitle = "Quick rise, slow fall of the song 'Blue (Da Ba Dee)' 
       in the German Top 100 Charts Ranking",
    caption = "Source: offiziellecharts.de | Created by: Max Nölscher | #ggplot2only"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "title", vjust = 7, hjust = 0.5, size = 60, colour = font_colour),
    plot.subtitle = element_text(family = "text", vjust = 7, hjust = 0.5, size = 14, colour = font_colour),
    axis.ticks = element_line(),
    panel.grid.major.y = element_line(colour = font_colour),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(family = "title"),
    axis.text.y = element_text(size = 18, colour = font_colour),
    axis.text.x = element_text(size = 18, colour = font_colour, vjust = 7),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.background = element_rect(fill = "#204996"),
    plot.margin = margin(2, 1, 1, 1, "cm"),
    plot.caption = element_text(colour = font_colour, size = 11, vjust = -6, family = "text")
  )

plot + patchwork::inset_element(map_plot, 0.7, 0.6, 1, 0.98, align_to = "full")
