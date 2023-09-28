## Load and install missing packages
pkgs <- c("readxl", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata")
newpkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newpkgs) > 0)
  install.packages(newpkgs)
lapply(pkgs, library, character.only = TRUE)

target_year <- "2019"

## Import and clean PWT
data <-
  read_xlsx("pwt1001.xlsx", sheet = "Data") %>%
  filter(year == target_year) %>%
  select(c("countrycode", "hc"))

## Load and clean world map data
world <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  rename("countrycode" = "iso_a3") %>%
  filter(admin != "Antarctica") %>%
  select(c("countrycode", "geometry"))

map <- merge(world, data, by = "countrycode", all = TRUE)

hcimap <-
  ggplot() +
  geom_sf(data = map, aes(fill = hc, color = ""), linewidth = 0.1) +
  scale_fill_gradientn(
    colors = terrain.colors(10, rev = TRUE),
    na.value = "lightgray"
  ) +
  scale_color_manual(values = "white") +
  theme_bw() +
  labs(
    title = paste("Human Captial Index (HCI) in", target_year),
    fill = "HCI",
    color = "Data unavailable"
  ) +
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    panel.border     = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank()
  )
