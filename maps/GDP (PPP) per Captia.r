## Load and install missing packages
pkgs <- c("readxl", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata")
newpkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newpkgs) > 0)
  install.packages(newpkgs)
lapply(pkgs, library, character.only = TRUE)

target_year <- "2019"

## Import and clean PWT
data <-
  read_xlsx("../pwt1001.xlsx", sheet = "Data") %>%
  filter(year == target_year) %>%
  mutate(value = (rgdpo / pop) / 1000) %>%
  select(c("countrycode", "value"))

## Load and clean world map data
world <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  rename("countrycode" = "iso_a3") %>%
  filter(admin != "Antarctica") %>%
  select(c("countrycode", "geometry"))

mapdata <- merge(world, data, by = "countrycode", all = TRUE)

map <-
  ggplot(mapping = aes(color = "Data unavailable")) +
  geom_sf(data = mapdata, aes(fill = value), linewidth = 0.05) +
  scale_fill_gradient2(
    low = "#FF0000",
    mid = "#00EE00",
    midpoint = 50,
    high = "#008800",
    na.value = "lightgray"
  ) +
  scale_discrete_manual("color", values = "black") +
  guides(color = guide_legend(override.aes = list(colour = "lightgray"))) +
  labs(
    title = paste("GDP (PPP) per capita in", target_year, "(in billion USD)"),
    fill = "",
    color = ""
  ) +
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    panel.background = element_blank(), panel.border     = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text.x      = element_blank(), axis.ticks.x     = element_blank(),
    axis.text.y      = element_blank(), axis.ticks.y     = element_blank()
  )

map