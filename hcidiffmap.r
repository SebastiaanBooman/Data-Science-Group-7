## Load and install missing packages
pkgs <- c("readxl", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata")
newpkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newpkgs) > 0)
  install.packages(newpkgs)
lapply(pkgs, library, character.only = TRUE)

start_year <- "1990"
end_year <- "2019"

## Import and clean PWT
data <-
  read_xlsx("pwt1001.xlsx", sheet = "Data") %>%
  #filter(year == start_year | year == end_year) %>%
  filter(!is.na(hc)) %>%
  select(c("year", "countrycode", "hc"))

## Calculate the difference in HCI between start_year and end_year
diffdata <- data %>%
  group_by(countrycode) %>%
  summarize(hcdiff = (hc[year == end_year] - hc[year == start_year]) /
              hc[year == start_year] * 100)

## Load and clean world map data
world <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  rename("countrycode" = "iso_a3") %>%
  filter(admin != "Antarctica") %>%
  select(c("countrycode", "geometry"))

map <- merge(world, diffdata, by = "countrycode", all = TRUE)

df <- data.frame(x = c(0), y = c(0), a = 0)

hcidiffmap <-
  ggplot() +
  geom_sf(data = map, aes(fill = hcdiff), color = "white", linewidth = 0.1) +
  scale_fill_gradientn(colors = terrain.colors(10, rev = TRUE),
                       limits = c(-100, NA)) +
  geom_point(data = df, aes(x = x, y = y, color = "Data unavailable"),
             alpha = 0) +
  theme_bw() +
  labs(
    title = paste("Human Captial Index change (HCI) between", start_year,
                  "and", end_year),
    fill = "HCI",
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
