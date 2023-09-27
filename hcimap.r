library(readxl)
library(dplyr)
library(ggplot2)
library(rnaturalearth)

## Import and clean PWT
data <-
    read_xlsx('pwt1001.xlsx', sheet = 'Data') %>%
    filter(year == '2017') %>%
    select(c('countrycode', 'hc'))

## Load and clean world map data
world <-
    ne_countries(scale = 'medium', returnclass = 'sf') %>%
    rename('countrycode' = 'iso_a3') %>%
    filter(admin != 'Antarctica') %>%
    select(c('countrycode', 'geometry'))

map <- merge(world, data, by = 'countrycode', all = TRUE)

worldplot <-
    ggplot(map, aes(fill = hc)) +
    geom_sf(color = 'black', linewidth = 0.1) +
    scale_fill_gradientn(colors = rev(terrain.colors(10))) +
    labs(fill = 'Human Captical Index (HCI)') +
    theme_bw() +
    theme(
          legend.position = 'bottom',
          legend.box = 'horizontal',
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
    )
