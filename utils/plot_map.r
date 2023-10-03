plot_map <- function(data, value) {
  pacman::p_load(dplyr)
  pacman::p_load(ggplot2)
  pacman::p_load(rnaturalearth)

  ## Load and clean world map data
  world <-
    rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    rename("countrycode" = "iso_a3") %>%
    filter(.data[["admin"]] != "Antarctica") %>%
    select(c("countrycode", "geometry"))

  ## Join geometry data and fill data
  map <- merge(world, data, by = "countrycode", all = TRUE)

  return(ggplot() +
    geom_sf(data = map,
            mapping = aes(fill = value, color = "No Data"),
            linewidth = 0.05) +
    scale_discrete_manual("color", values = "black") +
    guides(color = guide_legend(override.aes = list(fill = "gray"))) +
    labs(fill = "", color = "") +
    theme_bw() +
    theme(
      legend.position  = "bottom",
      legend.box       = "horizontal",
      panel.background = element_blank(), panel.border     = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.text.x      = element_blank(), axis.ticks.x     = element_blank(),
      axis.text.y      = element_blank(), axis.ticks.y     = element_blank()
    ))
}
