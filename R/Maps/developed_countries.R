#TODO: Potential refactors would be nice, (mainly retrieving nw from a central place so more files can use it)

source("Maps/utils.R")
pacman::p_load(stringr, rnaturalearth, ggplot2)
dev_and_not_dev_map <- function() {
  nw <- ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::rename("countrycode" = "iso_a3") %>%
    filter(admin != "Antarctica") %>%
    mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
    mutate(economy = replace(economy, !startsWith(economy, "Developed region"), "Not developed")) %>%
    mutate(economy = replace(economy, startsWith(economy, "Developed"), "Developed")) %>%
    select(c("countrycode", "economy")) 

  plot <- ggplot(nw, aes(fill = economy, color=economy)) +
    geom_sf( linewidth = 0.3) +
    scale_fill_manual( values =c("#008800", "#FF0000")) +
    scale_color_manual(values =c("white", "white")) +
    theme_bw() +
    theme(
      #legend.position  = "bottom",
      #legend.box       = "horizontal",
      panel.background = element_blank(), panel.border     = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.text.x      = element_blank(), axis.ticks.x     = element_blank(),
      axis.text.y      = element_blank(), axis.ticks.y     = element_blank(),
      plot.title = element_text(size = 18)
    ) #+
    #labs(title="Developed and not developed countries")
  
  
  return(plot)
}

plot <- dev_and_not_dev_map()
#plot
save_plot("./Maps/Dev_vs_non_Dev_map.png", plot, w= 10, h=5)