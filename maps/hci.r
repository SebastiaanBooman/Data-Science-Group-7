source("../utils/install_and_load_pacman.r", chdir = TRUE)
source("../utils/load_pwt.r", chdir = TRUE)
source("../utils/plot_map.r", chdir = TRUE)
pacman::p_load(dplyr)

hcimap <- function(start_year, end_year = start_year) {
  force(end_year)
  if (start_year > end_year)
    stop("start_year must be after end_year")

  if (start_year == end_year) {
    map_title <- paste("Human Captical Index (HCI) in", start_year)
    map_colors <- c("#FF0000", "#00EE00", "#008800")

    ## Filter out one year per country
    data <-
      pwt() %>%
      rename("value" = "hc") %>%
      filter(year == start_year) %>%
      select(c("countrycode", "value"))
  } else {
    map_title <- paste("Grow in Human Captical Index (HCI) between",
                       start_year, "and", end_year, "(in %)")
    map_colors <- c("#FFFFFF", "#0000FF")

    ## Calculate the difference in HCI between start_year and end_year
    data <-
      pwt() %>%
      filter(year == start_year | year == end_year) %>%
      select(c("countrycode", "hc", "year")) %>%
      group_by(countrycode) %>%
      summarize(value = (hc[year == end_year] - hc[year == start_year]) /
                  hc[year == start_year] * 100)
  }

  map <- plot_map(data, "value") +
    scale_fill_gradientn(colors = map_colors, na.value = "gray") +
    labs(title = map_title)

  return(map)
}

#hcimap("1990", "2019")
#hcimap("2019")
