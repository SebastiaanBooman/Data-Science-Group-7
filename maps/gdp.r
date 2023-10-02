source("../utils/install_and_load_pacman.r", chdir = TRUE)
source("../utils/load_pwt.r", chdir = TRUE)
source("../utils/plot_map.r", chdir = TRUE)
pacman::p_load(dplyr)

gdpmap <- function(start_year, end_year = start_year, mean = FALSE) {
  force(end_year)
  if (start_year > end_year)
    stop("start_year must be after end_year")

  ## Use rGDPna as input value
  data <- pwt() %>% rename("gdp" = "rgdpna")

  if (start_year == end_year) {
    if (mean == TRUE)
      stop("start_year and end_year must be different when calculating mean")

    map_title <- paste("GDP per captia in", start_year, "(in billion USD)")
    map_colors <- c("#FF0000", "#00EE00", "#008800")
    map_limits <- c()

    ## Filter out one year per country
    data <-
      data %>%
      filter(year == start_year) %>%
      mutate(value = (gdp / pop) / 1000) %>%
      select(c("countrycode", "value"))
  } else if (mean == TRUE) {
    map_title <- paste("Mean GDP per captia between",
                       start_year, "and", end_year, "(in billion USD)")
    map_colors <- c("#FF0000", "#00EE00", "#008800")
    map_limits <- c()

    ## Calculate the difference in GDP between start_year and end_year
    data <-
      data %>%
      filter(year >= start_year & year <= end_year) %>%
      mutate(value = (gdp / pop) / 1000) %>%
      select(c("countrycode", "value", "year")) %>%
      group_by(countrycode) %>%
      summarize(value = mean(value))
  } else {
    map_title <- paste("Grow in GDP per captia between",
                       start_year, "and", end_year, "(in %)")
    map_colors <- c("#FF0000", "#FFFFFF", "#0000FF")
    map_limits <- c(-100, 100)

    ## Calculate the difference in GDP between start_year and end_year
    data <-
      data %>%
      filter(year == start_year | year == end_year) %>%
      mutate(gdp = (gdp / pop) / 1000) %>%
      select(c("countrycode", "gdp", "year")) %>%
      group_by(countrycode) %>%
      summarize(value = (gdp[year == end_year] - gdp[year == start_year]) /
                  gdp[year == start_year] * 100)
  }

  map <- plot_map(data, "value") +
    scale_fill_gradientn(
      colors = map_colors,
      limits = map_limits,
      oob = scales::squish,
      na.value = "gray"
    ) +
    labs(title = map_title)

  return(map)
}

#gdpmap("1990", "2019", FALSE)
#gdpmap("1990", "2019", TRUE)
#gdpmap("2019")
