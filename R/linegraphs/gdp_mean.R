source("Linegraphs/utils.R")
pacman::p_load(ggplot2)

START_YEAR <- "1990"
END_YEAR <- "2019"

## Import and clean PWT
data <-
  pwt() %>%
  filter(year >= START_YEAR & year <= END_YEAR) %>%
  filter(!is.na(rgdpe)) %>%
  select(c("year", "countrycode", "rgdpe"))

## Calculate the difference in HCI between start_year and end_year
mean_rgdp_data <- data %>%
  group_by(year) %>%
  summarize(mean_rgdpe = mean(rgdpe))

ggplot(data = mean_rgdp_data, aes(x = year, y = mean_rgdpe)) +
  geom_line(linetype = "solid") +
  geom_point()
