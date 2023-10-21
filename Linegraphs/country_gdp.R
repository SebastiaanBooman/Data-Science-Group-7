source("Linegraphs/utils.R", chdir = TRUE)
pacman::p_load(ggplot2)

START_YEAR <- "2005"
END_YEAR <- "2019"
COUNTRY <- "Austria"

data <-
  pwt() %>%
  filter(year >= START_YEAR & year <= END_YEAR & COUNTRY == country) %>%
  filter(!is.na(rgdpe)) %>%
  select(c("year", "countrycode","country", "rgdpe"))


ggplot(data=, aes(x=, y=)) +
  geom_line(linetype = "solid")+
  geom_point()