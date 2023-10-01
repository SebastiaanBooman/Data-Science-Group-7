## Load and install missing packages
pkgs <- c("readxl", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata")
newpkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newpkgs) > 0)
  install.packages(newpkgs)
lapply(pkgs, library, character.only = TRUE)

START_YEAR <- "1990"
END_YEAR <- "2019"

## Import and clean PWT
data <-
  read_xlsx("pwt1001.xlsx", sheet = "Data") %>%
  filter(year >= START_YEAR & year <= END_YEAR) %>%
  filter(!is.na(rgdpe)) %>%
  select(c("year", "countrycode", "rgdpe"))

## Calculate the difference in HCI between start_year and end_year
mean_rgdp_data <- data %>%
  group_by(year) %>%
  summarize(mean_rgdpe = mean(rgdpe))

ggplot(data=mean_rgdp_data, aes(x=year, y=mean_rgdpe)) +
  geom_line(linetype = "solid")+
  geom_point()