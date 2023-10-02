## Load and install missing packages
pkgs <- c("readxl", "dplyr", "ggplot2")
newpkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(newpkgs) > 0)
  install.packages(newpkgs)
lapply(pkgs, library, character.only = TRUE)

START_YEAR <- 1990
END_YEAR <- 2019

## Import and clean PWT
data <-
  read_xlsx("pwt1001.xlsx", sheet = "Data") %>%
  filter(year >= START_YEAR & year <= END_YEAR) %>%
  filter(!is.na(rgdpe)) %>%
  select(c("year", "countrycode", "rgdpe", "rgdpo", "rgdpna"))

sum_rgdpe_data <- data %>%
  group_by(year) %>%
  summarize(sum_gdp = sum(rgdpe))

sum_rgdpo_data <- data %>%
  group_by(year) %>%
  summarize(sum_gdp = sum(rgdpo))

sum_rgdpna_data <- data %>%
  group_by(year) %>%
  summarize(sum_gdp = sum(rgdpna))

#Create df containing `years` and `percentage_change`. `percentage_change` contains a float that indicates the % of GDP fluctuation compared to previous year
create_percentage_fluctuation_df <- function(sum_gdp_data){
  percentage_change <- c()
  years <- c()
  for (i in seq(0, nrow(sum_gdp_data) -1,1)){
      old = subset(sum_gdp_data, year == (START_YEAR + i))
      new = subset(sum_gdp_data, year == (START_YEAR + i + 1))
      percentual_change <- (new$sum_gdp - old$sum_gdp) / old$sum_gdp * 100
      percentage_change <- append(percentage_change, percentual_change)
      years <- append(years, new$year)
    }
  return(data.frame(years, percentage_change))
}


rgdpe_dataframe <- create_percentage_fluctuation_df(sum_rgdpe_data)
rgdpo_dataframe <- create_percentage_fluctuation_df(sum_rgdpo_data)
rgdpna_dataframe <- create_percentage_fluctuation_df(sum_rgdpna_data)

ggplot(data=rgdpe_dataframe , aes(x=years, y=percentage_change, color="RGDPE")) +
  geom_line(aes(y = rgdpo_dataframe$percentage_change, color = "RGDPO"), linewidth = 1) + 
  geom_line(aes(y = rgdpna_dataframe$percentage_change, color = "RGDPNA"), linewidth = 1) + 
  geom_line(linetype = "solid", linewidth = 1)+
  theme_bw() +
  labs(
    title = paste("Real GDP fluctuations across the globe between", START_YEAR, "and", END_YEAR),
    y = "% change compared to preceding year",
    colour = "Legend"
  )
