## Load and install missing packages
pkgs <- c("readxl", "dplyr", "ggplot2", "tidyverse")
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

## Load and clean world map data
natural_earth_world_data <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  rename("countrycode" = "iso_a3") %>%
  filter(admin != "Antarctica") %>%
  mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
  select(c("countrycode", "economy"))

merged_data <- merge(data, natural_earth_world_data)

sum_rgdpna_merged_based_on_country_codes <- merged_data %>%
  group_by(year, economy) %>%
  summarize(sum_gdp = sum(rgdpna))


region_developed_g7_data <- subset(sum_rgdpna_merged_based_on_country_codes, economy=='Developed region: G7')
region_developed_non_g7_data <- subset(sum_rgdpna_merged_based_on_country_codes, economy=='Developed region: nonG7')
region_emerging_bric <- subset(sum_rgdpna_merged_based_on_country_codes, economy=='Emerging region: BRIC')
region_emerging_mikt <- subset(sum_rgdpna_merged_based_on_country_codes, economy=='Emerging region: MIKT')
region_emerging_g20 <- subset(sum_rgdpna_merged_based_on_country_codes, economy=='Emerging region: G20')
region_developing <- subset(sum_rgdpna_merged_based_on_country_codes, economy=='Developing region')
region_least_developed <- subset(sum_rgdpna_merged_based_on_country_codes, economy=='Least developed region')

#Create df containing `years` and `percentage_change`. `percentage_change` contains a float that indicates the % of GDP fluctuation compared to previous year
create_percentage_fluctuation_df <- function(sum_gdp_data){
  percentage_change <- c()
  years <- c()
  nrows <- nrow(sum_gdp_data) -1
  for (i in seq(0, nrows,1)){
    old = subset(sum_gdp_data, year == (START_YEAR + i))
    new = subset(sum_gdp_data, year == (START_YEAR + i + 1))
    percentual_change <- (new$sum_gdp - old$sum_gdp) / old$sum_gdp * 100
    percentage_change <- append(percentage_change, percentual_change)
    years <- append(years, new$year)
  }
  return(data.frame(sum_gdp_data$economy[1:nrows], years, percentage_change) %>% rename("economy" = "sum_gdp_data.economy.1.nrows."))
}

rgdpna_developed_g7_df <- create_percentage_fluctuation_df(region_developed_g7_data)
rgdpna_developed_non_g7_df <- create_percentage_fluctuation_df(region_developed_non_g7_data)
rgdpna_emerging_bric_df <- create_percentage_fluctuation_df(region_emerging_bric)
rgdpna_emerging_mikt_df <- create_percentage_fluctuation_df(region_emerging_mikt)
rgdpna_emerging_g20_df <- create_percentage_fluctuation_df(region_emerging_g20)
rgdpna_developing_df <- create_percentage_fluctuation_df(region_developing)
rgdpna_least_developed_df <- create_percentage_fluctuation_df(region_least_developed)

ggplot_graph <- ggplot(data=rgdpna_developed_g7_df , aes(x=years, y=percentage_change, color=economy[1])) +
  geom_line(aes(y = rgdpna_developed_non_g7_df$percentage_change, color =  rgdpna_developed_non_g7_df$economy[1]), linewidth = 1) + 
  geom_line(aes(y = rgdpna_emerging_bric_df$percentage_change, color = rgdpna_emerging_bric_df$economy[1]), linewidth = 1) + 
  geom_line(aes(y = rgdpna_emerging_mikt_df$percentage_change, color = rgdpna_emerging_mikt_df$economy[1]), linewidth = 1) + 
  geom_line(aes(y = rgdpna_emerging_g20_df$percentage_change, color = rgdpna_emerging_g20_df$economy[1]), linewidth = 1) + 
  geom_line(aes(y = rgdpna_developing_df$percentage_change, color = rgdpna_developing_df$economy[1]), linewidth = 1) + 
  geom_line(aes(y = rgdpna_least_developed_df$percentage_change, color = rgdpna_least_developed_df$economy[1]), linewidth = 1) + 
  geom_line(linetype = "solid", linewidth = 0.8)+
  geom_hline(yintercept=0) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = paste("Real GDP fluctuations across the globe (", START_YEAR, "-", END_YEAR, ")"),
    y = "% change compared to preceding year",
    colour = ""
  )
ggplot_graph