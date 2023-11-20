source("Linegraphs/utils.R")
pacman::p_load(stringr, rnaturalearth, ggplot2)

START_YEAR <- 1990
END_YEAR <- 2019
TITLE <- paste("Real GDP per capita fluctuations across the globe (", START_YEAR, "-", END_YEAR, ")")
OUT_FILE_PATH <- "./filename.png"

## Import and clean PWT
data <-
  pwt() %>%
  filter(year >= START_YEAR & year <= END_YEAR) %>%
  filter(!is.na(rgdpe)) %>%
  mutate(rgdpna = (rgdpna / pop)) %>%
  select(c("year", "countrycode", "rgdpna"))

## Load and clean world map data
natural_earth_world_data <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  rename("countrycode" = "iso_a3") %>%
  filter(admin != "Antarctica") %>%
  mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
  select(c("countrycode", "economy"))

merged_data <- merge(data, natural_earth_world_data)

sum_rgdpna_on_eco_year <- merged_data %>%
  group_by(year, economy) %>%
  summarize(sum_gdp = sum(rgdpna))

plot_gdp_region_data <- function(region_df, title_val, line_width= 1){
  ggplot_graph <- ggplot(data=region_df, aes(x=year, y=percentage_change, color=economy)) +
    geom_line() + 
    geom_hline(yintercept=0) +
    geom_vline(xintercept=2008, linetype='dotted') +
    geom_line(linetype = "solid", linewidth = 0.8) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
      title = title_val,
      y = "% change compared to preceding year",
      colour = ""
    )
  return (ggplot_graph)
}

plot_all_regions <- function(){
  merged_df <- sum_rgdpna_on_eco_year %>% 
    group_by(economy) %>%
    mutate(percentage_change = ((sum_gdp - lag(sum_gdp)) / lag(sum_gdp) * 100))
  graph <- plot_gdp_region_data(merged_df, TITLE, 1)
  return(graph)  
}

plot_merged_regions <- function(){
  rgn_dev_g7_data <- subset(sum_rgdpna_on_eco_year, economy=='Developed region: G7')
  rgn_dev_non_g7_data <- subset(sum_rgdpna_on_eco_year, economy=='Developed region: nonG7')
  rgn_emerg_bric <- subset(sum_rgdpna_on_eco_year, economy=='Emerging region: BRIC')
  rgn_emerg_mikt <- subset(sum_rgdpna_on_eco_year, economy=='Emerging region: MIKT')
  rgn_emerg_g20 <- subset(sum_rgdpna_on_eco_year, economy=='Emerging region: G20')
  rgn_dev <- subset(sum_rgdpna_on_eco_year, economy=='Developing region')
  rgn_least_dev <- subset(sum_rgdpna_on_eco_year, economy=='Least developed region')
  #Aggregate the subsets based on different economies, separated in different variables for readability
  rgn_dev_total <- merge(rgn_dev_g7_data, rgn_dev_non_g7_data, by.x = "year", by.y="year") %>%
    mutate(mean_gdp = (sum_gdp.x + sum_gdp.y) / 2) %>%
    mutate(economy = "Developed nations") %>%
    select(c("year", mean_gdp, economy))
  rgn_emerg_total <- merge(rgn_emerg_bric, rgn_emerg_mikt, by.x = "year", by.y="year") %>%
    rename("sum_gdp.z" = "sum_gdp.y") %>%
    merge(rgn_emerg_g20, by.x = "year", by.y = "year") %>%
    mutate(mean_gdp = (sum_gdp.x + sum_gdp.z + sum_gdp) / 3) %>%
    mutate(economy = "Emerging nations") %>%
    select(c("year", mean_gdp, economy))
  rgn_dev <- rgn_dev   %>%
    mutate(economy = "Developing nations") %>%
    rename("mean_gdp" = "sum_gdp")
  rgn_least_dev <- rgn_least_dev  %>%
    mutate(economy = "Least developed nations") %>%
    rename("mean_gdp" = "sum_gdp")
  
  merged_df <- rgn_dev_total %>% 
    rbind(rgn_emerg_total) %>% 
    rbind(rgn_dev) %>% 
    rbind(rgn_least_dev) %>% 
    #Add percentage fluctuation calculations
    group_by(economy) %>%
    mutate(percentage_change = ((mean_gdp - lag(mean_gdp)) / lag(mean_gdp) * 100))
  graph <- plot_gdp_region_data(merged_df, TITLE, 1)
  return(graph)
}

graph <- plot_merged_regions()
#graph <- plot_all_regions()
graph
#ggsave(OUT_FILE_PATH, graph, width=18.5, height=9, units="cm")