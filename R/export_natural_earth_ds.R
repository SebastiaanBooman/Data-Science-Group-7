##loads and exports relevant natural earth columns (dev status) for further research

source("common.R")
pacman::p_load(stringr, rnaturalearth, dplyr)

OUTPUT_DIR_FN <- "../Data/dev_status.csv"


pwt_data <-
  pwt() %>%
  select(c("countrycode")) %>%
  unique()

natural_earth_world_data <-
  ne_countries(scale = "medium", returnclass = "sf") %>%
  rename("countrycode" = "iso_a3") %>%
  filter(admin != "Antarctica") %>%
  mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
  select(c("countrycode", "economy"))
natural_earth_world_data$geometry <- NULL

relevant_countries <- subset(natural_earth_world_data, (countrycode %in% 
                                             pwt_data$countrycode))

write.csv(relevant_countries, OUTPUT_DIR_FN, row.names = FALSE)