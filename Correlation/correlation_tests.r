source("Correlation/utils.r")
pacman::p_load(stringr, rnaturalearth, rnaturalearthdata)

START_YEAR <- 1990
END_YEAR <- 2019

## Import and clean PWT
pwt <- pwt() %>%
  filter(year >= START_YEAR & year <= END_YEAR) %>%
  filter(!is.na(rgdpna) & !is.na(pop) & !is.na(avh) & !is.na(emp)) %>%
  mutate(rgdpna = (rgdpna / pop)) %>%
  mutate(avh_by_pop = (avh * emp) / pop)

nw <- ne_countries(scale = "medium", returnclass = "sf") %>%
  rename("countrycode" = "iso_a3") %>%
  filter(admin != "Antarctica") %>%
  mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
  select(c("countrycode", "economy"))

joined_data <- merge(nw, pwt)

save_correlation_results(joined_data, "rgdpna", "avh_by_pop", "Example")
