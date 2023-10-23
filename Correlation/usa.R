source("./Correlation/utils.r", chdir = TRUE)
#pacman::p_load(rnaturalearth, rnaturalearthdata)

#START_YEAR <- 1990
#END_YEAR <- 2019

### Import and clean PWT
#pwt <- pwt() %>%
#  filter(year >= START_YEAR & year <= END_YEAR) %>%
#  filter(!is.na(rgdpna) & !is.na(pop) & !is.na(avh) & !is.na(emp)) %>%
#  mutate(rgdpna = (rgdpna / pop)) %>%
#  mutate(avh_by_pop = (avh * emp) / pop)
#
#nw <- ne_countries(scale = "medium", returnclass = "sf") %>%
#  rename("countrycode" = "iso_a3") %>%
#  filter(admin != "Antarctica") %>%
#  mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
#  select(c("countrycode", "economy"))
#
#joined_data <- merge(nw, pwt)

data <- pwt() %>%
  #filter(year == 2019) %>%
  #select(c("cgdpo", )) %>%
  #group_by(countrycode) %>%
  #summarize(cgdpo = mean(cgdpo)) %>%
  filter(countrycode == "USA") %>%
  #summarize(cgdpo = mean(cgdpo)) %>%
  #group_by(year) %>%
  mutate(
    cshm_c = cgdpo * csh_c,
    cshm_i = cgdpo * csh_i,
    cshm_g = cgdpo * csh_g,
    cshm_x = cgdpo * csh_x,
    cshm_m = cgdpo * csh_m,
    cshm_r = cgdpo * csh_r
  )

save_correlation_results(data, "cgdpo", "pop", "GDP", "Population")
save_correlation_results(data, "cgdpo", "avh", "GDP", "Average hours worked")
#save_correlation_results(data, "cgdpo", "ctfp", "GDP", "TFP")
save_correlation_results(data, "cgdpo", "hc", "GDP", "HCI")
