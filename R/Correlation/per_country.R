source("Correlation/utils.R")

#countryname <- "USA"
countryname <- "CAN"
#countryname <- "JPY"

data <- pwt() %>%
  filter(
    countrycode == countryname,
    year >= 1990 & year <= 2019
  ) %>%
  mutate(
    cshm_c = cgdpo * csh_c,
    cshm_i = cgdpo * csh_i,
    cshm_g = cgdpo * csh_g,
    cshm_x = cgdpo * csh_x,
    cshm_m = cgdpo * csh_m,
    cshm_r = cgdpo * csh_r
  )

save_correlation_results(data, "cgdpo", "pop", "GDP", "Population", countryname)
save_correlation_results(data, "cgdpo", "cshm_i", "GDP", "Capital formation",
                         countryname)
save_correlation_results(data, "cgdpo", "avh", "GDP", "Average hours worked",
                         countryname)
save_correlation_results(data, "cgdpo", "ctfp", "GDP", "TFP", countryname)
save_correlation_results(data, "cgdpo", "hc", "GDP", "HCI", countryname)
