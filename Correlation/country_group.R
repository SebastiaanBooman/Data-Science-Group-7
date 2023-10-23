source("Correlation/utils.r")
pacman::p_load(rnaturalearth, stringr, sf)

gen_results <- function(data, out) {
  data <- data %>%
    group_by(year) %>%
    summarize_if(is.numeric, mean)

  gdp <- "cgdpo"
  name <- "GDP"

  return(
    list(
      pop = save_correlation(data, gdp, "pop", name, "Population", out),
      cshmi = save_correlation(data, gdp, "cshm_i", name, "Capital formation", out),
      ck = save_correlation(data, gdp, "ck", name, "Capital services", out),
      avh = save_correlation(data, gdp, "avh", name, "Average hours worked", out),
      ctfp = save_correlation(data, gdp, "ctfp", name, "TFP", out),
      hc = save_correlation(data, gdp, "hc", name, "HCI", out)
    )
  )
}

## Gather country economy status data, geometry data is dropped
ne <- ne_countries(returnclass = "sf") %>%
  st_drop_geometry %>%
  rename("countrycode" = "iso_a3") %>%
  mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
  select(c("countrycode", "economy"))

## Prepare the dataset for further processing
penn <- pwt() %>%
  filter(year >= 1990 & year <= 2019) %>%
  mutate(
    cshm_c = cgdpo * csh_c,
    cshm_i = cgdpo * csh_i,
    cshm_g = cgdpo * csh_g,
    cshm_x = cgdpo * csh_x,
    cshm_m = cgdpo * csh_m,
    cshm_r = cgdpo * csh_r
  )

### Join on countries with desired economy status and generate results
results <- list(
  developed = ne %>%
    filter(economy == "Developed region: G7") %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results("Developed Countries"),
  developing = ne %>%
    filter(economy == "Developing region") %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results("Developing Countries"),

  usa = penn %>% filter(countrycode == "USA") %>% gen_results("USA"),
  canada = penn %>% filter(countrycode == "CAN") %>% gen_results("Canada"),
  china = penn %>% filter(countrycode == "CHN") %>% gen_results("China")
)

## Generate a correlation matrix
independent_vars <- c("cgdpo", "pop", "cshm_i", "ck", "avh", "ctfp", "hc")

m <- matrix(unlist(results), ncol = length(results), byrow = TRUE)
colnames(m) <- names(results)
rownames(m) <- c("pop", "cshmi", "ck", "avh", "cftp", "hc")

# TODO generate fancy matrix with ggplot2
