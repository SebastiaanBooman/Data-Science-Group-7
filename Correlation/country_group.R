source("Correlation/utils.r")
pacman::p_load(rnaturalearth, stringr, sf, reshape)

SAVE_COR_RES <- FALSE

gen_results <- function(data, out) {
  data <- data %>%
    group_by(year) %>%
    summarize_if(is.numeric, mean)

  gdp <- "cgdpo"
  name <- "GDP"

  return(
    list(
      "Population" = correlate(data, gdp, "pop", name, "Population", out, SAVE_COR_RES),
      "Capital formation" = correlate(data, gdp, "cshm_i", name, "Capital formation", out, SAVE_COR_RES),
      "capital services" = correlate(data, gdp, "ck", name, "Capital services", out, SAVE_COR_RES),
      "Average hours worked" = correlate(data, gdp, "avh", name, "Average hours worked", out, SAVE_COR_RES),
      "TFP" = correlate(data, gdp, "ctfp", name, "TFP", out, SAVE_COR_RES),
      "HCI" = correlate(data, gdp, "hc", name, "HCI", out, SAVE_COR_RES)
    )
  )
}

## Gather country economy status data, geometry data is dropped
ne <- ne_countries(returnclass = "sf") %>%
  st_drop_geometry %>%
  dplyr::rename("countrycode" = "iso_a3") %>%
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
  "Developed countries" = ne %>%
    filter(economy == "Developed region: G7") %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results("Developed Countries"),
  "Dveloping countries" = ne %>%
    filter(economy == "Developing region") %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results("Developing Countries"),

  USA = penn %>% filter(countrycode == "USA") %>% gen_results("USA"),
  Canada = penn %>% filter(countrycode == "CAN") %>% gen_results("Canada"),
  China = penn %>% filter(countrycode == "CHN") %>% gen_results("China")
)

## Generate a correlation matrix
independent_vars <- c("cgdpo", "pop", "cshm_i", "ck", "avh", "ctfp", "hc")

m <- matrix(unlist(results), ncol = length(results), byrow = TRUE)
colnames(m) <- names(results)
rownames(m) <- names(results[[1]])

# TODO generate fancy matrix with ggplot2

#set.seed(8)
#ma <- matrix(round(rnorm(200), 2), 10, 10)
#colnames(ma) <- paste("Col", 1:10)
#rownames(ma) <- paste("Row", 1:10)

# Transform the matrix in long format
df <- melt(m)
colnames(df) <- c("x", "y", "value")

ggplot(df, aes(x = , x, y = y, fill = value)) +
  labs(
    title = "Correlation matrix",
    #subtitle = subtitle,
    x = "variable",
    y = "RGDPo"
  ) +
  geom_tile() +
  geom_text(aes(x,y, label=value), color = ifelse(df$value > 0.5, "white", "black"), size = 5) +
  scale_fill_distiller(palette = 2, direction=1)