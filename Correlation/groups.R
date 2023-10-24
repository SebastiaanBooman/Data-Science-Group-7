source("Correlation/utils.r")
pacman::p_load(rnaturalearth, stringr, sf, reshape)

vars <- list(
  pop    = "Population",
  avh    = "Average hours worked",
  cshm_i = "Capital formation",
  ck     = "Capital services",
  hc     = "HCI",
  ctfp   = "TFP"
)

gen_results <- function(data, out) {
  data <- data %>%
    group_by(year) %>%
    summarize_if(is.numeric, mean)

  ## Map every variable in vars and correlate against GDP,
  ## the results are stored in a list.
  results <- mapply(function(var, name) {
    correlate(data, "cgdpo", var, "GDP", name, out, FALSE)
  }, names(vars), vars, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  names(results) <- unlist(vars)

  return(results)
}

## Gather country economy status data, geometry data is dropped
ne <- ne_countries(returnclass = "sf") %>%
  st_drop_geometry %>%
  dplyr::rename("countrycode" = "iso_a3") %>%
  mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
  select(c("countrycode", "economy"))

## Prepare the dataset for further processing
penn <- pwt() %>%
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
  "Developing countries" = ne %>%
    filter(economy == "Developing region") %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results("Developing Countries"),

  USA = penn %>% filter(countrycode == "USA") %>% gen_results("USA"),
  Canada = penn %>% filter(countrycode == "CAN") %>% gen_results("Canada"),
  China = penn %>% filter(countrycode == "CHN") %>% gen_results("China"),
  Russia = penn %>% filter(countrycode == "RUS") %>% gen_results("Russia")
)

## Generate a correlation matrix
m <- matrix(unlist(results), ncol = length(results), byrow = TRUE)
colnames(m) <- names(results)
rownames(m) <- names(results[[1]])

## Transform the matrix into a "long format"
df <- melt(m)
colnames(df) <- c("x", "y", "value")

ggplot(df, aes(x = , x, y = y, fill = value)) +
  labs(
    title = "Correlation matrix - GDP vs. Variable",
    x = "Variable",
    y = "GDP"
  ) +
  geom_tile() +
  geom_text(aes(x, y, label = value),
            color = ifelse(df$value > 0.5, "white", "black"), size = 5) +
  scale_fill_distiller(palette = 2, direction = 1)
