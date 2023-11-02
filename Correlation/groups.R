source("Correlation/utils.r")
pacman::p_load(rnaturalearth, stringr, sf, reshape)

vars <- list(
  pop    = "Population",
  avh    = "Average hours worked",
  cshm_i = "Capital formation",
  rkna   = "Capital services",
  labshm = "Labor compensation",
  hc     = "HCI",
  rtfpna = "TFP"
  #chekjeld = "CheKjeldCoefficient"
)

gen_results <- function(data, out) {
  data <- data %>%
    #filter(countrycode != "-99") %>%
    #group_by(countrycode) %>%
    #mutate(gdppercap = log((cgdpo / pop) - (lag(cgdpo) / lag(pop)))) %>%
    #ungroup
    mutate(gdppercap = log(cgdpo / pop)) %>%
    group_by(year) %>%
    summarize_if(is.numeric, function(x) mean(x, na.rm = TRUE))
    #group_by(countrycode) %>%
    #summarize_if(is.numeric, function(x) mean(x, na.rm = TRUE))
    #mutate(chekjeld = mean(cshm_i + rkna + pop + rtfpna + hc, na.rm = TRUE))
    #mutate(cgdpo = (cgdpo / pop))
    #mutate(cgdpo = log(cgdpo)) %>%

  ## Map every variable in vars and correlate against GDP,
  ## the results are stored in a list.
  results <- mapply(function(var, name) {
    correlate(data, "gdppercap", var, "GDP per Capita", name, out, TRUE)
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
    cshm_r = cgdpo * csh_r,
    labshm = rgdpna * labsh,
  )

## Join on countries with desired economy status and generate results
results <- list(
  #"NLD + USD" = ne %>%
  #  filter(countrycode == "USA" | countrycode == "NLD") %>%
  #  left_join(penn, by = join_by(countrycode)) %>%
  #  gen_results("NLD + USA"),
  "Developed countries" = ne %>%
    filter(str_detect(economy, "^Developed region")) %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results("Developed countries"),
  "Emerging countries" = ne %>%
    filter(str_detect(economy, "^Emerging region")) %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results("Emerging countries"),
  "Developing countries" = ne %>%
    filter(economy == "Developing region") %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results("Developing countries"),
  "All countries" = ne %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results("All countries"),

  USA = penn %>% filter(countrycode == "USA") %>% gen_results("USA"),
  "The Netherlands" = penn %>% filter(countrycode == "NLD") %>% gen_results("The Netherlands"),
  #Germany = penn %>% filter(countrycode == "DEU") %>% gen_results("Germany"),
  Italy = penn %>% filter(countrycode == "ITA") %>% gen_results("Italy"),
  China = penn %>% filter(countrycode == "CHN") %>% gen_results("China"),
  Russia = penn %>% filter(countrycode == "RUS") %>% gen_results("Russia")
)

## Generate a correlation matrix
m <- matrix(unlist(results), ncol = length(results[[1]]), byrow = TRUE)
colnames(m) <- names(results[[1]])
rownames(m) <- names(results)

## Transform the matrix into a "long format"
df <- melt(m)
colnames(df) <- c("y", "x", "value")

cor_matrix <- ggplot(df, aes(x = , x, y = y, fill = value)) +
  labs(
    #title = "Correlation matrix - GDP vs. Variable",
    x = "Variable",
    y = "GDP"
  ) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(x, y, label = value),
            color = ifelse(df$value > 0.5, "white", "black"), size = 5) +
  scale_fill_distiller(palette = 2, direction = 1) +
  #presentation_theme +
  theme(axis.text.x = element_text(angle = 335, hjust = 0,
                                   margin = margin(3, 0, 0, 0)))
cor_matrix
save_plots("Correlation/Output/correlation_matrix.png", "", cor_matrix)





gen_results_hypo <- function(data, out, p_val=FALSE) {
 data <- data %>%
   #filter(countrycode != "-99") %>%
   #group_by(countrycode) %>%
   #mutate(gdppercap = log((cgdpo / pop) - (lag(cgdpo) / lag(pop)))) %>%
   #ungroup
   mutate(gdppercap = log(cgdpo / pop)) %>%
   group_by(year) %>%
   summarize_if(is.numeric, function(x) mean(x, na.rm = TRUE))
   #group_by(countrycode) %>%
   #summarize_if(is.numeric, function(x) mean(x, na.rm = TRUE))
   #mutate(chekjeld = mean(cshm_i + rkna + pop + rtfpna + hc, na.rm = TRUE))
   #mutate(cgdpo = (cgdpo / pop))
   #mutate(cgdpo = log(cgdpo)) %>%

 ## Map every variable in vars and correlate against GDP,
 ## the results are stored in a list.
 results <- mapply(function(var, name) {
   hyponose(data, "gdppercap", var, "GDP per Capita", name, p_val)
 }, names(vars), vars, SIMPLIFY = FALSE, USE.NAMES = FALSE)

 names(results) <- unlist(vars)

 return(results)
}

## Join on countries with desired economy status and generate results
hypres_p <- list(
 #"NLD + USD" = ne %>%
 #  filter(countrycode == "USA" | countrycode == "NLD") %>%
 #  left_join(penn, by = join_by(countrycode)) %>%
 #  gen_results("NLD + USA"),
 "Developed countries" = ne %>%
   filter(str_detect(economy, "^Developed region")) %>%
   left_join(penn, by = join_by(countrycode)) %>%
   gen_results_hypo("Developed countries", TRUE),
 "Emerging countries" = ne %>%
   filter(str_detect(economy, "^Emerging region")) %>%
   left_join(penn, by = join_by(countrycode)) %>%
   gen_results_hypo("Emerging countries", TRUE),
 "Developing countries" = ne %>%
   filter(economy == "Developing region", TRUE) %>%
   left_join(penn, by = join_by(countrycode)) %>%
   gen_results_hypo("Developing countries", TRUE),

 USA = penn %>% filter(countrycode == "USA") %>% gen_results_hypo("USA", TRUE),
 "The Netherlands" = penn %>% filter(countrycode == "NLD") %>% gen_results_hypo("The Netherlands", TRUE),
 #Germany = penn %>% filter(countrycode == "DEU") %>% gen_results_hypo("Germany"),
 Italy = penn %>% filter(countrycode == "ITA") %>% gen_results_hypo("Italy", TRUE),
 China = penn %>% filter(countrycode == "CHN") %>% gen_results_hypo("China", TRUE),
 Russia = penn %>% filter(countrycode == "RUS") %>% gen_results_hypo("Russia", TRUE)
)

## Generate a correlation matrix
test <- unlist(hypres_p)


m_p <- matrix(unlist(hypres_p), ncol = length(hypres_p[[1]]), byrow = TRUE)
colnames(m_p) <- names(hypres_p[[1]])
rownames(m_p) <- names(hypres_p)

## Transform the matrix into a "long format"
df_p <- melt(m_p)
colnames(df_p) <- c("y", "x", "value")

cor_matrix_p <- ggplot(df_p, aes(x = , x, y = y, fill = value)) +
 labs(
   #title = "Correlation matrix - GDP vs. Variable",
   x = "Variable",
   y = "GDP"
 ) +
 geom_tile(show.legend = FALSE) +
 geom_text(aes(x, y, label = round(value, 3)),
           color = ifelse(df_p$value < 0.001, "green", "red"), size = 5) +
 scale_fill_distiller(palette = 2, direction = 1) +
 #presentation_theme +
 theme(axis.text.x = element_text(angle = 335, hjust = 0,
                                  margin = margin(3, 0, 0, 0)))

cor_matrix_p 



hypres_e <- list(
  #"NLD + USD" = ne %>%
  #  filter(countrycode == "USA" | countrycode == "NLD") %>%
  #  left_join(penn, by = join_by(countrycode)) %>%
  #  gen_results("NLD + USA"),
  "Developed countries" = ne %>%
    filter(str_detect(economy, "^Developed region")) %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results_hypo("Developed countries"),
  "Emerging countries" = ne %>%
    filter(str_detect(economy, "^Emerging region")) %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results_hypo("Emerging countries"),
  "Developing countries" = ne %>%
    filter(economy == "Developing region") %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_results_hypo("Developing countries"),
  
  USA = penn %>% filter(countrycode == "USA") %>% gen_results_hypo("USA"),
  "The Netherlands" = penn %>% filter(countrycode == "NLD") %>% gen_results_hypo("The Netherlands"),
  #Germany = penn %>% filter(countrycode == "DEU") %>% gen_results_hypo("Germany"),
  Italy = penn %>% filter(countrycode == "ITA") %>% gen_results_hypo("Italy"),
  China = penn %>% filter(countrycode == "CHN") %>% gen_results_hypo("China"),
  Russia = penn %>% filter(countrycode == "RUS") %>% gen_results_hypo("Russia")
)

## Generate a correlation matrix
m_e <- matrix(unlist(hypres_e), ncol = length(hypres_e[[1]]), byrow = TRUE)
colnames(m_e) <- names(hypres_e[[1]])
rownames(m_e) <- names(hypres_e)

## Transform the matrix into a "long format"
df_e <- melt(m_e)
colnames(df_e) <- c("y", "x", "value")

cor_matrix_e <- ggplot(df_e, aes(x = , x, y = y, fill = value)) +
  labs(
    #title = "Correlation matrix - GDP vs. Variable",
    x = "Variable",
    y = "GDP"
  ) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(x, y, label = round(value, 3)),
            color = ifelse(df_e$value < 0.001, "green", "red"), size = 5) +
  scale_fill_distiller(palette = 2, direction = 1) +
  #presentation_theme +
  theme(axis.text.x = element_text(angle = 335, hjust = 0,
                                   margin = margin(3, 0, 0, 0)))

cor_matrix_e 


cor_matrix_both <- ggplot(df_p, aes(x = , x, y = y, fill = value)) +
  labs(
    #title = "Correlation matrix - GDP vs. Variable",
    x = "Variable",
    y = "GDP"
  ) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(x, y, label = round(df_e$value, 3)),
            color = ifelse(df_e$value < 0.005, "green", "red"), size = 5) +
  scale_fill_distiller(palette = 2, direction = 1) +
  #presentation_theme +
  theme(axis.text.x = element_text(angle = 335, hjust = 0,
                                   margin = margin(3, 0, 0, 0)))

cor_matrix_both


save_plots("Correlation/Output/hypothesis_pass_both.png", "", cor_matrix_both)





save_plots("Correlation/Output/hypothesis_pass.png", "", cor_matrix)

