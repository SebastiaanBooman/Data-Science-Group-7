source("Correlation/utils.r")
pacman::p_load(rnaturalearth, stringr, sf, reshape)

#CONSTANTS
INDEP_VARS_LIST <- list(
  pop    = "Population",
  avh    = "Average_hours_worked",
  cshm_i = "Capital_formation",
  rkna   = "Capital_services",
  labshm = "Labor_compensation",
  hc     = "HCI",
  rtfpna = "TFP"
  #chekjeld = "CheKjeldCoefficient"
)

NORMALIZE_DATA = TRUE
SAVE_RESULTS = TRUE

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

gen_cor_res <- function(data, out) {
  data <- data %>%
    mutate(gdppercap = log(cgdpo / pop)) %>%
    group_by(year) %>%
    summarize_if(is.numeric, function(x) mean(x, na.rm = TRUE))
  
  #Pipeline each independent variable will follow
  #-> Select necessary columns
  #-> (if) normalize data
  #-> create fortified linear model
  #-> (if) save results
  #-> return Pearson R2 value
  lm_pipeline <- function(term_var_name, indep_name){
    data <- data %>%
      select(c("gdppercap", term_var_name)) %>%
      filter(.[[term_var_name]] != 1)
    if (NORMALIZE_DATA) data <- data %>% scale
    data <- data %>% data.frame
    if (nrow(data) == 0) {
      warning(paste(independent_name, "contains only NA values"), call. = FALSE)
      return(NA)
    }
    
    model <- lm(data[["gdppercap"]] ~ data[[term_var_name]], na.action = na.omit)
    f_model <- fortify(model)
    
    #f_model <- create_fortified_lm(data, "gdppercap", term_var_name, "GDP per Capita", indep_name)
    colnames(f_model)[1] <- "response"
    colnames(f_model)[2] <- "terms"
    if (SAVE_RESULTS){
      output_dir <- paste("./Correlation/Output", out, indep_name,
                          sep = "/", collapse = "/")
      ## Create output directories if they did not exist yet
      if (!dir.exists(output_dir))
        dir.create(output_dir, recursive = TRUE)
      
      save_correlation_stats(f_model, output_dir)
      save_correlation_plots(f_model, "GDP per Capita", indep_name,
                             output_dir)
    }
    
    cor_test <- cor.test(f_model$response, f_model$terms, method = "pearson",
                         conf.level = 0.95)
    #Return p value and rsquared
    return(c(cor_test$p.value, round(cor_test$estimate ^ 2, 2)))
  } 
  
  ## Map every variable in vars and correlate against GDP,
  ## the results are stored in a list.
  res_list <- mapply(lm_pipeline, 
                     names(INDEP_VARS_LIST), INDEP_VARS_LIST, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  names(res_list) <- unlist(INDEP_VARS_LIST)
  
  return(res_list)
}
## Join on countries with desired economy status and generate results
cor_res <- list(
  #"NLD + USD" = ne %>%
  #  filter(countrycode == "USA" | countrycode == "NLD") %>%
  #  left_join(penn, by = join_by(countrycode)) %>%
  #  gen_results("NLD + USA"),
  # "Developed countries" = ne %>%
  #   filter(str_detect(economy, "^Developed region")) %>%
  #   left_join(penn, by = join_by(countrycode)) %>%
  #   gen_results("Developed countries"),
  # "Emerging countries" = ne %>%
  #   filter(str_detect(economy, "^Emerging region")) %>%
  #   left_join(penn, by = join_by(countrycode)) %>%
  #   gen_results("Emerging countries"),
  "Developing countries" = ne %>%
    filter(economy == "Developing region") %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_cor_res("Developing countries"),
  "All countries" = ne %>%
    left_join(penn, by = join_by(countrycode)) %>%
    gen_cor_res("All countries")#,
  
  #USA = penn %>% filter(countrycode == "USA") %>% gen_cor_res("USA"),
  #"The Netherlands" = penn %>% filter(countrycode == "NLD") %>% gen_cor_res("The Netherlands"),
  #Germany = penn %>% filter(countrycode == "DEU") %>% gen_results("Germany"),
  #Italy = penn %>% filter(countrycode == "ITA") %>% gen_cor_res("Italy"),
  #China = penn %>% filter(countrycode == "CHN") %>% gen_cor_res("China"),
  #Russia = penn %>% filter(countrycode == "RUS") %>% gen_cor_res("Russia")
)

append_char_even_odd <- function(col_names, even_char, odd_char){
  return_vec <- c()
  for (i in 1:length(col_names)){
    return_vec <- append(return_vec, paste(col_names[i], even_char))
    return_vec <- append(return_vec, paste(col_names[i], odd_char))
  }
  return(return_vec)
}

m <- matrix(unlist(cor_res), ncol = length(cor_res[[1]]) * 2, byrow = TRUE)
colnames(m) <- append_char_even_odd(names(cor_res[[1]]), "P-value", "Correlation")
rownames(m) <- names(cor_res)

## Transform the matrix into a "long format"
df <- melt(m)
colnames(df) <- c("y", "x", "value")
#transform the df (again)
df <- df %>%
  mutate(type = word(x,-1)) %>% 
  mutate(x = word(x,1))

unique_combos <- unique(df[c("x","y")])

df_corr <- data.frame("p-value"  = df[df$type == "P-value", "value"],
                      "correlation" = df[df$type == "Correlation", "value"]
)
get_color_based_on_p_val <- function(p_val){
  #STRONG -> GREEN \ MODERATE -> YELLOW \ WEAK -> ORANGE \ INSUFFICIENT -> RED
  if (p_val < 0.001) return("green")
  if (p_val< 0.05) return("yellow")
  if (p_val < 0.1) return("orange")
  return("red")
}
final_df <- cbind(unique_combos, df_corr) %>%
  mutate(conf = mapply(get_color_based_on_p_val,p.value))

cor_matrix <- ggplot(final_df, aes(x = , x, y = y, fill = correlation)) +
  labs(
    #title = "Correlation matrix - GDP vs. Variable",
    x = "Variable",
    y = "GDP"
  ) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(x, y, label = correlation),
            color = ifelse(final_df$correlation > 0.5, "white", "black"), size = 5) +
  scale_fill_distiller(palette = 2, direction = 1) +
  #presentation_theme +
  theme(axis.text.x = element_text(angle = 335, hjust = 0,
                                   margin = margin(3, 0, 0, 0)))
cor_matrix
#save_plots("Correlation/Output/cor_matrix.png", "", cor_matrix)


#Matrix including P-values
#TODO: Does not work as desired yet, colors should be discrete instead of continous scale (including legend)
cor_matrix_conf  <- ggplot(final_df, aes(x = , x, y = y, fill = p.value)) +
  labs(
    #title = "Correlation matrix - GDP vs. Variable",
    x = "Variable",
    y = "GDP"
  ) +
  geom_tile(show.legend = TRUE) +
  geom_text(aes(x, y, label = round(correlation, 3)),
            # color = ifelse(final_df$p.value < 0.0000000000000000005, "white", "black"),
            color = "black",
            size = 5) +
  scale_fill_distiller(palette = 2, direction = 1) +
  #scale_color_identity() +
  #scale_color_manual(values=final_df$conf) +
  #presentation_theme +
  theme(axis.text.x = element_text(angle = 335, hjust = 0,
                                   margin = margin(3, 0, 0, 0)))
#cor_matrix_conf
#TODO: Save (not here but after this code gets ran)
#save_plots("Correlation/Output/cor_matrix_conf.png", "", cor_matrix_conf)