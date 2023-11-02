#TODO: Change `hyponose` func (in ./utils.R), new name, refactor and relocate (similarities with func `correlate`).
#TODO: Fix funcs `generate_matrix_pearson` and `gen_matrix_pearson_conf`; highly similar, see what parts can be merged, (probably want a parameter for whether we would like to show p-value scale)
source("Correlation/utils.r")
pacman::p_load(rnaturalearth, stringr, sf, reshape)

vars <- list(
  pop    = "Population",
  avh    = "Average_hours_worked",
  cshm_i = "Capital_formation",
  rkna   = "Capital_services",
  labshm = "Labor_compensation",
  hc     = "HCI",
  rtfpna = "TFP"
  #chekjeld = "CheKjeldCoefficient"
)

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

generate_matrix_pearson <- function(){
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
  #save_plots("Correlation/Output/correlation_matrix.png", "", cor_matrix)
  #save_plots("Correlation/Output/hypothesis_pass_both.png", "", cor_matrix_both)
  #save_plots("Correlation/Output/hypothesis_pass.png", "", cor_matrix)
}

gen_matrix_pearson_conf <- function(){
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
  hypres <- list(
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
      gen_results_hypo("Emerging countries", TRUE)#,
    # "Developing countries" = ne %>%
    #   filter(economy == "Developing region", TRUE) %>%
    #   left_join(penn, by = join_by(countrycode)) %>%
    #   gen_results_hypo("Developing countries", TRUE),
    # 
    # USA = penn %>% filter(countrycode == "USA") %>% gen_results_hypo("USA", TRUE),
    # "The Netherlands" = penn %>% filter(countrycode == "NLD") %>% gen_results_hypo("The Netherlands", TRUE),
    # #Germany = penn %>% filter(countrycode == "DEU") %>% gen_results_hypo("Germany"),
    # Italy = penn %>% filter(countrycode == "ITA") %>% gen_results_hypo("Italy", TRUE),
    # China = penn %>% filter(countrycode == "CHN") %>% gen_results_hypo("China", TRUE),
    # Russia = penn %>% filter(countrycode == "RUS") %>% gen_results_hypo("Russia", TRUE)
  )
  
  ## Generate a correlation matrix
  #Takes vector column names, returns vector with length col_names * 2 containing different suffix names depending on location of name in vector
  append_char_even_odd <- function(col_names, even_char, odd_char){
    return_vec <- c()
    for (i in 1:length(col_names)){
      return_vec <- append(return_vec, paste(col_names[i], even_char))
      return_vec <- append(return_vec, paste(col_names[i], odd_char))
    }
    return(return_vec)
  }
  
  m <- matrix(unlist(hypres), ncol = length(hypres[[1]]) * 2, byrow = TRUE)
  #colnames(m_p) <- names(hypres_p[[1]])
  colnames(m) <- append_char_even_odd(names(hypres[[1]]), "P-value", "Correlation")
  
  #rownames(m_p) <- names(hypres_p)
  rownames(m) <- names(hypres)
  
  ## Transform the matrix into a "long format"
  df <- melt(m)
  colnames(df) <- c("y", "x", "value")
  #transform the df (again)
  df <- df %>%
    mutate(type = word(x,-1)) %>% 
    mutate(x = word(x,1))
  
  #test <- distinct(df_p, y)
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
  
  cor_matrix <- ggplot(final_df, aes(x = , x, y = y, fill = p.value)) +
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
  cor_matrix
  #TODO: Save (not here but after this code gets ran)
}

#generate_matrix_pearson()
gen_matrix_pearson_conf()