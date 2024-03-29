source("./Correlation/statistical_tests.R")
source("./Correlation/visual_tests.R")
source("./Correlation/auto_correlation_testing.R")
pacman::p_load(rnaturalearth, stringr, sf, reshape)

#' Loads and merges Penn World Table and Natural Earth datasets
#'
#' Drops geometry data from Natural Earth and adds columns to Penn World Table before returning a merged dataframe.
#'
#' @returns dataframe, the merged Penn World Table and Natural Earth data.
prepare_dataset <- function(){
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
  
  ## Merge PWT with NE
  merged_penn_ne <- ne %>%
    left_join(penn, by = join_by(countrycode))
  
  return (merged_penn_ne)
}

#' Returns a character to use as render name for y-axis.
#'
#' If `y_var_name` is in `named_vec`, returns the name of `named_vec` element
#' Else returns default set in `default`
#'
#' @param y_var_name character, word to check in `named_vec`
#' @param named_vec character vector
#' @param default character, default return value
#' @returns character
get_y_render_name <- function(y_var_name, named_vec, default="GDP per capita"){
  if (!y_var_name %in% named_vec)
    return (default)
  y_render_name_i <- which(named_vec==y_var_name)
  return(names(named_vec)[y_render_name_i])
}

#' Given a dataframe
#' 
#' Select necessary columns
#' (if) normalize data
#' create fortified linear model
#' (if) save results
#' return Pearson R2 value
#' 
#' @param data character, word to check in `X_VARS_VEC`
#' @param x_var_name character, independent variable name used to retrieve data from `data`
#' @param y_var_name character, dependent variable name used to retrieve data from `data`
#' @param x_render_name character, name to use when referring to the independent data (e.g: when creating plots or dirs/saving)
#' @param y_render_name character, name to use when referring to the dependent data (e.g: when creating plots or dirs/saving)
#' @param data_scope_str character, metadata about scope of `data`
#' @param save_res logical, default = TRUE, Whether the results of the linear correlation should get saved (plots and stat test)
#' @returns float vector, vector w/ length = 2 containing: Pearson's P-value and R2 of the linear relationship
lm_pipeline <- function(data, x_var_name, y_var_name, x_render_name, y_render_name, data_scope_str, save_res=TRUE){
  data <- select_x_y_data(data, y_var_name, x_var_name)
  if (nrow(data) == 0) {
    warning(paste(x_render_name, "contains only NA values"), call. = FALSE)
    return(NA)
  }
  
  lin_model <- lm(data[[y_var_name]] ~ data[[x_var_name]], na.action = na.omit)
  
  ## Save the correlation results
  output_dir <- ""
  if (save_res){
    output_dir <- paste("./Correlation/Output", y_render_name, data_scope_str, x_render_name,
                        sep = "/", collapse = "/")
    create_output_dir(output_dir)
  }
  ## Always need these correlation stats for return or save_
    test_res <- gen_corr_stats(lin_model, output_dir)
    r_squared <- get_stat_result_by_testname(test_res, "R-squared", TRUE)
    p_val <- get_stat_result_by_testname(test_res, "Reject H₀ with p-value")
      
  ## Only need plots and some stats if they get saved
  if (save_res){
    sw_stat <- get_stat_result_by_testname(test_res, "Shapiro Wilk: statistic for residuals")
    sw_p <- get_stat_result_by_testname(test_res, "Shapiro Wilk: P-value for residuals indicates linearity")
    s <- get_stat_result_by_testname(test_res, "s")
    
    ac_df <- auto_correlation_dataframe(data[[x_var_name]])
      
    save_correlation_plots(data_scope_str, lin_model, y_render_name, x_render_name,
                           output_dir, sw_stat, sw_p, s, r_squared, ac_df)
  }

  #Return p value and rsquared
  return(c(p_val, r_squared))
}

#TODO Docstring
select_x_y_data <- function(data, y_var_name, x_var_name){
  data <- data %>%
    select(c(y_var_name, x_var_name)) %>%
    filter(.[[x_var_name]] != 1)
  return(data)
}

#TODO Docstring
create_output_dir <- function(output_dir){
  ## Create output directories if they do not exist yet
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)
}

#' Wrapper function for generating linear correlation tests
#'
#' @param data dataframe, should contain PWT and NE data 
#' @param data_scope_str character, metadata about scope of `data`
#' @param x_vars_vec named character vector, should contain "x var names"
#' @param y_var_name character, dependent variable name used to retrieve data from `data`
#' @param normalize_data logical, default = FALSE, whether the data should get normalized
#' @param save_res logical, default = TRUE, Whether the results of the linear correlation should get saved (plots and stat test)
#' @returns list, contains the correlation result for each independent variable
gen_cor_res <- function(data, data_scope_str, x_vars_vec, y_var_name, normalize_data=FALSE, save_res=TRUE) {
  #TODO: Do not really want to adjust the dataframe in this function
  
  ## Prepare data
  data <- data %>%
    mutate(gdppercap = log(cgdpo / pop)) %>%
    group_by(year) %>%
    summarize_if(is.numeric, function(x) mean(x, na.rm = TRUE))
  if (normalize_data) data <- data %>% scale %>% data.frame
  
  ## Map every variable in vars and correlate against GDP,
  ## the results are stored in a list.
  y_render_name <- get_y_render_name(y_var_name, x_vars_vec)
  res_list <- mapply(lm_pipeline, list(data),
                     x_vars_vec, y_var_name, names(x_vars_vec), y_render_name, data_scope_str, save_res,
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
  names(res_list) <- names(x_vars_vec)
  
  return(res_list)
}

#' Wrapper function for generating linear correlation tests
#'
#' @param normalize_data logical, default = FALSE, whether the data should get normalized
#' @param save_res logical, default = TRUE, Whether the results of the linear correlation should get saved (plots and stat test)
#' @returns list, contains the nested correlation result for each independent variable
generate_cor_res <- function(y_var_name="gdppercap", normalize_data=FALSE, save_res=TRUE){
  
  #TODO: Figure out how to name the elements in vector without underscores 
  #     for prettier output (right now crashes later on when generating matrices)
  X_VARS_VEC <- setNames(c("pop", "avh", "cshm_i", "rkna", "labshm", "hc", "rtfpna"), 
                         c("Population", "Average_hours_worked", "Capital_formation", 
                           "Capital_services","Labor_compensation", "HCI", "TFP" ))
  
  #Uncomment to test independent variable collinearity 
  #y_var_name = "pop"
  
  data <- prepare_dataset()
  
  ## Join on countries with desired economy status and generate results
  cor_res <- list(
    #TODO: Fix code in comments
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
    "Developing countries" = data %>%
      filter(economy == "Developing region") %>%
      gen_cor_res("Developing countries", X_VARS_VEC, y_var_name, normalize_data, save_res),
    "All countries" = data  %>%
      gen_cor_res("All countries", X_VARS_VEC, y_var_name, normalize_data, save_res)#,
    
    #USA = penn %>% filter(countrycode == "USA") %>% gen_cor_res("USA"),
    #"The Netherlands" = penn %>% filter(countrycode == "NLD") %>% gen_cor_res("The Netherlands"),
    #Germany = penn %>% filter(countrycode == "DEU") %>% gen_results("Germany"),
    #Italy = penn %>% filter(countrycode == "ITA") %>% gen_cor_res("Italy"),
    #China = penn %>% filter(countrycode == "CHN") %>% gen_cor_res("China"),
    #Russia = penn %>% filter(countrycode == "RUS") %>% gen_cor_res("Russia")
  )
  
  return (cor_res)
}

generate_cor_res()
