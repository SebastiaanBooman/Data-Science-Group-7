source("common.R")

##global-values
first_filter_range <- c(1:3)
second_filter_range <- c(55:99)

#global-methods
get_fdi_numbers <- function(data, country_name, start_year, end_year){
  d <- data %>% 
    select(everything()) %>% 
    filter(if_any(c_name, function(x) country_name == x)) %>% 
    select(!c(1,2)) %>% 
    mutate_all(., function(x) as.integer(as.character(x)))
  
  d <- d %>% pivot_longer(cols = as.character(2005:2022), names_to = "year") %>% 
    filter(year >= start_year & year <= end_year)
  return(d)
}