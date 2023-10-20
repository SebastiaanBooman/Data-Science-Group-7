source("../common.R", chdir = TRUE)

##global-values
first_filter_range <- c(1:3)
second_filter_range <- c(55:99)

#global-methods
get_fdi_numbers <- function(data, country_name){
  return( data %>% 
            select(everything()) %>% 
            filter(if_any(c_name, function(x) country_name == x)) %>% 
            select(!c(1,2)) %>% 
            mutate_all(., function(x) as.integer(as.character(x)))
  )
}