source("Linegraphs/utils.R")
pacman::p_load(ggplot2)

Country_gdp <- function(start_year, end_year, c_name){
  data <-
    pwt() %>%
    filter(year >= start_year & year <= end_year & c_name == country) %>%
    filter(!is.na(rgdpe)) %>%
    select(c("year", "countrycode","country", "rgdpe"))
  
  return(data)
}
