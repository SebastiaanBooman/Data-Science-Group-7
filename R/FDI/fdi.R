source("FDI/utils.R")

##FDI-INFLOWS
ds <- FDI(3)

select_countries <- ds %>% 
  select(1,2) %>%
  filter(!row_number() %in% first_filter_range) %>% 
  filter(!row_number() %in% second_filter_range) %>% 
  na.omit()

select_numbers <- ds %>% 
  select(c(3:10,15,20,25,30,35,40,45,50,55,60)) %>% 
  filter(!row_number() %in% first_filter_range) %>% 
  filter(!row_number() %in% second_filter_range) %>% 
  filter(if_any(everything(), (function(x) !is.na(x))))

FDI_INFLOW <- bind_cols(select_countries, select_numbers)
FDI_INFLOW <- rename(FDI_INFLOW, c_code = ...1, c_name = ...2)


##FDI-OUTFLOWS
#the table for outflows has asterisks* in the country names so i take these from the inflows which does not have *
ds <- FDI(3)

select_countries <- ds %>% 
  select(1,2) %>%
  filter(!row_number() %in% first_filter_range) %>% 
  filter(!row_number() %in% second_filter_range) %>% 
  na.omit()

ds <- FDI(2)

select_numbers <- ds %>% 
  select(c(3:10,15,20,25,30,35,40,45,50,55,60)) %>% 
  filter(!row_number() %in% first_filter_range) %>% 
  filter(!row_number() %in% second_filter_range) %>% 
  filter(if_any(everything(), (function(x) !is.na(x))))

FDI_OUTFLOW <- bind_cols(select_countries, select_numbers)
FDI_OUTFLOW <- rename(FDI_OUTFLOW, c_code = ...1, c_name = ...2)

