source("FDI/fdi.R")
source("Linegraphs/country_gdp.R")

dutch_fdi_inflow <- get_fdi_numbers(FDI_INFLOW, "Netherlands", 2005, 2018)
dutch_fdi_outflow <- get_fdi_numbers(FDI_OUTFLOW, "Netherlands", 2005, 2018)
dutch_rgdpe <- Country_gdp(2005, 2018, "Netherlands")

dutch_fdi_net <- dutch_fdi_inflow %>% 
  mutate(value = .$value - dutch_fdi_outflow$value)

d <- bind_cols(dutch_fdi_inflow$year,dutch_fdi_inflow$value, dutch_fdi_outflow$value, dutch_fdi_net$value, dutch_rgdpe$rgdpe)
colnames(d) <- c("year", "inflow", "outflow", "net", "rgdpe")

plot.default(x = dutch_fdi_inflow$value, y = dutch_rgdpe$rgdpe)
plot.default(x = dutch_fdi_outflow$value, y = dutch_rgdpe$rgdpe)
plot.default(x = dutch_fdi_net$value, y = dutch_rgdpe$rgdpe)