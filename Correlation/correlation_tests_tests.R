source("utils.R", chdir = TRUE)
pacman::p_load(dplyr, stringr, ggplot2, rnaturalearth, rnaturalearthdata)

START_YEAR <- 1990
END_YEAR <- 2019

## Import and clean PWT
pwt <- pwt() %>%
  filter(year >= START_YEAR & year <= END_YEAR) %>%
  filter(!is.na(rgdpna)) %>%
  mutate(rgdpna = (rgdpna / pop)) %>%
  mutate(avh_by_pop = (avh * emp) / pop)

#hc vs GDP ALL
#hc_vs_gdp <- ggplot(data= pwt, aes(x=hc, y=rgdpna, color=countrycode)) +
#  geom_point()
#hc_vs_gdp

nw <- ne_countries(scale = "medium", returnclass = "sf") %>%
  rename("countrycode" = "iso_a3") %>%
  filter(admin != "Antarctica") %>%
  mutate(economy = str_replace(economy, "^.{0,3}", "")) %>%
  select(c("countrycode", "economy"))

joined_data <- merge(nw, pwt)


hc_vs_gdp <- ggplot(data= joined_data, aes(x=hc, y=rgdpna, color=economy)) +
  geom_point()
hc_vs_gdp

ftp_vs_gdp <- ggplot(data= joined_data, aes(x=ctfp, y=rgdpna, color=economy)) +
  geom_point()
ftp_vs_gdp


subset_NL <- joined_data[joined_data$countrycode == "NLD",]


hc_vs_gdp <- ggplot(data= subset_NL, aes(x=hc, y=rgdpna, color=economy)) +
  geom_point()
hc_vs_gdp

cor_test_year_rgdpna <- cor(subset_NL$hc, subset_NL$rgdpna, method = c("pearson"))

cor_test_tfp_rgdpna <- cor(subset_NL$ctfp, subset_NL$rgdpna, method = c("pearson"))

tfp_vs_gdp <- ggplot(data= subset_NL, aes(x=ctfp, y=rgdpna, color=economy)) +
  geom_point()
tfp_vs_gdp

#FOR LOOP, FOR EACH VARIABLE ATTEMPT PEARSON relation test, then create a dictionary with results, sorted by positive to negative correlations



#for loop 



#FTP GDP
#WORKING HOURS GDP
#WORKING HOURS FTP
#WORKING HOURS (TAKING INTO ACCOUNT TOTAL POPULATION?)
#HC




