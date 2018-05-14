#devtools::install_github("hrbrmstr/albersusa")
library(albersusa)
library(tidycensus)
library(tidyverse)
library(ggthemes) #just for theme_map()
library(viridis)


###########
# GET DATA
###########

income <- get_acs(geography = "county", variables = "B19013_001", geometry = TRUE)
hc <- get_acs(geography = "county", variables = "B25105_001", geometry = TRUE)
hc$estimated <- hc$estimate*12
income$percent<-hc$estimated/income$estimate*100


county_shapefile <- counties_sf("aeqd")
county_shapefile$NAME <- paste0(county_shapefile$name,' ', 
                              county_shapefile$lsad,', ',
                              county_shapefile$state)
county_income <- left_join(county_shapefile,income,by=c('NAME'))

names(cty_income)[9]<-'geometry'

county_income %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  scale_fill_viridis(option = "inferno") + 
  scale_color_viridis(option = "inferno")+
  theme_map(base_size = 11,base_family = 'Roboto Condensed')+labs(title='Household Income by County',subtitle='Median household income from the 2011-2015 ACS')

county_income %>%
  ggplot(aes(fill = percent, color = percent)) + 
  geom_sf() + 
  scale_fill_viridis(option = "inferno") + 
  scale_color_viridis(option = "inferno")+
  theme_map(base_size = 11) +
  labs(title='Percent of Household Income Toward Housing Costs',subtitle='Median housing costs divided by median household income\nby county from the 2011-2015 ACS')
