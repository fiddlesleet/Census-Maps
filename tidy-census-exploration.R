library(tidycensus)
library(tidyverse)
library(viridis)
library(ggplot2)
library(mapview)
devtools::install_github("tidyverse/ggplot2") # need for geom_sf()

options(tigris_use_cache = TRUE)

# explore potential variables
v <- load_variables(2010, "sf1", cache = TRUE)
view(v)


# get_decennial gives data from 1990, 2000 or 2010 US census
m90 <- get_decennial(geography = "state", # get data by state
                     variables = "H043A001", # median gross rent
                     year = 1990)
# inspect
m90

# visualize
m90 %>% 
  ggplot(aes(x = value, y = reorder(NAME, value))) +
  geom_point()

# get median household data by county for Washington, using ACS from 2011-2015
# Note: ACS returns estimate and margin of error, with default moe = 90 percent confidence level 
# - Can change confidence level to 95 or 99 with moe_level parameter in get_acs
wa <- get_acs(geography = "county",
              variable = c(medincome = "B19013_001"),
              state = "WA")
wa

# visualize
wa %>% 
  mutate(NAME = gsub(" County, Washington", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) + 
  labs(title = "Household income by county in Washington State",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

# map household income for Orange County by census tract
orange <- get_acs(state = "CA", county = "Orange",
                  geography = "tract", 
                  variables = "B19013_001", # median household income
                  geometry = TRUE)
orange

# plot 
orange %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "inferno") + 
  scale_color_viridis(option = "inferno")

# faceted mapping
racevars <- c(White = "P0050003", 
              Black = "P0050004", 
              Asian = "P0050006", 
              Hispanic = "P0040003")
philly <- get_decennial(geography = "tract", variables = racevars,
                        state = "PA", county = "Philadelphia County", 
                        geometry = TRUE, summary_var = "P0010001")
# inspect
philly # value = the value by race, summary_value = overall population

# plot
philly %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = pct, color = pct)) +
  facet_wrap(~variable) +
  geom_sf() +
  coord_sf(crs = 26915) + 
  scale_fill_viridis() +
  scale_color_viridis()


ny <- get_acs(geography = "tract", 
              variables = "B19013_001", # median household income 
              state = "NY", 
              county = "New York", 
              geometry = TRUE,
              cb = FALSE) # used to subtract water boundaries for cities around water

mapview(ny, zcol = "estimate", legend = TRUE)
