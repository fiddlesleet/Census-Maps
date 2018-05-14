# setwd('./Downloads')
# getwd()
library(choroplethr)
library(acs)
library(ggplot2)
library(dplyr)
library(choroplethrAdmin1)
library(blscrapeR) # scrapes unemployment data
library(RColorBrewer)

data("continental_us_states")
data(df_county_demographics)
data(df_president_ts)
data(df_pop_county)

api.key.install('3f00945100252d6c2182ca15875df1d3c20181d5')

#-------------------------- NATIONWIDE -----------------------------#
# map per capita income by state
state_choropleth_acs("B19301", 
                     num_colors = 5)
# percent black
df_county_demographics$value = df_county_demographics$percent_black
county_choropleth(df_county_demographics) + scale_colour_brewer(palette = "Set1")



# percent white
df_county_demographics$value = df_county_demographics$percent_white
county_choropleth(df_county_demographics)

# percent hispanic
df_county_demographics$value = df_county_demographics$percent_hispanic
county_choropleth(df_county_demographics)
#-------------------------- WASHINGTON -----------------------------#

# get demographics from choroplethr
wa_stats = get_tract_demographics("washington")
head(wa_stats)

# get tract map washington
wa = get_tract_map("washington")
ggplot(wa, aes(long, lat, group=group)) + geom_polygon()

# per capita income
county_choropleth_acs("B19301", 
                      num_colors = 1, 
                      state_zoom = 'washington')

### tract_choropleth takes in a data frame with a column named 'value'

# plot tracts by median rent
wa_stats$value = wa_stats$median_rent
tract_choropleth(wa_stats, "washington", 
                 title = "2013 Per Capita Income \nCensus Tracts", 
                 legend="Range")

# plot tracts by per capita income
wa_stats$value = wa_stats$per_capita_income
tract_choropleth(wa_stats, "washington", 
                 title = "2013 Per Capita Income \nCensus Tracts", 
                 legend="Range")
tract_choropleth(wa_stats, "washington", 
                 title = "2013 Per Capita Income \nCensus Tracts", 
                 legend="Range", reference_map = TRUE)

# plot tracts by percent white
wa_stats$value = wa_stats$percent_white
tract_choropleth(wa_stats, "washington", 
                 title = "2013 Percent White \nCensus Tracts", legend="Percent")

# plot tracts by percent black
wa_stats$value = wa_stats$percent_black
tract_choropleth(wa_stats, "washington", 
                 title = "2013 Percent Black \nCensus Tracts", legend="Percent")
tract_choropleth(wa_stats, "washington", 
                 title = "2013 Percent Black \nCensus Tracts", legend="Percent", reference_map = TRUE)
#-------------------------- PENNSYLVANIA -----------------------------#

# get demographics from choroplethr
pa_stats = get_tract_demographics("pennsylvania")
head(pa_stats)

# get tract map pennsylvania
pa = get_tract_map("pennsylvania")
ggplot(pa, aes(long, lat, group=group)) + geom_polygon()

# per capita income
county_choropleth_acs("B19301", 
                      num_colors = 1, 
                      state_zoom = 'pennsylvania')

# plot tracts by median rent
pa_stats$value = pa_stats$median_rent
median_rent = tract_choropleth(pa_stats, "pennsylvania", 
                 title = "2013 Median Rent\nCensus Tracts", legend="Dollars")
median_rent$ggplot_polygon = geom_polygon(aes(fill = value), 
                                color = NA)
median_rent$render()

# plot tracts by per capita income
wa_stats$value = wa_stats$per_capita_income
tract_choropleth(wa_stats, "washington", 
                 title = "2013 Per Capita Income \nCensus Tracts", 
                 legend="Range")

# plot tracts by percent white
pa_stats$value = pa_stats$percent_white
tract_choropleth(pa_stats, "pennsylvania", 
                 title = "2013 Percent White \nCensus Tracts", legend="Percent")

# plot tracts by percent black
pa_stats$value = pa_stats$percent_black
class(tract_choropleth(pa_stats, "pennsylvania", 
                 title = "2013 Percent Black \nCensus Tracts", legend="Percent"))

#--------------------- MY WHOLE WORLD PRE-UNIVERSITY -----------------#
# washington fips = 53, philly fips = 101

# whatcom, skagit, san juan, king, island, snohomish 
wa_county_fips = c(53073, 53057, 53055, 53033, 53029, 53061)
pnw_counties = get_tract_demographics('washington', county_fips = wa_county_fips, 
                                     endyear = 2010, span = 5)
pnw_counties$value = pnw_counties$per_capita_income

county_choropleth(pnw_counties, "washington", legend = "Dollars", 
                 county_zoom = 53057)

pnw_m1 = tract_choropleth(pnw_counties, "washington", 
                           legend = "Dollars", county_zoom = wa_county_fips)

pnw_m2 = tract_choropleth(pnw_counties, "washington", 
                          legend = "Dollars", county_zoom = wa_county_fips,
                          reference_map = TRUE)

double_map(pnw_m1, pnw_m2, "2010 Median Rent\nNW Washington Census Tracts")

# show the population of the 5 counties (boroughs) that make up 
# upper left USA
wa_county_names = c("skagit", "whatcom", "snohomish", "king", "san juan")
wa_county_fips = county.regions %>%
    filter(state.name == "washington" & county.name %in% wa_county_names) %>%
    select(region)
county_choropleth(df_pop_county,
                  title = "Population of Counties in Upper Left USA",
                  legend = "Population",
                  num_colors = 1,
                  county_zoom = wa_county_fips$region)

nyc_county_names = c("kings", "bronx", "new york", "queens", "richmond")
nyc_county_fips = county.regions %>%
    filter(state.name == "new york" & county.name %in% nyc_county_names) %>%
    select(region)
county_choropleth(df_pop_county,
                  title = "Population of Counties in New York City",
                  legend = "Population",
                  num_colors = 1,
                  county_zoom = nyc_county_fips$region)

# 36061 is the FIPS code for New York county (i.e. Manhattan)
manhattan_2010 = get_tract_demographics("new york", county_fips=36061, endyear = 2010, span = 5)
manhattan_2010$value = manhattan_2010$median_rent

m1 = tract_choropleth(manhattan_2010, "new york", legend = "Dollars", county_zoom = 36061)
m2 = tract_choropleth(manhattan_2010, "new york", legend = "Dollars", county_zoom = 36061, reference_map = TRUE)

?double_map
double_map(m1, m2, "2010 Median Rent\nManhattan Census Tracts")

#-------------------------- PHILADELPHIA -----------------------------#
# pennsylvania fips = 42, philly fips = 101
philly_2010 = get_tract_demographics("pennsylvania", county_fips=42101, endyear = 2015, span = 5)
philly_2010$value = philly_2010$median_rent

m1 = tract_choropleth(philly_2010, "pennsylvania", legend = "Dollars", county_zoom = 42101)
m2 = tract_choropleth(philly_2010, "pennsylvania", legend = "Dollars", county_zoom = 42101, reference_map = TRUE)

double_map(m1, m2, "2010 Median Rent\nPhiladelphia Census Tracts")
