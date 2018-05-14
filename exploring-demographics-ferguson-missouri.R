library(choroplethr)
library(dplyr)
library(choroplethrZip)
library(ggplot2)
library(reshape2)
library(choroplethrMaps)

#########################################################################################
# GET DATA
# Ferguson is in Zipcode 63135, St. Louis County (FIPS code 29189), and state of Missouri
#########################################################################################

# Get Missouri demographics
# Data comes from the 2013 5-year American Community Survey (ACS).
data(df_state_demographics)
df.missouri_demographics <- tbl_df(df_state_demographics) %>%
  filter(region == "missouri") %>% 
  select(percent_white, percent_black)

# Get St. Louis County Demographic
# Data comes from the 2013 5-year American Community Survey (ACS)s
data(df_county_demographics)
df.st_louis_demographics <-  tbl_df(df_county_demographics) %>%
  filter(region == "29189") %>% 
  select(percent_white, percent_black)

# Get zip code demographics
data("df_zip_demographics")
df.zipcode_demographics <-  tbl_df(df_zip_demographics) %>%
  filter(region == "63135") %>% 
  select(percent_white, percent_black)

# Combine race stats into single dataframe
df.race_stats <- cbind(region = c("state", "county", "zip"), 
                      rbind(df.missouri_demographics, df.st_louis_demographics, df.zipcode_demographics))
df.race_stats <- melt(df.race_stats)
colnames(df.race_stats) <- c("region", "demographic metric", "percent")
df.race_stats


#######
# PLOTS
#######

# make bar chart of demographics by region
ggplot(df.race_stats, aes(x = region, y = percent, fill = `demographic metric`)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Demographics of the State, County and ZCTA\n of Ferguson, Missouri")


###########
# MAKE MAPS
###########


# draw highlight around a county 
highlight_county <- function(county_fips) {
  data(county.map)
  df <- county.map[county.map$region %in% county_fips, ]
  geom_polygon(data=df, aes(long, lat, group = group), color = "yellow", fill = NA, size = 1)
}

# map white population
df_county_demographics$value <- df_county_demographics$percent_white
map.missouri_white <- county_choropleth(df_county_demographics, state_zoom="missouri", num_colors=1) + 
  highlight_county(29189) +
  ggtitle("Missouri Counties\n Percent White") +
  coord_map()

data(df_pop_county)
county_choropleth(df_pop_county, 
                  title  = "US 2012 County Population Estimates", 
                  legend = "Population")


# map white population
df_county_demographics$value <- df_county_demographics$percent_black
map.missouri_black <- county_choropleth(df_county_demographics, state_zoom="missouri", num_colors=1) + 
  highlight_county(29189) +
  ggtitle("Missouri Counties\n Percent Black") + 
  coord_map()

library(gridExtra)
grid.arrange(map.missouri_white, map.missouri_black, ncol=2)



# highlight a zcta
highlight_zip = function(zip)
{
  library(choroplethrZip)
  data(zip.map)
  df = zip.map[zip.map$region %in% zip, ]
  geom_polygon(data=df, aes(long, lat, group=group), color="yellow", fill=NA, size=0.5)
}

df_zip_demographics$value = df_zip_demographics$percent_white
choro_white = zip_choropleth(df_zip_demographics, num_colors=1, msa_zoom="St. Louis, MO-IL") +
  highlight_zip("63135") + 
  ggtitle("St. Louis MSA ZCTAsn Percent White") + 
  coord_map()

df_zip_demographics$value = df_zip_demographics$percent_black
choro_black = zip_choropleth(df_zip_demographics, num_colors=1, msa_zoom="St. Louis, MO-IL") +
  highlight_zip("63135") + 
  ggtitle("St. Louis MSA ZCTAsn Percent Black") + 
  coord_map()

grid.arrange(choro_white, choro_black, ncol=2)
