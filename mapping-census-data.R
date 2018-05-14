library(choroplethr)
library(acs)
library(ggplot2)
library(dplyr)
library(choroplethrAdmin1)
library(blscrapeR) # scrapes unemployment data
if (!require(gpclib)) install.packages("gpclib", type="source")

data(county.regions)
head(county.regions)
data("continental_us_states")
data(df_county_demographics)
data(df_president_ts)
data(df_pop_county)

api.key.install('3f00945100252d6c2182ca15875df1d3c20181d5')

#-------------------------- PHILADELPHIA -----------------------------#
# pennsylvania fips = 42, philly fips = 101
philly_2010 = get_tract_demographics("pennsylvania", county_fips=42101, endyear = 2015, span = 5)
head(philly_2010)

# load desired stat into value field
philly_2010$value = philly_2010$median_rent

m1 = tract_choropleth(philly_2010, "pennsylvania", legend = "Dollars", county_zoom = 42101)
m2 = tract_choropleth(philly_2010, "pennsylvania", legend = "Dollars", county_zoom = 42101, reference_map = TRUE)

double_map(m1, m2, "2010 Median Rent\nPhiladelphia Census Tracts")
