# TODO:  WORK THROUGH THIS, MAKE SURE IT WORKS
# TODO: For additional practice, redo this tutorial with census block group data. Because block group is two words you will need to put quotes around them in some functions. You will also need to use the tigris function block_groups instead of tracts.
# TODO: Another interesting task would be to compare the 2010-2014 ACS with an earlier five year period.  The 2005-2009 ACS data would be ideal as these would be non-overlapping years. However, this earlier period is not yet available via the Census API (although it is via FactFinder). Instead you can get the 2006-2010 ACS data. Once you fetch that data you can compute percent change and map that. However, the acs14lite package, used above, does not work with other ACS data periods. You would need to create some custom code or use the acs package. 

## KEY FINDINGS TO EXPLORE: 
#College graduates
#In 130 counties, 40 percent or more of the population 25 or older had a bachelor's degree or higher in 2010-2014, with 40 of these counties located in the South.
#In 22 counties, 50 percent or more of the population 25 or older had a bachelor's degree or higher in 2005-2009; this increased to 34 counties in 2010-2014.
#Of the 3,142 counties or county equivalents in the U.S., the percentage of the population 25 or older with a bachelor's degree or more increased in 1,000 counties and decreased in 60 counties between 2005-2009 and 2010-2014.

#Median household income
#260 counties had median household income greater than $60,000, and 223 had median household income of less than $35,000. Counties surrounding Washington, D.C., and New York City had among the highest median household incomes, along with counties in the San Francisco and Los Angeles metropolitan areas.
#Between 2005-2009 and 2010-2014, median household income increased in 187 counties and decreased in 991 counties. The change in median income was not statistically significant in 1,964 counties.

#Poverty
#In 2010-2014, the county poverty rates (the percentage of people with income below the official poverty threshold) ranged from 1 percent to 53 percent.
#Poverty rates were 30 percent or higher in 119 counties. Of these, 93 counties were in the South.
#Between 2005-2009 and 2010-2014, poverty rates increased in 1,052 counties and decreased in 136 counties. The change in poverty rates was not statistically significant in 1,954 counties.

# _________________________
# WITH TIDY CENSUS
library(tidyr)
library(ggplot2)
library(tidycensus)
library(dplyr)
library(purrr)
library(sf)
library(viridis)
library(forcats)

# use tigris package's caching feature if plan to use this workflow multiple times. 
options(tigris_use_cache = TRUE)

# set api key
#census_api_key("my_key", install = TRUE, overwrite = TRUE)

## df of US state and county IDs:
#    tidycensus uses this data frame to handle translations between state/county 
#    names and FIPS codes. However, this data frame can also be used to generate 
#    a vector of state codes to be fed to the map_df function in purrr. 
#    As such, this is all it takes to get a tibble of total population estimates 
#    for all US Census tracts from the 2011-2015 ACS (Get any ACS or decennial
#    Census data in this way):
df.state_county_fips <- unique(fips_codes$state)[1:51]

#purr's map_df(x, f) is effectively the same as do.call("rbind", lapply(x, f))
df.totalpop <- map_df(df.state_county_fips, function(x) {
    get_acs(geography = "tract", 
            variables = "B01003_001",
            state = x)
})

str(df.totalpop)
df.totalpop # is tibble, don't need head

# tract geometry for mapping: map_df in purrr uses the bind_rows function under
#   the hood, which doesn’t work with simple features objects (yet). However, sf 
#   does have an rbind method that works for sf objects and can be fed to
#   purrr’s reduce function.

### Get data + shapefiles for mapping: 
# use 'geometry = true' to have tidycensus fetche tract feature geometry using 
# the tigris package and automatically  merge it to the ACS data:
#
# NOTE: might note the discrepancy in tracts between the geometry-enabled and 
# regular data frames; this is due to the removal of some water-only tracts in 
# the cartographic boundary shapefiles used by tidycensus.
df.totalpop_shapefiles <- reduce(map(df.state_county_fips, function(x) {
        get_acs(geography = "tract", 
                variables = "B01003_001", 
                state = x, 
                geometry = TRUE) # do automerge shapefiles
    }), 
    rbind
)

str(df.totalpop_shapefiles)
head(df.totalpop_shapefiles) # not tibble

## Two major functions implemented in tidycensus: 
##    get_decennial: access the 1990, 2000, and 2010 decennial US Census APIs
##    and get_acs: access the 5-year American Community Survey APIs.

## search for variables: load_variables(year, dataset ["sf1", "sf3", or "acs5"],
##    cache = TRUE)
v15 <- load_variables(2015, "acs5", cache = TRUE)
View(v15) # use View function in RStudio to interactively browse for variables


# median gross rent by state in 1990
m90 <- get_decennial(geography = "state", variables = "H043A001", year = 1990)
str(m90)
m90 # is tibble

m90 %>%
    ggplot(aes(x = value, y = reorder(NAME, value))) + 
    geom_point()

### American Community Survey data differ from decennial Census data in that 
### ACS data are based on an *****annual sample of ~ 3 million households*****,
### rather than a more complete enumeration of the US population. In turn, ACS 
### data points are estimates characterized by a margin of error.
###
### tidycensus always returns both estimate and margin of error together
###    for any requested variables.

# median household income data from the 2011-2015 ACS for counties in Vermont
vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT")
# moe represents the default 90 percent confidence level around the estimate; 
#    this can be changed to 95 or 99 percent with the 
#    moe_level parameter in get_acs if desired
vt # is tibble

# using margin of error, visualize the uncertainty around the estimate:
vt %>%
    dplyr::mutate(NAME = gsub(" County, Vermont", "", NAME)) %>% ## remove last part of name entry
    ggplot(aes(x = estimate, y = reorder(NAME, estimate))) + # order county names by est
    geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
    geom_point(color = "red", size = 3) +
    labs(title = "Household income by county in Vermont",
         subtitle = "2011-2015 American Community Survey",
         y = "",
         x = "ACS estimate (bars represent margin of error)")

## get shapefiles
# set geometry = TRUE: tidycensus uses the tigris package to retrieve the 
#     corresponding geographic dataset from the US Census Bureau and pre-merge 
#     it with the tabular data obtained from the Census API. 
#     tidycensus uses the Census cartographic boundary shapefiles for faster
#     processing; if you prefer the TIGER/Line shapefiles, set cb = FALSE in the 
#     function call


# median household income from the 2011-2015 ACS for Census tracts in 
#    Orange County, California
orange <- get_acs(state = "CA", county = "Orange", geography = "tract", 
                  variables = "B19013_001", geometry = TRUE)
head(orange)

# map
orange %>%
    ggplot(aes(fill = estimate, color = estimate)) + 
    geom_sf() + 
    coord_sf(crs = 26911) + 
    scale_fill_viridis(option = "magma") + 
    scale_color_viridis(option = "magma")

### facet mapping
## Many Census and ACS variables return counts, however, which are generally
##     inappropriate for choropleth mapping. In turn, get_decennial and get_acs 
##     have an optional argument, summary_var.

# data for non-Hispanic whites, non-Hispanic blacks, non-Hispanic Asians, and
# Hispanics by Census tract for the 2010 Census
racevars <- c("P0050003", "P0050004", "P0050006", "P0040003")

# specify total population as the summary variable
# year not necessary here: default is 2010
harris <- get_decennial(geography = "tract", variables = racevars, 
                        state = "TX", county = "Harris County", geometry = TRUE,
                        summary_var = "P0010001") 
harris # tibble

# uses forcats
harris %>%
    #  calculate a new percent-of-total column
    mutate(pct = 100 * (value / summary_value), 
           # recode the Census variable names into more intuitive labels
           variable = fct_recode(variable,
                                 White = "P0050003",
                                 Black = "P0050004",
                                 Asian = "P0050006",
                                 Hispanic = "P0040003")) %>%
    ggplot(aes(fill = pct, color = pct)) +
    theme(text=element_text(family="Garamond", size=14)) +
    labs(title = "Population Percentage by Race",
         subtitle = "2010 U.S. Census",
         y = "",
         x = "") +
    # visualize the result for each group in a faceted plot.
    facet_wrap(~variable) +
    geom_sf() +
    coord_sf(crs = 26915) + 
    scale_fill_viridis() +
    scale_color_viridis()
    
### --------------------------- where I've lived ---------------------------
    
# data for non-Hispanic whites, non-Hispanic blacks, non-Hispanic Asians, and
#   Hispanics by Census tract for the 2010 Census
racevars <- c("P0050003", "P0050004", "P0050006", "P0040003")

# list of counties and their location keys
counties <- list(riverside = list(state = "CA", county = "Riverside County"),
                 san_bernardino = list(state = "CA", county = "San Bernardino County"),
                 skagit = list(state = "WA", county = "Skagit County"),
                 philadelphia = list(state = "PA", county = "Philadelphia County"),
                 king = list(state = "WA", county = "King County"),
                 gallatin = list(state = "MT", county = "Gallatin County")
)

# prepare list of subtitles
county_list <- c("Riverside County, CA:", 
                 "San Bernardino County, CA:",
                 "Skagit County, WA:",
                 "Philadelphia County, PA:",
                 "King County, WA:",
                 "Gallatin County, MT:")
headlines <- sprintf("%s2010 U.S. Census", county_list)

# pull race data
get_census_dfs <- function(location) {
    # create df
    get_decennial(geography = "tract", 
                  variables = racevars, 
                  state = location$state, 
                  county = location$county, 
                  geometry = TRUE, 
                  summary_var = "P0010001") %>%
        mutate(pct = 100 * (value / summary_value),
               variable = fct_recode(variable,
                                     White = "P0050003",
                                     Black = "P0050004",
                                     Asian = "P0050006",
                                     Hispanic = "P0040003"),
               county_name = location$county) 
}

# map census data
map_census_data <- function(df) {
    df %>%
        dplyr::group_by(county_name) %>%
        ggplot(aes(fill = pct, color = pct)) +
        theme(text=element_text(family="Garamond", size=14)) +
        labs(title = "Population Percentage by Race",
             #subtitle = sprintf("%s, 2010 U.S. Census", df$county_name),
             y = "",
             x = "") +
        facet_wrap(~variable) +
        geom_sf() +
        coord_sf(crs = 26915) + 
        scale_fill_viridis() +
        scale_color_viridis(option = "magma")
}

# prepare list of dfs containing race data
dfs <- map(counties, get_census_dfs)
dfs <- do.call(rbind, dfs)
# convert county name to factor var
dfs$county_name <- as.factor(dfs$county_name)
str(dfs)
map_census_data(dfs)
# Vectorizing a function over multiple arguments
df <- data.frame(
    x = c("apple", "banana", "cherry"),
    pattern = c("p", "n", "h"),
    replacement = c("x", "f", "q"),
    stringsAsFactors = FALSE
)
pmap(df, gsub)
#> [[1]]
#> [1] "axxle"
#> 
#> [[2]]
#> [1] "bafafa"
#> 
#> [[3]]
#> [1] "cqerry"
#> 
pmap_chr(df, gsub)
#> [1] "axxle"  "bafafa" "cqerry"

p <- mapply(map_census_data, df = dfs, headline = headlines, SIMPLIFY = FALSE)
gridExtra::grid.arrange(grobs=p)


### population by race
df.riverside_county_race <- get_race_df("CA", "Riverside County")
df.sanbernardino_county_race <- get_race_df("CA", "San Bernardino County")
df.skagit_county_race <- get_race_df("WA", "Skagit County")
df.philadelphia_county_race <- get_race_df("PA", "Philadelphia County")
df.king_county_race <- get_race_df("WA", "King County")
df.gallatin_county_race <- get_race_df("MT", county = "Gallatin County")

# test pattern for proceeding list of dcs
ls(pattern = "_county_race")

# list of dataframes of county data by race
home_counties <- mget(ls(pattern = "_county_race"))
str(home_counties)



## Where I've lived: 2015 ACS 
# year not necessary here: default is 2015 (most recent year avail.)
# When a summary variable is specified in get_acs, both summary_est and summary_moe columns will be returned.


# When a summary variable is specified in get_acs, both summary_est and summary_moe columns will be returned.
riverside <- get_decennial(geography = "tract", variables = racevars, 
                           state = "CA", county = "Riverside County", geometry = TRUE,
                           summary_var = "P0010001") 
san_bernadino <- get_decennial(geography = "tract", variables = racevars, 
                               state = "CA", county = "San Bernadino County", geometry = TRUE,
                               summary_var = "P0010001") 
skagit <- get_decennial(geography = "tract", variables = racevars, 
                        state = "WA", county = "Skagit County", geometry = TRUE,
                        summary_var = "P0010001") 
philadelphia <- get_decennial(geography = "tract", variables = racevars, 
                              state = "PA", county = "Philadelphia County", geometry = TRUE,
                              summary_var = "P0010001") 
king <- get_decennial(geography = "tract", variables = racevars, 
                      state = "WA", county = "King County", geometry = TRUE,
                      summary_var = "P0010001") 
gallatin <- get_decennial(geography = "tract", variables = racevars, 
                          state = "MT", county = "Gallatin County", geometry = TRUE,
                          summary_var = "P0010001") 

?get_acs()


# _________________________

install.packages(c('dplyr','ggplot2','ggmap','sp','rgdal','rgeos','maptools','devtools'))
devtools::install_github('walkerke/acs14lite')
devtools::install_github('walkerke/tigris')
devtools::install_github('becarioprecario/cartodb-r/CartoDB', dep=TRUE)

library(sp) # for working with spatial data objects
library(rgdal) # for importing and exporting spatial data in various formats
library(acs14lite) # used to fetch ACS data
library(tigris) # used to fetch TIGER data (shapefiles)
library(dplyr) # used to reformat the ACS data
library(maptools) # used by ggplot and base maps
library(ggplot2) # used to make maps of the ACS data
library(ggmap) # for adding Google Maps data to our maps
library(CartoDB) # to create interactive maps in CartoDB.com


#Set the Census API key for the acs14lite library. 
#If you don't have an API key go to
#http://api.census.gov/data/key_signup.html to get one.
#my_census_api_key <- "your api key"
#set_api_key(my_census_api_key)

# explore B17021: Poverty Status of Individuals in the Past 12 months 
#   by Living Arrangement. This data is available for county, tract and block 
#   group aggregations in 2010-2014.
# B17021_001E: count of people for whom poverty status has been determined 
#     (the sample estimate)
# B17021_001M: count of people for whom poverty status has been determined 
#    (the margin of error)
#MB17021_002E: count of those people whose income in the past 12 months is below
#    poverty (estimate)
# B17021_002M: count of those people whose income in the past 12 months is below 
#    poverty (margin of error)

sf_poverty <- acs14(geography = 'tract', state = 'CA', county = 'San Francisco', 
                    variable = c('B17021_001E', 'B17021_001M', 'B17021_002E', 'B17021_002M'))

head(sf_poverty) # view retrieved data

# use the dplyr mutate and select functions to convert the counts to percents 
#    and create a simple data frame with those values
#    acs14lite function moe_prop to calculate the margin of error for each percentage
sf_poverty14 <- mutate(sf_poverty,
                       geoid = paste0(state, county, tract),
                       pctpov = round(100 * (B17021_002E / B17021_001E), 1),
                       moepov = round(100 * (moe_prop(B17021_002E, B17021_001E, B17021_002M, B17021_001M)),1))
sf_poverty14 <- select(sf_poverty14, geoid, pctpov, moepov)

head(sf_poverty14) # take a look at the retieved and reformatted ACS data


#To create a map of the data use the tigris package to download TIGER geographic 
#   data in the form of ESRI shapefiles. Use ??tigris in R for details about the 
#   package and availalable  functions and options. By default tigris downloads
#   the 2014 TIGER data. The tigris functions for retrieving data are names after
#   the types of data that they retrieve, e.g., tracts() or block_groups().
# 
#  cb=TRUE option will retrieve the more generalized TIGER data which will save 
#    time and memory. It's a good idea unless you know you need the more detailed 
#    data.  The sf_tracts data object is of class SpatialPolygonsDataFrame. 
#    For details see ?"SpatialPolygonsDataFrame-class"
sf_tracts <- tracts('CA', 'San Francisco', cb=TRUE)


# Use the acs14lite function geo_join to join the ACS data (sf_poverty14) to the 
# tracts spatial data (sf_tracts)
sf_tracts2 <- geo_join(sf_tracts, sf_poverty14, "GEOID", "geoid")
#  Then remove any tracts with no data for the ACS variable. We do this for SF 
#mainly because the Farralon Islands are so far off the coast of SF that they
# mess up the map of the data.
sf_tracts2 <- sf_tracts2[!is.na(sf_tracts2$pctpov),]
# look at the data
class(sf_tracts2)
str(sf_tracts2)
str(sf_tracts2@data)

# We now have a spatial data object that  we can map ACS data with! Below is
#    some code to do this with ggplot2 and ggmap.
# First use fortify() to make the spatial data object a data frame that ggplot can map.
ggplotData <- fortify(sf_tracts2, data=sf_tracts2@data, region="geoid")
head(ggplotData) # look at the data frame created with the fortify function

# Join the ACS data to the fortified data frame
ggplotData <- merge(ggplotData, sf_tracts2@data, by.x="id", by.y="geoid")
head(ggplotData) # look at the data

# Plot the data to emphasize the areas of highest poverty
# First use the ggmap get_map function to fetch a Google Map image to use as our basemap
sf_basemap <-get_map('San Francisco', zoom=12) 
ggmap(sf_basemap) +
    geom_polygon(data = ggplotData, aes(x = long, y = lat, group = group, fill = pctpov), alpha=0.75) +
    scale_fill_distiller(palette = "Reds") +
    guides(fill = guide_legend(reverse = TRUE)) +
    ggtitle("Percent of Individuals below Poverty Level\n ACS 2010-2014 Data") +
    theme_nothing(legend=TRUE) +
    coord_map()


#################---------------With Choroplethr








