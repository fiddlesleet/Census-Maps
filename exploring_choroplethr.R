
######## TODO: FIX ACS FUNCTIONS PROCESSIONG, FIND OUT IF FUNCTIONS GO
######## B4 OR AFTER R CODE, BREAK INTO MODULES, MAYBE TURN FUNCS INTO LIBRARY,
######## R MARKUP THIS, POST TO GIT HUB



# NOTE: "A choropleth is any map that shows borders (such as states), 
# and expresses values for the regions with color"

# install and load the packages
# install.packages(c("choroplethr", "choroplethrMaps"))
library(choroplethr)
library(choroplethrMaps)
library(acs)
library(gridExtra)
library(microbenchmark)

##########
# GET DATA
##########

# state population data from choroplethr 
data(df_pop_state)
tail(df_pop_state)

# acs data
df_2010_demographics = get_state_demographics(endyear = 2010)
df_2011_demographics = get_state_demographics(2011)

?get_state_demographics
## county population data
# choroplethr data
data(df_pop_county)

### Custom Functions
## Optimization Note: passing a local copy of the dataframe to these functions tested
## significantly faster (using the microbenchmark package) than having the 
## functions access the global dataframe

# choropleth of states with population under max_pop people
map_pop_less_than <- function(max_pop, legend_n) { 
    data(df_pop_state)
    df_pop_state$str = ""
    for (i in 1:nrow(df_pop_state))
    {
        if (df_pop_state[i,"value"] < max_pop)
        {
            df_pop_state[i,"str"] = paste("<", legend_n, sep = " ") 
        } else {
            df_pop_state[i,"str"] = paste(">", legend_n, sep = " ") 
        }
    }
    df_pop_state$value = df_pop_state$str
    df_pop_state
}

### ACS analysis 
## Generate regional analysis using acs data 
acs_demog <- function(region = NULL) {
    c(income = acs_income(region), race = acs_race(region))
}
acs_income <- function(region = NULL) {
    c(median_rent = acs_median_rent(region), 
      per_capita = acs_per_capita(region))
}
acs_race <- function(region = NULL) {
    c(percent_black = acs_percent_black(region), 
      percent_white = acs_percent_white(region),
      percent_hispanic = acs_percent_hispanic(region))
}

## Income 
# median rent
acs_median_rent <- function(region = NULL) {
    df_2011_demographics$value = df_2011_demographics$median_rent
    map_continuous_color_state("2011 State Median Rent Estimates",
                               df_2011_demographics, zoom = region)
}

# per capita income
acs_per_capita <- function(region = NULL) {
    df_2011_demographics$value = df_2011_demographics$per_capita_income
    map_continuous_color_state("2011 State Per Capita Income Estimates",
                               df_2011_demographics, zoom = region)
    map_quartiles_state("2011 State Per Capita Income Estimates",
                        df_2011_demographics, zoom = region)
}

## Race
# Black population
acs_percent_black <- function(region = NULL) {
    title = "2011 State Black Population Estimates"
    df_2011_demographics$value = df_2011_demographics$percent_black
    map_quartiles_state(title, legend_label = "Quartiles (%)",
                        df = df_2011_demographics, zoom = region)
    map_continuous_color_state(title, df = df_2011_demographics, zoom = region)
}

# Hispanic population
acs_percent_hispanic <- function(region = NULL) {
    title = "2011 State Hispanic Population Estimates"
    df_2011_demographics$value = df_2011_demographics$percent_hispanic
    map_quartiles_state(title, legend_label = "Quartiles (%)",
                        df = df_2011_demographics, zoom = region)
    map_continuous_color_state(title, df = df_2011_demographics, 
                               zoom = region)
}

# White population 
acs_percent_white <- function(region) {
    title = "2011 State White Population Estimates"
    df_2011_demographics$value = df_2011_demographics$percent_white
    map_quartiles_state(title, legend_label = "Quartiles (%)",
                        df = df_2011_demographics, zoom = region)
    map_continuous_color_state("2011 State White Population Estimates", 
                               df = df_2011_demographics, zoom = region)
}
    
## Automated mapping 
# baseline choropleth of United States 
baseline_map_state <- function(map_title, df = df_pop_state,
                               legend_label = "Population",
                               zoom_zone = NULL, ref_map = FALSE) {
    state_choropleth(df, 
                     title = map_title, 
                     legend = legend_label,
                     zoom = zoom_zone,
                     reference_map = ref_map)
}

baseline_map_county <- function( map_title, df=df_pop_county,
                                legend_label = "Population",
                                zoom_zone_state = NULL,
                                zoom_zone_county = NULL, ref_map = FALSE) {
    county_choropleth(df, 
                     title = map_title, 
                     legend = legend_label,
                     state_zoom = zoom_zone_state,
                     county_zoom = zoom_zone_county,
                     reference_map = ref_map)
}

# choropleth of US, with continuous scale coloring to visualize outliers   
map_continuous_color_state <- function(map_title, df=df_pop_state, 
                                       legend_label = "Population",
                                       zoom_zone = NULL, ref_map = FALSE) {
    state_choropleth(df, 
                     title = map_title, 
                     legend = legend_label, 
                     num_colors = 1,
                     zoom = zoom_zone,
                     reference_map = ref_map)
}

map_continuous_color_county <- function(map_title, df=df_pop_county,
                                        legend_label = "Population",
                                        zoom_zone_state = NULL,
                                        zoom_zone_county = NULL,
                                        ref_map = FALSE) {
    county_choropleth(df, 
                     title = map_title, 
                     legend = legend_label, 
                     num_colors = 1,
                     state_zoom = zoom_zone_state,
                     county_zoom = zoom_zone_county,
                     reference_map = ref_map)
}

# choropleth of US States, colored by population quartiles 
map_quartiles_state <- function(map_title, df=df_pop_state,  
                                legend_label="Quartiles",
                                zoom_zone = NULL,
                                ref_map = FALSE) {
    state_choropleth(df, 
                     title = map_title, 
                     legend = legend_label, 
                     num_colors = 4,
                     zoom = zoom_zone,
                     reference_map = ref_map)
}

map_quartiles_county <- function(map_title, df=df_pop_county, 
                                 legend_label="Quartiles",
                                 zoom_zone_state = NULL,
                                 zoom_zone_county = NULL,
                                 ref_map = FALSE) {
    county_choropleth(df, 
                     title = map_title, 
                     legend = legend_label, 
                     num_colors = 4,
                     state_zoom = zoom_zone_state,
                     county_zoom = zoom_zone_county,
                     reference_map = ref_map)
}

### **************** Whole US ****************

## General population comparisons 

# map settings
state_map_title = "United States by Population (2012)"
county_map_title = "US County Population Estimates (2012)"
state_legend_label = "Population"
county_legend_label = "Population"

## mapping

# baseline choropleth map of US States 
baseline_map_state(state_map_title)
baseline_map_county(county_map_title)

# baseline choropleth map of US States, with continuous scale coloring 
# to visualize outliers             
map_continuous_color_state(state_map_title)
map_continuous_color_county(county_map_title)


# choropleth colored by population quartiles 
map_quartiles_state(state_map_title)
map_quartiles_county(county_map_title)

## ACS data
acs
# choropleth map of US States population in 2010  
df_2010_demographics$value = df_2010_demographics$total_population
baseline_map_state("2010 State Population Estimates", df_2010_demographics)

# choropleth map of US States population in 2011
df_2011_demographics$value = df_2011_demographics$total_population
baseline_map_state("2011 State Population Estimates", df_2011_demographics)

## Hispanic population comparisons 

# choropleth map of US States Hispanic population in 2010, with
# continuous scale coloring to visualize outliers      
df_2010_demographics$value = df_2010_demographics$percent_hispanic
map_continuous_color_state("2010 State Hispanic Population Estimates",
                           df_2010_demographics)

# choropleth map of US States Black population in 2011, with
# continuous scale coloring to visualize outliers     
df_2011_demographics$value = df_2011_demographics$percent_black
map_continuous_color_state("2011 State Black Population Estimates",
                           df_2011_demographics)

# Which states have less than 750K people? Plot. 
max_pop <- 750000
legend_n <- "750K"
state_choropleth(pop_less_than(max_pop, legend_n), 
                           title = paste("Which states have less than", 
                                         legend_n, "people?", sep = " ")
                )

# Which states have less than 2M people? Plot. 
max_pop <- 2000000
legend_n <- "2M"
state_choropleth(pop_less_than(max_pop, legend_n), 
                 title = paste("Which states have less than", 
                               legend_n, "people?", sep = " " )
                )

# Which states have less than 10M people? Plot. 
max_pop <- 10000000
legend_n <- "10M"
state_choropleth(pop_less_than(max_pop, legend_n), 
                 title = paste("Which states have less than", 
                               legend_n, "people?", sep = " ")
)

acs_regional_demog()
### **************** Regional US maps ****************

## The South
us_south <- c("texas", "louisiana", "alabama", "arkansas",
              "oklahoma", "mississippi", "georgia", "florida",
              "tennessee", "kentucky", "south carolina",
              "north carolina", "virginia")

# map settings
state_map_title = "U.S. South by Population"
county_map_title = "U.S. South by County Population Estimates (2012)"
state_legend_label = "Population"
county_legend_label = "Population"

## mapping

# choropleth map of the South
baseline_map_state(state_map_title, zoom = us_south)
baseline_map_county(county_map_title, zoom_zone_state = us_south)

# choropleth map of the South, with
# continuous scale coloring to visualize outliers
map_continuous_color_state(state_map_title, zoom = us_south)
map_continuous_color_county(county_map_title, zoom_zone_state = us_south)

# choropleth map of the South, quartile coloring, with ref. map
map_quartiles_state(state_map_title, zoom = us_south, ref_map = TRUE)
# by county, with reference map
map_quartiles_county(county_map_title, zoom_zone_state = us_south, 
                     ref_map = TRUE)
# choropleth map of the South, quartile coloring, without ref. map
map_quartiles_state(state_map_title, zoom = us_south)
# by county, without reference map
map_quartiles_county(county_map_title, zoom_zone_state = us_south)

## ACS data
acs_race(us_south)
# Black population
sapply(us_south, acs_income)



df_2011_demographics$value = df_2011_demographics$percent_black
map_quartiles_state("2011 State Black Population Estimates", 
                    legend_label = "Quartiles (%)",
                    df = df_2011_demographics, zoom = us_south)
map_continuous_color_state("2011 State Black Population Estimates", 
                    df = df_2011_demographics, zoom = us_south)
# median rent
df_2011_demographics$value = df_2011_demographics$median_rent
map_continuous_color_state("2011 State Median Rent Estimates",
                           df_2011_demographics, zoom = us_south)
# per capita income
df_2011_demographics$value = df_2011_demographics$per_capita_income
map_continuous_color_state("2011 State Per Capita Income Estimates",
                           df_2011_demographics, zoom = us_south)
map_quartiles_state("2011 State Per Capita Income Estimates",
                           df_2011_demographics, zoom = us_south)
# Hispanic population comparisons 
df_2011_demographics$value = df_2011_demographics$percent_hispanic
map_quartiles_state("2011 State Hispanic Population Estimates", 
                    legend_label = "Quartiles (%)",
                    df = df_2011_demographics, zoom = us_south)
map_continuous_color_state("2011 State Hispanic Population Estimates", 
                           df = df_2011_demographics, zoom = us_south)


## The West
us_west <- c("washington", "oregon", "california", "idaho", "montana", 
             "wyoming", "colorado", "nevada", "arizona", "utah", "new mexico")

# map settings
state_map_title = "Western U.S. by Population"
county_map_title = "Western U.S. by County Population Estimates (2012)"
state_legend_label = "Population"
county_legend_label = "Population"

## mapping

# choropleth map of the West
baseline_map_state(state_map_title, zoom = us_west)
baseline_map_county(county_map_title, zoom_zone_state = us_west)

# choropleth map of the West, quartile
map_quartiles_state(state_map_title, zoom = us_west)
# noting the 4Q range, not terribly useful:
map_quartiles_county(county_map_title, zoom_zone_state = us_west)

# choropleth map of the West, continuous scale coloring to visualize
# outliers, with ref. map
map_continuous_color_state(state_map_title, zoom = us_west)
map_continuous_color_county(county_map_title, zoom = us_west)

# population by quintile
state_choropleth(df_pop_state, 
                 title = state_map_title,
                 legend = "Quintile range",
                 num_colors = 5, # color by quintile
                 zoom = us_west,
                 reference_map = TRUE
)

# by county, with reference map
baseline_map_county(county_map_title, zoom_zone_state = us_west, 
                    ref_map = TRUE)
# by county, without reference map
baseline_map_county(county_map_title, zoom_zone_state = us_west)

## ACS data
# Black population
df_2011_demographics$value = df_2011_demographics$percent_black
map_quartiles_state("2011 State Black Population Estimates", 
                    legend_label = "Quartiles (%)",
                    df = df_2011_demographics, zoom = us_south)
map_continuous_color_state("2011 State Black Population Estimates", 
                           df = df_2011_demographics, zoom = us_south)
# median rent
df_2011_demographics$value = df_2011_demographics$median_rent
map_continuous_color_state("2011 State Median Rent Estimates",
                           df_2011_demographics, zoom = us_south)
# per capita income
df_2011_demographics$value = df_2011_demographics$per_capita_income
map_continuous_color_state("2011 State Per Capita Income Estimates",
                           df_2011_demographics, zoom = us_south)
map_quartiles_state("2011 State Per Capita Income Estimates",
                    df_2011_demographics, zoom = us_south)
# Hispanic population comparisons 
df_2011_demographics$value = df_2011_demographics$percent_hispanic
map_quartiles_state("2011 State Hispanic Population Estimates", 
                    legend_label = "Quartiles (%)",
                    df = df_2011_demographics, zoom = us_south)
map_continuous_color_state("2011 State Hispanic Population Estimates", 
                           df = df_2011_demographics, zoom = us_south)


### **************** State Maps ****************

## generate state maps


## **************** Washington State by county ****************

# vector of county fips codes 
FINAL_FIPS <- 77 # state's last FIPS county code
county_fips <- seq(1, FINAL_FIPS, 2) # vector of all state's county FIPS codes
STATE_FIPS <- 53 # state's state-level FIPS code
regex <- paste0(STATE_FIPS, "%03d")
county_fips <- sprintf(regex, county_fips) 

# map settings
county_map_title = "Where Washington Lives, by County (2012)"
county_legend_label = "Population"

# generate state maps
map_continuous_color_county(county_map_title, zoom_zone_county = county_fips)
map_quartiles_county(county_map_title, zoom_zone_county = county_fips)
baseline_map_county(county_map_title, 
                    zoom_zone_county = county_fips, 
                    ref_map = TRUE)

## **************** Pennsylvania by county ****************

FINAL_FIPS <- 133 # state's last FIPS county code
county_fips <- seq(1, FINAL_FIPS, 2) # vector of all state's county FIPS codes
STATE_FIPS <- 42 # state's state-level FIPS code
regex <- paste0(STATE_FIPS, "%03d")
county_fips <- sprintf(regex, county_fips) 

# map settings
county_map_title = "Where Pennsylvania Lives, by County (2012)"
county_legend_label = "Population"

# generate state maps
map_continuous_color_county(county_map_title, zoom_zone_county = county_fips)
map_quartiles_county(county_map_title, zoom_zone_county = county_fips)
baseline_map_county(county_map_title, 
                    zoom_zone_county = county_fips, 
                    ref_map = TRUE)

## **************** Alabama by county ****************
FINAL_FIPS <- 133 # state's last FIPS county code
county_fips <- seq(1, FINAL_FIPS, 2) # vector of all state's county FIPS codes
STATE_FIPS <- 01 # state's state-level FIPS code
regex <- paste0(STATE_FIPS, "%03d")
county_fips <- sprintf(regex, county_fips) 

# map settings
county_map_title = "Where Alabama Lives, by County (2012)"
county_legend_label = "Population"

# generate state maps
map_continuous_color_county(county_map_title, zoom_zone_county = county_fips)
map_quartiles_county(county_map_title, zoom_zone_county = county_fips)
baseline_map_county(county_map_title, 
                    zoom_zone_county = county_fips, 
                    ref_map = TRUE)

## **************** California by county ****************
FINAL_FIPS <- 115 # state's last FIPS county code
county_fips <- seq(1, FINAL_FIPS, 2) # vector of all state's county FIPS codes
STATE_FIPS <- 06 # state's state-level FIPS code
regex <- paste0(STATE_FIPS, "%03d")
county_fips <- sprintf(regex, county_fips) 

# map settings
county_map_title = "Where California Lives, by County (2012)"
county_legend_label = "Population"

# generate state maps
map_continuous_color_county(county_map_title, zoom_zone_county = county_fips)
map_quartiles_county(county_map_title, zoom_zone_county = county_fips)
baseline_map_county(county_map_title, 
                    zoom_zone_county = county_fips, 
                    ref_map = TRUE)


### **************** County Cluster Maps ****************
## zooming in on Western Washington counties
nw_wa_counties <- c("Island", "King", "San Juan", "Skagit", 
                    "Snohomish", "Whatcom")
# nw_wa_counties' corresponding fips codes, in order:
county_fips <- c("53029", "53033", "53055", "53057", "53061", "53073")

# map settings
county_map_title = "Where the Upper Left Lives, by County (2012)"
county_legend_label = "Population"

# generate state maps
map_continuous_color_county(county_map_title, zoom_zone_county = county_fips)
map_quartiles_county(county_map_title, zoom_zone_county = county_fips)
baseline_map_county(county_map_title, 
                    zoom_zone_county = county_fips, 
                    ref_map = TRUE)


 ?state_choropleth
county_choropleth(df_pop_state)
zip_map(df_pop_state)
country_choropleth()
?county.regions