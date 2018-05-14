# population estimates data by race and ethnicity for zip code tabulated areas for 2013

library(acs)
library(devtools)
library(dplyr)
# package hosted on github bc of its size
install_github('arilamstein/choroplethrZip@v1.4.0') 
library(choroplethrZip)
data(df_zip_demographics)
summary(df_zip_demographics$total_population)

# for eace col in the df
for (i in 2:ncol(df_zip_demographics)) {
    # set val and title
    df_zip_demographics$value = df_zip_demographics[,i]
    title = paste0("2013 ZCTA Demographics:\n",
                   colnames(df_zip_demographics[i]))
    
    # print the map
    choro = zip_choropleth(df_zip_demographics, title = title)
    print(choro)
}

co


zip_geo <- geo.make(zip.code = "*")
# B03002 - Hipanic or Latino Origin by Race. 
race.data <- acs.fetch(geography = zip_geo, table.number = "B03002", 
                       col.names = "pretty", endyear = 2013, span = 5)

# convert to df
df.race <- data.frame(region = as.character(geography(race.data)$zipcodetabulationarea),
                      total_pop = as.numeric(estimate(race_data[,1])),
                      white_not_hispanic = as.numeric(estimate(race.data[,3])),
                      black_not_hispanic = as.numeric(estimate(race.data[,4])),
                      asian_not_hispanic = as.numeric(estimate(race.data[,6])),
                      hispanic_all_races = as.numeric(estimate(race.data[,12])),
                      white_hispanic = as.numeric(estimate(race_data[,13])),
                      black_hispanic = as.numeric(estimate(race_data[,14])),
                      some_other_race_only_hispanic = as.numeric(estimate(race_data[,18])),
                      two_plus_races_hispanic = as.numeric(estimate(race_data[,19]))
                      )

head(df.race)
str(df.race)
df.race$region = as.character(df.race$region) # no idea why, but it's a factor before this line
str(df.race)

df.race <- tbl_df(df.race)
percent_cols <- function(!! col) {
    round(df.race$col / df.race$total_pop * 100)
}

# add percent cols
df.race <- mutate(df.race, percent_white=percent_cols(white_not_hispanic))
str(df.race)

