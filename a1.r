library("tidyverse")
library("janitor")

# loading in the tree census dataframe
trees <- read_csv("2015_Street_Tree_Census_-_Tree_Data_20241001.csv")  %>%
  clean_names()  %>%
  select(tree_id, block_id, tree_dbh, spc_common, status, health,
         postcode, borough, latitude, longitude, borocode)
head(trees)

# grouping tree counts by zip code and borough, then getting count of all street
# trees in the zip code
zip_counts <- trees  %>% 
  group_by(postcode, borough)  %>%
  summarize(num_trees = n())  %>% 
  rename(zip = postcode)
head(zip_counts)

# creating a new column for quick stats at the boro level
boro_counts <- trees  %>%
  group_by(borough)  %>%
  summarize(num_trees = n())  %>%
  mutate(boro_pop = case_when(
    borough == "Bronx" ~ 1472654,
    borough == "Brooklyn" ~ 2736074,
    borough == "Manhattan" ~ 1694251,
    borough == "Queens" ~ 2405464,
    borough == "Staten Island" ~ 495747
  ),
  boro_area = case_when(
      borough == "Bronx" ~ 42.2,
      borough == "Brooklyn" ~ 69.4,
      borough == "Manhattan" ~ 22.7,
      borough == "Queens" ~ 108.7,
      borough == "Staten Island" ~ 57.5
    ),
  trees_per_capita = num_trees / boro_pop,
  trees_per_sq_mi = num_trees / boro_area
  )

boro_counts
boro_counts %>% write_csv(file="boro_counts.csv")

# making a couple of boro-level graphs
# These didn't make it into the final assignment since I wanted to
# use datawrapper for nicer looking plots 
tree_coverage_bar <- ggplot(boro_counts) +
  aes(x = borough, y = trees_per_sq_mi, fill = borough) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Street Trees Per Sq Mi in NYC Boroughs",
       x = "Borough",
       y = "Street Trees Per Sq Mi",
       fill = "Borough")

tree_coverage_bar

trees_per_capita_bar <- ggplot(boro_counts) +
  aes(x = borough, y = trees_per_capita, fill = borough) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Street Trees Per Capita in NYC Boroughs",
       x = "Borough",
       y = "Street Trees Per Capita",
       fill = "Borough")

trees_per_capita_bar

# bringing ACS income data into the fold
income_data <- read_csv("ACSST5Y2015.S1901_2024-10-09T153412/ACSST5Y2015.S1901-Data.csv")  %>%
  clean_names()  %>%
  select(name, s1901_c01_012e, s1901_c01_012m) %>% # just selecting median household income and margin of error
  rename(median_income = s1901_c01_012e, income_margin = s1901_c01_012m)  %>%
  mutate(zip = substring(name, 7, nchar(name)))  %>% # dropping the "ZCTA5 " part of the zipcode field 
  filter(nchar(zip) == 5)  %>%  # filtering out the first two rows that give stats about NYC metro and extra column names (don't need at this point) 
  select(zip, median_income, income_margin) # get rid of name now that we've gotten what we need out of it

# converting the data types of the columns to numbers so it meshes with zip_counts and graphs easily
income_data$zip = as.numeric(income_data$zip)
income_data$median_income = as.numeric(income_data$median_income)
income_data$income_margin = as.numeric(income_data$income_margin)
#income_data <- income_data  %>% column_to_rownames(var = "zip")
head(income_data)

# joining income data into the zip_counts data frame
zip_trees_income <- left_join(x = zip_counts, y = income_data, by = "zip")  %>% 
  filter(zip != 83) # central park is zip 00083, lack of income data messes things up

# reading in the dataset mapping zipcodes to their land area
zcta_areas <- read_csv("nanda_landcover_zcta_2001-2016_01P.csv")  %>% filter(year_intp == 2015) %>% select(zcta19, zcta_area)  %>% rename(zip=zcta19)
zcta_areas$zip = as.numeric(zcta_areas$zip)
head(zcta_areas)

# joining zcta data set into the data frame containing tree and income info
full_zip_data <-
  left_join(zip_trees_income, zcta_areas, by = "zip") %>%
  mutate(trees_per_sq_mi = num_trees / (zcta_area * 0.0000003861)) # zcta area is in sq meters, so we multiply by that to get square miles 

head(full_zip_data)

# making a scatter plot of street trees per sq mi vs. median zcta income
ggplot(full_zip_data, aes(x = median_income, y = trees_per_sq_mi, color = borough)) + geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Street Tree Density vs. Median ZCTA Income in NYC",
    x = "Median Zip Code Income (USD)",
    y = "Trees Per Sq Mi",
    color = "Borough")

# reading in hvi rankings, joining that into the rest of the data
heat_rankings <- read_csv("Heat_Vulnerability_Index_Rankings_20241014.csv")  %>%
  clean_names() %>%
  rename(zip = zip_code_tabulation_area_zcta_2020, hvi = heat_vulnerability_index_hvi )

zip_heat <- left_join(full_zip_data, heat_rankings, by = "zip")

# plotting the street tree density vs. hvi
ggplot(zip_heat, aes(x = trees_per_sq_mi, y = hvi, color = borough)) + geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Street Tree Density vs. Heat Vulnerability Index",
    y = "Heat Vulnerability Index",
    x = "Trees Per Sq Mi",
    color = "Borough")

# exporting out the zip_heat dataframe to put the data in datawrapper
# to make nicer looking charts 
zip_heat %>% write_csv("zip_heat.csv")

# moving on to the map plot to visualize trees per capita by zcta
# install.packages("rjson")
# run the line above only if rjson is not already installed

library(plotly)
library(rjson)

zipcode_json <- fromJSON(file="nyc-zip-code-tabulation-areas-polygons.geojson")
g <- list(

  fitbounds = "locations",

  visible = FALSE

)
fig <- plot_ly()

fig <- fig  %>% add_trace(
  type = "choropleth",
  geojson = zipcode_json,
  locations = full_zip_data$zip,
  z = full_zip_data$trees_per_sq_mi,
  featureidkey="properties.postalCode"
)

fig <- fig %>% layout(geo = g, title = "Street Trees per Square Mile in NYC Zip Codes")

fig

