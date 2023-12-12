#Load the necessary packages
library(dplyr)
library(rnaturalearth)
library(sf)

#load the data
trace <- read.csv('../data/trace_data_since_2015_to_2021_combined_false.csv')
gapminder <- read.csv('../data/gapminder_pop_v7.csv')

#this is used to add some additional information to the dataset (such as full country names)
world_ne <- rnaturalearth::ne_download(
  scale = 110,
  type = "countries",
  category = "cultural",
  returnclass = "sf"
)

world_ne <- st_transform(world_ne, crs = "+proj=robin")

world <-  world_ne %>%
  select(c('ISO_A3','SOVEREIGNT'))

#merge the Climate trace dataset with the n.earth data
chart_data <-  trace %>%
  full_join(
    world,
    .,
    by = c('ISO_A3' = 'region'))
class(chart_data)

map_data <-  chart_data %>%
  filter(!is.na(SOVEREIGNT))

class(map_data)


# Convert the sf object to a standard dataframe
map_data_df <- as.data.frame(map_data)
map_data_no_geom <- map_data_df %>% 
  select(-geometry)

#recalculate emissions to million ton
map_data_no_geom$co2 <- map_data_no_geom$co2 / 1000000
map_data_no_geom$ch4 <- map_data_no_geom$ch4 / 1000000
map_data_no_geom$n20 <- map_data_no_geom$n20 / 1000000

map_data_no_geom <- map_data_no_geom %>%
  group_by(SOVEREIGNT, year, `sector.subsector`) %>%
  summarize(
    co2 = sum(co2, na.rm = TRUE),
    ch4 = sum(ch4, na.rm = TRUE),
    n20 = sum(n20, na.rm = TRUE),
  ) %>%
  ungroup()

# Write the dataframe without the geometry column to a CSV file
write.csv(map_data_no_geom, "../data/chart_data.csv", row.names = FALSE)


#Some further dataprocessing is needed so reload the file for that purpose
climate_trace <- read.csv('../data/chart_data.csv')
climate_trace <- climate_trace %>%
  rename(country = SOVEREIGNT)


name_changes <- c("United States" = "United States of America",
                   "Slovak Republic" = "Slovakia",
                   "Czech Republic" = "Czechia",
                   "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                   "Timor-Leste" = "East Timor",
                   "Cote d'Ivoire" = "Ivory Coast",
                   "Kyrgyz Republic" = "Kyrgyzstan",
                   "Lao" = "Laos",
                   "Macedonia, FYR" = "North Macedonia",
                   "Serbia" = "Republic of Serbia",
                   "Congo, Rep." = "Republic of the Congo",
                   "Bahamas" = "The Bahamas",
                   "Tanzania" = "United Republic of Tanzania",
                   "Swaziland" = "eSwatini"
)

gapminder <- gapminder %>%
  mutate(name = recode(name, !!!name_changes))
           

#Merge the two datasets
## First we need to ensure we have the same datatype for matching:
gapminder$time <- as.integer(gapminder$time)
##Then we can merge
df <-  inner_join(climate_trace, gapminder, by = c("country" = "name", "year" = "time"))

df_missing <-  full_join(climate_trace, gapminder, by = c("country" = "name", "year" = "time"))
#check how well the merge was performed (always check after merge! like this, or by comparing shapes or number of unique values)
missingness <- df_missing %>%
  filter(is.na(Population)) %>%
  select(country)
missingness <- as.list(unique(missingness))
print(missingness)


#Let's do a final prep: rename columns for dynamic displays, and export file again
#This is the final data for all charts except the map view
chart_data <- df %>%
  mutate(Population = as.numeric(Population)) %>%
  rename(
         Carbon_Dioxide = co2,
         Methane = ch4,
         Nitrous_oxide = n20
  ) %>%
  mutate(Carbon_Dioxide = round(Carbon_Dioxide, 2),
         Methane = round(Methane, 2),
         Nitrous_oxide = round(Nitrous_oxide, 2))
write.csv(chart_data, "../data/chart_data.csv", row.names = FALSE)


#Let's add a geometry to be able to visualize on a world map, calculate per capita emissions, and export
#This is the final data for the overview world map
gapminder_2021 <- gapminder %>%
  filter(time == 2021)

filtered_data <- chart_data %>%
  filter(year==2021) %>%
  group_by(country) %>%
  summarize(Carbon_Dioxide = sum(Carbon_Dioxide, na.rm = TRUE)) %>%
  left_join(gapminder_2021, by = c("country" = "name")) %>%
  mutate(Population = as.numeric(Population)) %>%
  mutate(CO2_PerCapita = Carbon_Dioxide * 1000000 / Population)


world_ne <- rnaturalearth::ne_download(
  scale = 110,
  type = "countries",
  category = "cultural",
  returnclass = "sf"
)
world_ne <- st_transform(world_ne, crs = "+proj=robin")
map_data <- filtered_data %>%
  full_join(
    world_ne,
    .,
    by = c('SOVEREIGNT' = 'country')
  )

map_data2 <- map_data %>%
  select(c(SOVEREIGNT,geometry, Carbon_Dioxide, CO2_PerCapita, Population))

#For some reason, the Climate Trace database assumes very high emissions for the Bahamas and Panama. I will
#manually rewrite these values to the ones published by the World Bank. These might have been misclassified from 
#nearby countries (e.g., from the US in case of the Bahamas), and these per capita values are so large they render the rest 
#of the map a uniform color.

#Similarly, let's take the log of emissions for the color map (actual values will be allocated to hover)

map_data2 <- map_data2 %>%
  mutate(log_emissions = ifelse(Carbon_Dioxide > 0, log(Carbon_Dioxide), NA)) %>%
  mutate(CO2_PerCapita = case_when(
    SOVEREIGNT == "The Bahamas" ~ 7.02,
    SOVEREIGNT == "Panama" ~ 3.5,
    TRUE ~ CO2_PerCapita
  ))

saveRDS(map_data2, '../data/map_data.rds')



