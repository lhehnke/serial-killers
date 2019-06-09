##------------------------------------------------------------------------------------------##
##                     SERIAL KILLERS: DATA SCRAPING AND ANALYSIS                           ##
##------------------------------------------------------------------------------------------##


## R version 3.3.1 (2016-06-21)

## Data source: https://de.wikipedia.org/wiki/Liste_von_Serienmördern

## Note: Killers' pseudonyms are partially in German in the final data set. 


#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(dplyr, geosphere, ggplot2, jsonlite, leaflet, maptools, raster, reshape2, rvest, stringr, tidyr)


#----------------------------#
# Scrape data from Wikipedia #
#----------------------------#

# Scrape wikipedia tables on male and female serial killers
url <- "https://de.wikipedia.org/wiki/Liste_von_Serienmördern"

male <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[1]") %>% 
  html_table(fill = TRUE)
male <- data.frame(male)

female <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[2]") %>% 
  html_table(fill = TRUE)
female <- data.frame(female)


#----------------#
# Data wrangling #
#----------------#

# Add column for sex
male$sex <- "male"
female$sex <- "female"

# Merge data frames
killers <- rbind(male, female)

# Rename columns
names(killers) <- c("name", "land", "pseudonym", "victims_proven", "victims_suspected", "years_active", "sex")

# Strip name from [reference number]
killers %<>% 
  mutate(name = gsub("\\[.*?\\]", "", name))

# Scrape country names
url <- "https://de.wikipedia.org/wiki/Liste_der_Staaten_der_Erde"

countries <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[1]") %>% 
  html_table(fill = TRUE)
countries <- data.frame(countries)

# Subset country df
countries <- countries[-c(1:6), c(1, 11)]

# Rename columns
names(countries) <- c("land", "country")

# Strip land (= country in German) from [reference number]
countries %<>% mutate(land = gsub("\\[.*?\\]", "", land))

# Manually rename land to match land names in killers df
countries[33, "land"] = "China"
countries[36, "land"] = "Dänemark"
countries[48, "land"] = "Frankreich"
countries[61, "land"] = "Indien"
countries[67, "land"] = "Israel"
countries[86, "land"] = "Nordkorea"
countries[87, "land"] = "Südkorea"
countries[107, "land"] = "Marokko"
countries[125, "land"] = "Niederlande"
countries[130, "land"] = "Norwegen"
countries[134, "land"] = "Pakistan"
countries[146, "land"] = "Russland"
countries[185, "land"] = "Tschechische Republik"
countries[199, "land"] = "USA"
countries[200, "land"] = "Großbritannien"

# Manually rename country
countries[86, "country"] = "North Korea"
countries[87, "country"] = "South Korea"
countries[146, "country"] = "Russia"

# Merge country df with killers df
## Note: Locations "Australien / USA" and "Sowjetunion / Ukraine" are coerced to NAs.
killers <- left_join(killers, countries, by = "land")

# Manually rename countries
killers$country[killers$land == "Jugoslawien"] <- "Yugoslavia"
killers$country[killers$land == "Sowjetunion"] <- "Sowjet Union"
killers$country[killers$land == "Tschechoslowakei"] <- "Czechoslovakia"

# Clean years_active
killers %<>% 
  mutate(years_active = gsub("-", "–", years_active)) %>%
  mutate(years_active = gsub("Jahre", "", years_active)) %>%
  mutate(years_active = gsub("er", "s", years_active))

# Clean victims_proven
## Note: Code transform "[number of victims]+" to "[number of victims]". 
killers %<>% 
  mutate(victims_proven = gsub("\\[.*?\\]", "", victims_proven)) %>%
  mutate(victims_proven = gsub("[^0-9\\-]", "", victims_proven)) 
killers$victims_proven <- as.numeric(killers$victims_proven)

# Clean victims_suspected 
killers %<>% 
  mutate(victims_suspected = gsub("\\[.*?\\]", "", victims_suspected)) %>% 
  mutate(victims_suspected = gsub("bis", "–", victims_suspected)) %>% 
  mutate(victims_suspected = gsub("mindestens", "at least", victims_suspected)) 

# Split name to given name and surname
killers_name <- colsplit(killers$name," ", c("given_name", "surname"))
killers <- cbind(killers, killers_name)

# Split years_active 
killers_years <- colsplit(killers$years_active,"–", c("active_from", "active_to"))
killers <- cbind(killers, killers_years)

# Convert columns and replace empty values in active_to
killers$active_from <- as.numeric(killers$active_from)
killers$active_to <- as.numeric(killers$active_to)
killers$active_to <- ifelse(is.na(killers$active_to), killers$active_from, killers$active_to)

# Subset and order killers df
killers <- killers[, c("name", "given_name", "surname", "pseudonym", "country", "sex", "victims_proven", 
                           "victims_suspected", "years_active", "active_from", "active_to")]
killers <- killers[order(killers$name), ]


#-------------------#
# Geocode countries #
#-------------------#

# Google Maps key
apiKey <- "[INSERT OWN KEY HERE]" 

# Adapted function to extract coordinates
getGeoCoord <- function(loc, apiKey) {
  
  request <- paste0("https://maps.googleapis.com/maps/api/geocode/json?", 
                    "address=", gsub(" ", "+", loc), "&key=", apiKey)
  
  result <- request %>% lapply(fromJSON) %>% .[[1]]
  
  if (result$status == "OK") {
    result <- result$results$geometry$location[1, ]
  } else if (result$status == "ZERO_RESULTS") {
    result <- data.frame(lat = NA, lng = NA)
  }
  
  result %>% data.frame
}

# Send request for country locations
countries_loc <- lapply(killers$country, getGeoCoord, apiKey = apiKey) %>% 
  bind_rows %>% setNames(paste0(names(.), "country"))

# Add locations to data and rename columns
killers <- cbind(killers, countries_loc)
names(killers)[names(killers) == "latcountry"] <- "country_lat"
names(killers)[names(killers) == "lngcountry"] <- "country_long"

# Export data
saveRDS(killers, "serial_killers_data.rds")


#---------------------------------#
# Mapping locations using leaflet #
#---------------------------------#

# Set colours
pal <- colorNumeric(palette = c("#ffe6e6", "#ff8080", "#ff0000"), domain = c(0, 100))

# Plot locations of killers
## Note: Locations overlap due to geocoding by country. 
leaflet(killers, width = "100%") %>% 
  addProviderTiles("CartoDB.DarkMatter") %>% 
  # color of locations reflect number of victims
  addCircleMarkers(~country_long,
                   ~country_lat,
                   color = ~pal(victims_proven),
                   radius = 2)


#---------------------------------#
# Mapping locations using ggplot2 #
#---------------------------------#

# Download, unzip and import shapefiles from Natural Earth webpage
temp <- tempfile()
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", temp, mode = "w")
unzip(temp)
world_shp <- readShapeSpatial("ne_50m_admin_0_countries.shp", proj4string = CRS("+proj=longlat +ellps=WGS84"))
unlink(temp)

# Remove Antarctica
world_shp <- subset(world_shp, NAME != "Antarctica")

# Plot locations of killers
country_list <- as.list(unique(killers[, "country"]))
killers_shp <- subset(world_shp, NAME %in% country_list) 

ggplot() + 
  geom_polygon(data = world_shp, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = killers_shp, aes(x = long, y = lat, group = group), color = "red", fill = "red", alpha = 0.3) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  coord_equal()









