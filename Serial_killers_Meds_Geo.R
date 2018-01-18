##------------------------------------------------------------------------------------------##
##                          SERIAL KILLER GEOGRAPHIC ANALYSIS                               ##
##------------------------------------------------------------------------------------------##


## R version 3.3.1 (2016-06-21)

## Main data Source: https://en.wikipedia.org/wiki/List_of_serial_killers_by_number_of_victims#Medical_professionals_and_pseudo-medical_professionals)


#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(dplyr, geosphere, ggplot2, jsonlite, leaflet, magrittr, maptools, rvest)


#----------------------#
# Scrape data from web #
#----------------------#

# Scrape wikipedia tables
url <- "https://en.wikipedia.org/wiki/List_of_serial_killers_by_number_of_victims"

tbl1 <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[2]") %>% 
  html_table(fill = TRUE)
tbl1 <- data.frame(tbl1)

tbl2 <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[3]") %>% 
  html_table(fill = TRUE)
tbl2 <- data.frame(tbl2)

tbl3 <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[4]") %>% 
  html_table(fill = TRUE)
tbl3 <- data.frame(tbl3)

tbl4 <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[5]") %>% 
  html_table(fill = TRUE)
tbl4 <- data.frame(tbl4)

# Rename columns
colnames(tbl3) <- names(tbl1)
colnames(tbl4) <- names(tbl1)

# Merge data
killers <- rbind(tbl1, tbl2, tbl3, tbl4)

## (Pseudo-)Medical professionals only
tbl5 <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[6]") %>% 
  html_table(fill = TRUE)

tbl5 <- data.frame(tbl5)
colnames(tbl5) = names(tbl1)
meds <- tbl5[-1, ] 


#----------------#
# Data wrangling #
#----------------#

# Clean Country
killers %<>% 
  mutate(Country = gsub("\\[[^\\]]*\\]", "", Country))

# Clean Years.active
killers %<>% 
  mutate(Years.active = gsub("\\[[^\\]]*\\]", "", Years.active)) %>%
  mutate(Years.active = gsub("to", "-", Years.active)) %>% 
  mutate(Years.active = gsub("[^0-9\\-]", "", Years.active)) 


###########
## Meds* ##
###########

# * = (Pseudo-)Medical professionals; hereafter Meds only

# Clean Years.active
meds %<>% 
  mutate(Years.active = gsub("\\[[^\\]]*\\]", "", Years.active)) %>%
  mutate(Years.active = gsub("to", "-", Years.active)) %>% 
  mutate(Years.active = gsub("[^0-9\\-]", "", Years.active))

# Clean Proven.victims
meds %<>% mutate(Proven.victims = gsub("[^0-9\\-]", "", Proven.victims))
meds[11, "Proven.victims"] = 11
meds$Proven.victims <- as.numeric(meds$Proven.victims)

# Rename Country (US) to match polygon
meds %<>% mutate(Country = gsub("United States", "United States of America", Country))


#-------------------#
# Geocode countries #
#-------------------#

# Google Maps API
apiKey <- "[INSERT OWN API HERE]"

# Adapted function to extract coordinates
getGeoCoord <- function(loc, apiKey) {
  # Create request
  request <- paste0("https://maps.googleapis.com/maps/api/geocode/json?", 
                    "address=", gsub(" ", "+", loc), "&key=", apiKey)
  
  # extract results and convert them to strings
  result <- request %>% lapply(fromJSON) %>% .[[1]]
  
  if (result$status == "OK") {
    result <- result$results$geometry$location[1, ]
  } else if (result$status == "ZERO_RESULTS") {
    result <- data.frame(lat = NA, lng = NA)
  }
  
  result %>% data.frame
}

# Send request for country locations
Country <- lapply(meds$Country, getGeoCoord, apiKey = apiKey) %>% 
  bind_rows %>% setNames(paste0(names(.), "Country"))

# Add locations to data
meds <- cbind(meds, Country)

# Replace country for Michael Swango
meds[26, "latCountry"] = 37.09024
meds[26, "lngCountry"] = -95.712891


#------------------------#
# Visualization: leaflet #
#------------------------#

# Set colours
pal <- colorNumeric(palette = c("#ffe6e6", "#ff8080", "#ff0000"), domain = c(0, 100))

# Plot locations of killers
leaflet(meds, width = "100%") %>% 
  addProviderTiles("CartoDB.DarkMatter") %>% 
  # color of locations reflect number of victims
  addCircleMarkers(~lngCountry,
                   ~latCountry,
                   color = ~pal(Proven.victims),
                   radius = 2)


#------------------------#
# Visualization: ggplot2 #
#------------------------#

# Download, unzip and import shapefiles from Natural Earth webpage
temp <- tempfile()
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", temp, mode = "w")
unzip(temp)
world_shp <- readShapeSpatial("ne_50m_admin_0_countries.shp", proj4string = CRS("+proj=longlat +ellps=WGS84"))
unlink(temp)

# Remove Antarctica
world_shp <- subset(world_shp, NAME != "Antarctica")

# Plot locations of killers (points)
ggplot() + 
  geom_polygon(data = world_shp, aes(x = long, y = lat, group = group)) +
  geom_point(data = meds, aes(x = lngCountry, y = latCountry), size = 1.2, color = "red") +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  coord_equal()

# Plot locations of killers (polygons) 
Country_list <- as.list(unique(meds[, "Country"]))

meds_shp <- subset(world_shp, NAME %in% Country_list) # Iraq, Czech. Rep. & Czech. missing

ggplot() + 
  geom_polygon(data = world_shp, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = meds_shp, aes(x = long, y = lat, group = group), color = "red", fill = "red", alpha = 0.3) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  coord_equal()