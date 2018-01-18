##--------------------------------------------------------------------------------------------##
##                                 SERIAL KILLERS IN THE US                                   ##
##--------------------------------------------------------------------------------------------##


## R version 3.3.1 (2016-06-21)

## Main data source: https://en.wikipedia.org/wiki/List_of_serial_killers_in_the_United_States


# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(dplyr, gender, genderdata, ggplot2, jsonlite, magrittr, maptools, raster, reshape2, rvest, stringr, tidyr)


#----------------------#
# Scrape data from web #
#----------------------#

# Scrape wikipedia table
url <- "https://en.wikipedia.org/wiki/List_of_serial_killers_in_the_United_States"

killers <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[1]") %>% 
  html_table(fill = TRUE)

killers %<>% data.frame()


#----------------#
# Data wrangling #
#----------------#

# Clean Years.active
killers %<>% 
  mutate(Years.active = gsub("\\[[^\\]]*\\]", "", Years.active)) %>%
  mutate(Years.active = gsub("to", "-", Years.active)) %>% 
  mutate(Years.active = gsub("[^0-9\\-]", "", Years.active)) 


#-------------------#
# Extract locations #
#-------------------#

# Scrape wikipedia table
url <- "https://simple.wikipedia.org/wiki/List_of_U.S._states"

states <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table") %>% 
  html_table(fill = TRUE)

# Extract state names
states %<>% data.frame()
states <- states[, "State.Name"]

# Extract location by matching
killers$State <- str_extract(killers$Notes, paste(states, collapse = "|"))


#-------------#
# Predict sex #
#-------------#

# Split name to given and surname
killers_name <- colsplit(killers$Name," ", c("Given.name", "Surname"))
killers <- cbind(killers, killers_name)

# Split Years.active 
killers_years <- colsplit(killers$Years.active,"-",c("Active.from", "Active.to"))
killers <- cbind(killers, killers_years)

# Replace missing values in Active.to
killers$Active.to <- ifelse(is.na(killers$Active.to), killers$Active.from, killers$Active.to)

# Adapt year range to packages, i.e., 1880-2012
killers$Active.from.mod <- killers$Active.from
killers$Active.from.mod[killers$Active.from < "1880"] <- "1880"
killers$Active.from.mod[killers$Active.from > "2012"] <- "2012"

# Predict sex and clean data
results <- gender_df(killers, name_col = "Given.name", method = "ssa", year_col = "Active.from.mod")
sex <- unique(results[order(results$name), c("name", "gender")])
colnames(sex) <- c("Given.name", "Sex")

# Merge with df
killers <- left_join(killers, sex, by ="Given.name")


#-------------------------#
# Extract causes of death #
#-------------------------#

# Scrape wikipedia table
url <- "https://en.wikipedia.org/wiki/List_of_causes_of_death_by_rate"

causes_death <- url %>% read_html() %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[2]") %>% 
  html_table(fill = TRUE)

# Transform to list
causes_death %<>% data.frame()
causes_death <- causes_death[-c(1:2), "Cause"]

# Add execution as cause of death
add <- list("Executed", "Died in Prison", "Sentenced to Death", "Killed")
causes_death <- union(causes_death, add)

# Extract cause of death by matching
killers$Death.cause <- str_extract(killers$Status, paste(causes_death, collapse = "|"))

# Transform "Sentenced to Death" to NA and extend "Died in Prison"
## Note: Undisclosed refers to the information obtained from wikipedia.
killers %<>% 
  mutate(Death.cause = gsub("Sentenced to Death", NA, Death.cause)) %>%
  mutate(Death.cause = gsub("Died in Prison", "Died in Prison (undisclosed)", Death.cause)) 

# Extract year of death
killers$Death.year <- killers$Status
killers$Death.year <- str_extract(killers$Death.year, "[0-9]{4}")


#---------------------------#
# Visualize causes of death #
#---------------------------#

# Remove NAs
killers_cause <- killers %>% drop_na(Death.cause)

# Plot causes of death
ggplot(killers_cause, aes(Death.cause)) + geom_bar(color = "red", fill = "black") + 
  labs(x = "Cause", y = "Count", title = "Causes of death", subtitle = "Imprisoned serial killers in the US") +
  ylim(0, 60)

# Cause of death by gender
killers_sex <- killers %>% 
  group_by(Sex, Death.cause) %>% 
  tally() %>% 
  complete(Death.cause, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)

# Remove NAs
killers_sex %<>% drop_na(Sex)

# Plot causes of death by gender
ggplot(killers_sex, aes(Death.cause, percentage, fill = Sex)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Cause", y = "Count", title = "Causes of death by gender", subtitle = "Imprisoned serial killers in the US") +
  ylim(0, 60)


#-----------------------------#
# Visualize number of victims #
#-----------------------------#

# Transform to numeric and remove NAs
killers$Proven.victims <- as.numeric(killers$Proven.victims)

# Remove NAs
killers_victims <- killers %>% drop_na(Proven.victims)

# Number of victims by sex
killers_victims <- killers %>% 
  group_by(Sex, Proven.victims) %>% 
  tally() %>% 
  complete(Proven.victims, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)

# Remove NAs
killers_victims %<>% drop_na(Sex)

# Plot number of (proven) victims
ggplot(killers_victims, aes(Proven.victims, percentage)) + 
  geom_bar(stat = "identity", color = "red", fill = "black") +
  labs(x = "Number of victims", y = "Count", title = "Number of victims", subtitle = "Imprisoned serial killers in the US") +
  ylim(0, 25)

# Plot number of (proven) victims by gender
ggplot(killers_victims, aes(Proven.victims, percentage, fill = Sex)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Number of victims", y = "Count", title = "Number of victims by gender", subtitle = "Imprisoned serial killers in the US") +
  ylim(0, 25)


#---------------#
# Visualize sex #
#---------------#

# Count number of killers by sex
counts <- data.frame(table(killers$Sex))

# Plot killers by sex
ggplot(counts, aes(Var1, Freq)) + 
  geom_bar(stat = "identity", color = "red", fill = "black") +
  labs(x = "Number of killers", y = "Count", title = "Number of killers by gender", subtitle = "Imprisoned serial killers in the US") +
  ylim(0, 250)


#---------------------#
# Visualize locations #
#---------------------#

# Download, unzip and import cartographic boundary shapefiles from USCB webpage
temp <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_state_5m.zip", temp, mode = "w")
unzip(temp)
US_shp <- readShapeSpatial("cb_2014_us_state_5m.shp", proj4string = CRS("+proj=longlat +ellps=WGS84"))
unlink(temp)

# Crop to Continental US
US_shp_cropped <- crop(US_shp, extent(-124.848974, -66.885444, 24.396308, 49.384358)) 

# Subset killer locations
killers_loc <- killers$State
killers_shp <- subset(US_shp_cropped, NAME %in% killers_loc)

# Plot (known) locations of killers
ggplot() + 
  geom_polygon(data = US_shp_cropped, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = killers_shp, aes(x = long, y = lat, group = group), color = "red", fill = "red", alpha = 0.3) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  coord_equal()