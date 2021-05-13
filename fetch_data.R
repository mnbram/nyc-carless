library(httr)
library(readr)
library(dplyr)

# Get census (ACS) data -------------------------------------------------------

API_KEY <- readLines("census_key.txt")

vehicle_vars <- sapply(1:6, function(x) paste0("B08201_00", x, "E"))

vehicles_url <- paste0(
  "https://api.census.gov/data/2019/acs/acs5?get=NAME,",
  paste0(vehicle_vars, collapse = ","),
  "&for=tract:*&in=state:36+county:005,047,061,081,085",
  "&key=", API_KEY
)

vehicles_req <- GET(vehicles_url)
vehicles_text <- content(vehicles_req, "text")

vehicles_csv <- gsub("(?<=\n)\\[", "", gsub(
  "],(?=\n)", "",
  substr(vehicles_text, 3, nchar(vehicles_text) - 2),
  perl = TRUE
), perl = TRUE)

veh <- read_csv(vehicles_csv) %>%
  rename(total = 2, zero = 3, one = 4, two = 5, three = 6, four_plus = 7)

write_csv(veh, "data/nyc_vehicles_avail_acs2019_5y_tract.csv")

# Get census tract shapefiles -------------------------------------------------

ny_tract_url <- (
  "https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_36_tract_500k.zip")

download.file(ny_tract_url, "data/cb_2019_36_tract_500k.zip")
unzip("data/cb_2019_36_tract_500k.zip", exdir = "data/tracts")

# Get transit shapefiles ------------------------------------------------------

# Subway
subway_url <- paste0(
  "https://data.cityofnewyork.us/api/geospatial/",
  "3qz8-muuu?method=export&format=Shapefile")
download.file(subway_url, "data/subway_lines.zip")
unzip("data/subway_lines.zip", exdir = "data/subway")

# Bicycle
bike_url <- paste0(
  "https://data.cityofnewyork.us/api/geospatial/",
  "7vsa-caz7?method=export&format=Shapefile")
download.file(bike_url, "data/bicycle_routes.zip")
unzip("data/bicycle_routes.zip", exdir = "data/bicycle")

# Bus
get_mta_bus_data <- function(borough) {
  url <- paste0(
    "http://web.mta.info/developers/data/nyct/bus/google_transit_",
    borough, ".zip"
  )
  download.file(url, paste0("data/bus_", borough, ".zip"))
}
get_mta_bus_data("manhattan")
get_mta_bus_data("brooklyn")
get_mta_bus_data("queens")
get_mta_bus_data("bronx")
get_mta_bus_data("staten_island")
