library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(htmltools)
library(tidytransit)

tracts <- st_read("data/tracts/cb_2019_36_tract_500k.shp") %>%
  filter(COUNTYFP %in% c("005", "047", "061", "081", "085")) %>%
  st_transform(st_crs("+proj=longlat +datum=WGS84 +no_defs"))

subway <- st_read(
  "data/subway/geo_export_f573270e-5856-4601-95ce-7c8c24e78273.shp") %>%
  st_transform(st_crs("+proj=longlat +datum=WGS84 +no_defs"))

bikes <- st_read(
  "data/bicycle/geo_export_9689df31-46e7-4799-8c5c-1e9521582b36.shp") %>%
  st_transform(st_crs("+proj=longlat +datum=WGS84 +no_defs"))

create_bus_sf <- function(borough) {
  read_gtfs(
    paste0("data/bus_", borough, ".zip"),
    c("shapes", "stops")
  ) %>%
  gtfs_as_sf() %>%
  `$`(shapes)
}
bus <- rbind(
  bus_manhattan <- create_bus_sf("manhattan"),
  bus_brooklyn <- create_bus_sf("brooklyn"),
  bus_queens <- create_bus_sf("queens"),
  bus_bronx <- create_bus_sf("bronx"),
  bus_staten <- create_bus_sf("staten_island")
)

veh <- read_csv("data/nyc_vehicles_avail_acs2019_5y_tract.csv")

vehpct <- veh %>%
  filter(total > 0) %>%
  mutate(pct = zero / total) %>%
  select(COUNTYFP = county, TRACTCE = tract, total, pct)

pal_veh <- colorNumeric(
  palette = "inferno",
  domain = c(0, 1)
)

pal_veh_rev <- colorNumeric(
  palette = "inferno",
  domain = c(0, 1),
  reverse = TRUE
)

labelfunc <- function(percent, households) {
  paste0(
    '<span style="font-weight:bold;font-size:14pt">',
    scales::percent(percent, accuracy = 1), "</span><br/>",
    " of households do NOT have access to a car<br/>(",
    households, " households)"
  )
}

joined <- tracts %>%
  inner_join(vehpct, by = c("COUNTYFP", "TRACTCE")) %>%
  mutate(lab = labelfunc(pct, total))

joined_bbox <- st_bbox(joined)

leaf <- joined %>%
  leaflet(options = leafletOptions(minZoom = 10)) %>%
  addPolygons(
    fillColor = ~pal_veh(pct),
    color = ~pal_veh(pct),
    weight = 2,
    fillOpacity = 1,
    label = lapply(joined$lab, htmltools::HTML),
    labelOptions = labelOptions(style = list(
      "text-align" = "center",
      "background-color" = "#333333",
      "color" = "white"
    )),
    group = "Car ownership"
  ) %>%
  addProviderTiles(providers$Stamen.TonerBackground) %>%
  addProviderTiles(
    providers$Stamen.TerrainLabels,
    group = "Place labels"
  ) %>%
  addLegend(
    "topleft",
    pal = pal_veh_rev,
    values = 0:5/5,
    title = "% of households<br/>WITHOUT access<br/>to a car",
    opacity = 1,
    labFormat = function(type, x) scales::percent(sort(x, decreasing = TRUE)),
    className = "info legend leaf-legend",
    group = "Car ownership"
  ) %>%
  addMapPane("bikes", 470) %>%
  addMapPane("buses", 475) %>%
  addMapPane("subways", 480) %>%
  addPolylines(
    data = subway,
    opacity = 1,
    color = "#333333",
    weight = 7,
    options = pathOptions(pane = "subways"),
    group = "Subway lines"
  ) %>%
  addPolylines(
    data = subway,
    opacity = 1,
    color = "#44ffaa",
    weight = 5,
    options = pathOptions(pane = "subways"),
    group = "Subway lines"
  ) %>%
  addPolylines(
    data = bikes,
    opacity = 1,
    color = "#333333",
    weight = 4,
    options = pathOptions(pane = "bikes"),
    group = "Bike routes"
  ) %>%
  addPolylines(
    data = bikes,
    opacity = 1,
    color = "#44aaff",
    weight = 2,
    options = pathOptions(pane = "bikes"),
    group = "Bike routes"
  ) %>%
  addPolylines(
    data = bus,
    opacity = 1,
    color = "#333333",
    weight = 4,
    options = pathOptions(pane = "buses"),
    group = "Bus routes"
  ) %>%
  addPolylines(
    data = bus,
    opacity = 1,
    color = "#ff3333",
    weight = 2,
    options = pathOptions(pane = "buses"),
    group = "Bus routes"
  ) %>%
  addLayersControl(
    overlayGroups = c(
      "Car ownership",
      "Subway lines",
      "Bus routes",
      "Bike routes",
      "Place labels"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("Subway lines") %>%
  hideGroup("Place labels") %>%
  hideGroup("Bike routes") %>%
  hideGroup("Bus routes") %>%
  setMaxBounds(
    joined_bbox[[1]], joined_bbox[[2]], joined_bbox[[3]], joined_bbox[[4]]
  ) %>%
  setView(
    mean(joined_bbox[[1]], joined_bbox[[3]]),
    sum(joined_bbox[[2]], joined_bbox[[4]]*2)/3,
    11
  )

leaf$sizingPolicy$defaultHeight <- "calc(100vh - 20px)"

css <- read_file("style.css")

browsable(
  tagList(list(
    tags$head(
      tags$style(css)
    ),
    leaf
  ))
)

# TO DO
# Add PUMA names to tooltips?
# Get PUMS data for income x car ownership analysis (Megan?)
# Set up git repo
# Add title
# Figure out how to publish