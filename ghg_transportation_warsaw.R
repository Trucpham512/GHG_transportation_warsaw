# 1. LOAD PACKAGES
library(gtfstools)
library(sf)
library(osmdata)
library(dplyr)
library(leaflet)

# 2. LOAD GTFS
gtfs <- read_gtfs("warsaw_gtfs.zip")

# Convert shapes to sf
shapes_sf <- convert_shapes_to_sf(gtfs)

# 3. DOWNLOAD STREET GEOMETRY FROM OSM
street_name <- "Aleje Jerozolimskie"

q <- opq("Warsaw, Poland") %>%
  add_osm_feature("highway") %>%
  add_osm_feature("name", street_name)

osm_street <- osmdata_sf(q)$osm_lines
osm_street <- st_transform(osm_street, st_crs(shapes_sf))

# 4. FIND SHAPES THAT INTERSECT THE STREET
intersections <- st_intersects(shapes_sf, osm_street)
shape_ids <- shapes_sf$shape_id[lengths(intersections) > 0]

# 5. FILTER GTFS BY SHAPE_ID
gtfs_filtered <- filter_by_shape_id(gtfs, shape_ids)

# 6. GET SHAPES FOR FILTERED ROUTES
routes_sf <- shapes_sf[shapes_sf$shape_id %in% shape_ids, ]

# 7. COMPUTE LENGTH OF EACH SHAPE
routes_sf$length_m <- as.numeric(st_length(routes_sf))
routes_sf$length_km <- routes_sf$length_m / 1000

# 8. ASSUME AVERAGE SPEED (km/h)
speed_kmh <- 18
routes_sf$time_h <- routes_sf$length_km / speed_kmh

# 9. EMISSION FACTORS (example values)
EF_CO2  <- 1100   # g/km
EF_NOx  <- 8      # g/km
EF_PM10 <- 0.3    # g/km

# 10. COMPUTE EMISSIONS
routes_sf$CO2  <- routes_sf$length_km * EF_CO2
routes_sf$NOx  <- routes_sf$length_km * EF_NOx
routes_sf$PM10 <- routes_sf$length_km * EF_PM10

# 11. SUMMARIZE BY ROUTE
emi_summary <- routes_sf %>%
  st_drop_geometry() %>%
  group_by(shape_id) %>%
  summarise(
    length_km = sum(length_km),
    CO2  = sum(CO2),
    NOx  = sum(NOx),
    PM10 = sum(PM10)
  )

emi_summary

# 12. PLOT MAP
leaflet() %>%
  addTiles() %>%
  addPolylines(data = routes_sf, color = "blue", weight = 3, opacity = 0.8) %>%
  addPolylines(data = osm_street, color = "red", weight = 4, opacity = 1) %>%
  addLegend("bottomright",
            colors = c("red", "blue"),
            labels = c("Selected Street", "Bus Routes"),
            title = "Map Layers")

# 13. HEATMAP CO2
pal <- colorNumeric(
  palette = "YlOrRd",        # yellow → orange → red
  domain = routes_sf$CO2
)

leaflet() %>%
  addTiles() %>%
  addPolylines(
    data = routes_sf,
    color = ~pal(CO2),       # màu theo CO₂
    weight = 5,
    opacity = 0.9,
    popup = ~paste0(
      "<b>Shape ID:</b> ", shape_id, "<br>",
      "<b>Length:</b> ", round(length_km, 2), " km<br>",
      "<b>CO₂:</b> ", round(CO2, 1), " g<br>",
      "<b>NOx:</b> ", round(NOx, 2), " g<br>",
      "<b>PM10:</b> ", round(PM10, 3), " g"
    )
  ) %>%
  addPolylines(
    data = osm_street,
    color = "red",
    weight = 6,
    opacity = 1
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = routes_sf$CO2,
    title = "CO₂ (g)",
    opacity = 1
  )


