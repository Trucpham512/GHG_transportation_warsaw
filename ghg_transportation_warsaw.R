# 1. LOAD PACKAGES
library(gtfstools)
library(sf)
library(osmdata)
library(dplyr)
library(leaflet)
library(osmdata)
library(sf)

# 2. LOAD GTFS
gtfs <- read_gtfs("warsaw_gtfs.zip")

# Convert shapes to sf
shapes_sf <- convert_shapes_to_sf(gtfs)

# 3. DOWNLOAD STREET GEOMETRY FROM OSM
# Use a stable Overpass server
set_overpass_url("https://overpass-api.de/api/interpreter")

street_name <- "Aleje Jerozolimskie"

# Build minimal query (filter only by name)
q <- opq("Warsaw, Poland", timeout = 60) %>%
  add_osm_feature("name", street_name)

# Function to safely fetch OSM data with retries
fetch_osm_with_retry <- function(query, attempts = 3, wait_sec = 3) {
  for (i in seq_len(attempts)) {
    message(sprintf("Attempting to load OSM data (try %d)...", i))
    
    result <- tryCatch(
      osmdata_sf(query)$osm_lines,
      error = function(e) NULL
    )
    
    if (!is.null(result)) {
      message("Download successful!")
      return(result)
    }
    
    Sys.sleep(wait_sec)
  }
  
  stop("Unable to load OSM data after multiple attempts. Please try again in 1–2 minutes.")
}

# Fetch street geometry
osm_street <- fetch_osm_with_retry(q)

# Transform CRS to match shapes
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

# 13. CO2 MAP
pal <- colorNumeric("YlOrRd", domain = routes_sf$CO2)

map_CO2 <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = routes_sf, color = ~pal(CO2), weight = 5, opacity = 0.9) %>%
  addPolylines(data = osm_street, color = "red", weight = 6) %>%
  addLegend("bottomright", pal = pal, values = routes_sf$CO2, title = "<span style='font-size:18px;'>CO₂ (g)</span>", opacity = 1)

# 14. NOx MAP
pal_nox <- colorNumeric("PuBuGn", domain = routes_sf$NOx)

map_NOx <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = routes_sf, color = ~pal_nox(NOx), weight = 5, opacity = 0.9) %>%
  addPolylines(data = osm_street, color = "red", weight = 6) %>%
  addLegend(
    "bottomright",
    pal = pal_nox,
    values = routes_sf$NOx,
    title = "<span style='font-size:18px;'>NOx (g)</span>",
    opacity = 1
  )


# 15. PM10 MAP
pal_pm10 <- colorNumeric("YlGnBu", domain = routes_sf$PM10)

map_PM10 <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = routes_sf, color = ~pal_pm10(PM10), weight = 5, opacity = 0.9) %>%
  addPolylines(data = osm_street, color = "red", weight = 6) %>%
  addLegend(
    "bottomright",
    pal = pal_pm10,
    values = routes_sf$PM10,
    title = "<span style='font-size:18px;'>PM10 (g)</span>",
    opacity = 1
  )

# 16. CO2eq MAP
routes_sf$CO2eq <- routes_sf$CO2 + routes_sf$NOx * 0.02 + routes_sf$PM10 * 0.004
pal_co2eq <- colorNumeric("RdPu", domain = routes_sf$CO2eq)

map_CO2eq <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = routes_sf, color = ~pal_co2eq(CO2eq), weight = 5, opacity = 0.9) %>%
  addPolylines(data = osm_street, color = "red", weight = 6) %>%
  addLegend(
    "bottomright",
    pal = pal_co2eq,
    values = routes_sf$CO2eq,
    title = "<span style='font-size:18px;'>CO₂eq (g)</span>",
    opacity = 1
  )

# STEP 23 — EXPORT HIGH-RESOLUTION MAPS (2000px, STABLE VERSION)

library(mapview)
library(webshot)

# Install PhantomJS once (if already installed, this line is ignored)
webshot::install_phantomjs()

# Export maps at 2000px resolution
mapshot(map_CO2,           file = "CO2_map.png",           vwidth = 2000, vheight = 1400)
mapshot(map_NOx,           file = "NOx_map.png",           vwidth = 2000, vheight = 1400)
mapshot(map_PM10,          file = "PM10_map.png",          vwidth = 2000, vheight = 1400)
mapshot(map_CO2eq,         file = "CO2eq_map.png",         vwidth = 2000, vheight = 1400)


