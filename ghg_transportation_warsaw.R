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

# 14. HEATMAP NOx
pal_nox <- colorNumeric(
  palette = "PuBuGn",      # xanh → tím
  domain = routes_sf$NOx
)

leaflet() %>%
  addTiles() %>%
  addPolylines(
    data = routes_sf,
    color = ~pal_nox(NOx),
    weight = 5,
    opacity = 0.9,
    popup = ~paste0(
      "<b>Shape ID:</b> ", shape_id, "<br>",
      "<b>NOx:</b> ", round(NOx, 2), " g<br>",
      "<b>CO₂:</b> ", round(CO2, 1), " g<br>",
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
    pal = pal_nox,
    values = routes_sf$NOx,
    title = "NOx (g)",
    opacity = 1
  )

# 15. HEATMAP PM10
pal_pm10 <- colorNumeric(
  palette = "YlGnBu",      # vàng → xanh dương
  domain = routes_sf$PM10
)

leaflet() %>%
  addTiles() %>%
  addPolylines(
    data = routes_sf,
    color = ~pal_pm10(PM10),
    weight = 5,
    opacity = 0.9,
    popup = ~paste0(
      "<b>Shape ID:</b> ", shape_id, "<br>",
      "<b>PM10:</b> ", round(PM10, 3), " g<br>",
      "<b>CO₂:</b> ", round(CO2, 1), " g<br>",
      "<b>NOx:</b> ", round(NOx, 2), " g"
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
    pal = pal_pm10,
    values = routes_sf$PM10,
    title = "PM10 (g)",
    opacity = 1
  )

# 16. HEATMAP CO2eq (IPCC proxy)
routes_sf$CO2eq <- routes_sf$CO2 +
  routes_sf$NOx * 0.02 +
  routes_sf$PM10 * 0.004

pal_co2eq <- colorNumeric(
  palette = "RdPu",      # hồng → tím
  domain = routes_sf$CO2eq
)

leaflet() %>%
  addTiles() %>%
  addPolylines(
    data = routes_sf,
    color = ~pal_co2eq(CO2eq),
    weight = 5,
    opacity = 0.9,
    popup = ~paste0(
      "<b>Shape ID:</b> ", shape_id, "<br>",
      "<b>CO₂eq:</b> ", round(CO2eq, 1), " g<br>",
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
    pal = pal_co2eq,
    values = routes_sf$CO2eq,
    title = "CO₂eq (g)",
    opacity = 1
  )

# STEP 17 — CREATE SEGMENTS (ROBUST FIX)
library(purrr)

make_segments <- function(shape_row) {
  geom <- shape_row$geometry
  
  # Convert MULTILINESTRING → LINESTRING
  if (inherits(geom, "MULTILINESTRING")) {
    geom <- st_line_merge(geom)
  }
  
  # Skip empty or invalid geometry
  if (st_is_empty(geom)) {
    return(NULL)
  }
  
  coords <- st_coordinates(geom)
  
  # Skip shapes with fewer than 2 points
  if (nrow(coords) < 2) {
    return(NULL)
  }
  
  # Build segments
  segs <- map2(
    .x = coords[-nrow(coords), ],
    .y = coords[-1, ],
    ~ st_linestring(rbind(.x, .y))
  )
  
  st_sf(
    shape_id = shape_row$shape_id,
    geometry = st_sfc(segs, crs = st_crs(shape_row))
  )
}

segments_sf <- map_dfr(1:nrow(routes_sf), ~ make_segments(routes_sf[.x, ]))

# STEP 18
segments_sf$length_m  <- as.numeric(st_length(segments_sf))
segments_sf$length_km <- segments_sf$length_m / 1000

# STEP 19
EF_CO2  <- 1100
EF_NOx  <- 8
EF_PM10 <- 0.3

segments_sf$CO2  <- segments_sf$length_km * EF_CO2
segments_sf$NOx  <- segments_sf$length_km * EF_NOx
segments_sf$PM10 <- segments_sf$length_km * EF_PM10

segments_sf$CO2eq <- segments_sf$CO2 +
  segments_sf$NOx * 0.02 +
  segments_sf$PM10 * 0.004

# STEP 20
pal_seg <- colorNumeric("YlOrRd", domain = segments_sf$CO2)

leaflet() %>%
  addTiles() %>%
  addPolylines(
    data = segments_sf,
    color = ~pal_seg(CO2),
    weight = 4,
    opacity = 0.9
  ) %>%
  addLegend("bottomright", pal = pal_seg, values = segments_sf$CO2,
            title = "CO₂ per segment (g)")

# 21. CREATE EMISSION INDEX (ALL)
norm <- function(x) (x - min(x)) / (max(x) - min(x))

segments_sf$CO2_norm  = norm(segments_sf$CO2)
segments_sf$NOx_norm  = norm(segments_sf$NOx)
segments_sf$PM10_norm = norm(segments_sf$PM10)

# Weights (adjustable)
w_CO2  = 1
w_NOx  = 1
w_PM10 = 1

segments_sf$EmissionIndex =
  segments_sf$CO2_norm  * w_CO2 +
  segments_sf$NOx_norm  * w_NOx +
  segments_sf$PM10_norm * w_PM10

# segments_sf$EmissionIndex <- norm(segments_sf$EmissionIndex)

# 22. HEATMAP EMISSION INDEX (ALL)
pal_total <- colorNumeric(
  palette = "inferno",   # tím → đỏ → vàng (đẹp và rõ)
  domain = segments_sf$EmissionIndex
)

leaflet() %>%
  addTiles() %>%
  addPolylines(
    data = segments_sf,
    color = ~pal_total(EmissionIndex),
    weight = 4,
    opacity = 0.9,
    popup = ~paste0(
      "<b>Shape ID:</b> ", shape_id, "<br>",
      "<b>Segment length:</b> ", round(length_km, 4), " km<br><br>",
      "<b>CO₂:</b> ", round(CO2, 2), " g<br>",
      "<b>NOx:</b> ", round(NOx, 3), " g<br>",
      "<b>PM10:</b> ", round(PM10, 4), " g<br>",
      "<b>CO₂eq:</b> ", round(CO2eq, 2), " g<br><br>",
      "<b>Emission Index:</b> ", round(EmissionIndex, 3)
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
    pal = pal_total,
    values = segments_sf$EmissionIndex,
    title = "Combined Emission Index",
    opacity = 1
  )