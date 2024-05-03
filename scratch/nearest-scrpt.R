
# construct df out of lat and long inputs 
lonlat <- data.frame(cbind(long = -118.39068621397,lat = 33.8145966383499))

# making the point into a dataframe / sf object
lonlat_sf = st_as_sf(lonlat, coords=c("long", "lat"), crs="EPSG:4326")


# make the areas df into a spatial object
polsf <- sf::st_as_sf(advisory_areas)

# advisory_lines <- polsf %>% 
#   st_cast(to = 'LINESTRING')

# Find nearest feature from filtered reference geometries
#nearest <- filtered_polsf[sf::st_nearest_feature(lonlat_sf, filtered_polsf), ]

# assign the point to a fishing zone polygon based on nearest distance

nearest <- polsf[sf::st_nearest_feature(lonlat_sf, polsf) ,]

# get the distance measurement from the point to the nearest polygon
meters <- st_distance(x = lonlat_sf, y = nearest) %>% 
  as.numeric()

nearest["check"] <- if(any(meters > 500)) {
  "too far"
} else {
  "TRUE"
}   



# assign point a sediment DDT value
advisory_id <- lonlat_sf %>% 
  mutate(name = nearest$Name) %>% 
  st_drop_geometry()

name <- advisory_id[[1]]



path = "shinydashboard/data/OEHHA/"
species_name_img <- tolower(gsub(" ", "-", input$species))

#Determine image path based on advisory name
image_path <- read_csv(paste0(path, "venturaharbor", "/other_advisory.csv")) %>% 
  filter(Species == "Sandbass")

nrow(image_path)
