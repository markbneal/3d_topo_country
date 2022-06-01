# 3d Country topo Attempt for New Zealand?

# https://milospopovic.net/making-3d-topographic-maps-in-r/

libs <- c("elevatr", "rayshader", "tidyverse", 
          "sf", "giscoR", "jsonlite", "httr", "png")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = T))


# 1. GET COUNTRY MAP
#-------------------


get_country_sf <- function(specific_country_or_region = "Austria", 
                           output_crsLONGLAT = "+proj=longlat +datum=WGS84 +no_defs", 
                           specific_year = 2010,
                           specific_EPSG = 4326,
                           specific_resolution = 10) {
  
  # Country outlines via giscoR
  # https://cran.r-project.org/web/packages/giscoR/vignettes/giscoR.html
  
  country_sf <- giscoR::gisco_get_countries(
    year = specific_year,
    epsg = specific_EPSG,
    resolution = specific_resolution,
    country = specific_country_or_region)
  
  country_transformed <- st_transform(country_sf, crs = output_crsLONGLAT)
  
  return(country_transformed)
}

#my_output_crsLONGLAT <- "" #or use default

#my_country <- "Austria" # this is now default. Note, Austria is ~100,000 km2
#my_country <- "New Zealand" #  Note, New Zealand is ~300,000 km2
my_country <- "Japan" #  Note, Japan is ~400,000 km2


# country_transformed <- get_country_sf() # would get default, Austria
my_country_transformed <- get_country_sf(my_country)



# 2. GET ELEVATION DATA
#----------------------

get_elevation_data <- function(country_transformed, zoom = 7) {
  # re: zoom level
  # https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
  
  country_elevation <- elevatr::get_elev_raster(
    locations = country_transformed, 
    z = zoom, 
    clip = "locations")
  
  elevation_mat <- raster_to_matrix(country_elevation)
  
  return(elevation_mat)
}

my_country_dem <- get_elevation_data(my_country_transformed, 5) #default of 7 takes longer, too much data?



# Notice the dimensions of the matrix below: we will pass the augmented values to the image save function at the very end. For now, let’s store the height and width.
dim(my_country_dem)[1]

# h <- 537
h <- dim(my_country_dem)[1]
# w <- 1552
w <- dim(my_country_dem)[2]


# 3. GET OVERLAY SATELLITE IMAGE
#----------------------

# bb <- st_bbox(my_country_transformed) #now in function
# type <- "World_Imagery" # not used?
# file <- NULL #not used?

# height <- h*6 #calculation now in function, with h_w_factor default of 6
# width <- w*6

#crs_bb <- 4326 # now default in function

get_satellite_img <- function(specific_country_transformed = my_country_transformed,
                              crs_bb = 4326, #this could be done in function?
                              h_w_factor = 6) {
  
  bb <- st_bbox(specific_country_transformed)
  
  url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
  
  params <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = unbox("https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer"))
      )
    ),
    exportOptions = list(
      outputSize = c(w * h_w_factor, h * h_w_factor)
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = unbox(crs_bb)),
        xmax = unbox(bb["xmax"]),
        xmin = unbox(bb["xmin"]),
        ymax = unbox(bb["ymax"]),
        ymin = unbox(bb["ymin"])
      )
    )
  )
  
  res <- GET(
    url, 
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = toJSON(params))
  )
  
  return(res)
  
}

res <- get_satellite_img() #takes a while

write_map_png <- function(specific_country = my_country) {
  
  res_body <- content(res, type = "application/json")
  img_res <- GET(res_body$results[[1]]$value$url)
  img_bin <- content(img_res, "raw")
  file <- paste0(getwd(), "/", specific_country, "_image.png") #now uses country label for file
  writeBin(img_bin, file)
}

write_map_png()

get_map_png <- function(specific_country = my_country) {
  
  img_file <- paste0(specific_country, "_image.png")
  img <- readPNG(img_file)
  
  return(img)
}

my_country_img <- get_map_png()

# 4. 3D MAP
#---------

my_country_dem %>%
  sphere_shade(texture ="desert") %>%
  add_overlay(my_country_img, alphalayer = .99) %>%
  plot_3d(my_country_dem, 
          zscale = 15, 
          fov = 0, 
          theta = 0, 
          zoom = .55, 
          phi = 75,
          windowsize = c(w, h), # do these need to be the same as h and w, or just convenient for same aspect ratio?
          background="black")

render_highquality(filename=paste0(my_country, "_dem.png"),
                   lightintensity=1500,
                   lightaltitude=90,
                   title_text= paste0("Topography of ", my_country, " \n",
                   "Original code Milos Popovic https://milospopovic.net\n",
                   "Modified by Mark Neal, github repo here"),
                   title_font="Georgia",
                   title_color="grey20",
                   title_size=100,
                   title_offset=c(360,180),
                   width=w*3, 
                   height=h*3)

