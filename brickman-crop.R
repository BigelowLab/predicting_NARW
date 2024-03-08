suppressPackageStartupMessages(
  {
    library(dplyr) #data transformation
    library(sf) #simple features -- storing and managing georeferences
    library(stars) #spatial data
    library(calanusthreshold) #calanus data
    library(brickman) #brickman data
  })

#use query_layers to determine available variable names (exclude _ann)
climate <- brickman::read_layers(scenario="PRESENT", vars=c("SST", "SSS"), interval="ann")

##### PROCURING CALANUS DATA #####

#Bounding box:  xmin: -68.75417 ymin: 43.88309 xmax: -58.17 ymax: 50.104

#specifying the path to the calanus data
gsts_filename <- file.path("/mnt/ecocast/projects/calanus/calanus-threshold/data",
                           "GSTS_Calanus_consolidated.csv")

# we wanted to further specify calanus dataset
lump_var <- "Calanus finmarchicus"
#defining variables to drop 
#note that complement_species returns species other than given
drop_var = c("geometry", "station", "year", "siconc",
             "sithick", calanusthreshold::complement_species(lump_var))

calanus <- readr::read_csv(gsts_filename, show_col_types = FALSE) |>
  #adding prep_dataset to drop unnecessary variables and lump calanus 
  # into a patch() value
  calanusthreshold::prep_dataset(
    lump_var = lump_var,
    #lump_fun = starts_with (default)
    drop_var = drop_var,
    # to maintain original number of cases 
    complete_cases_only = FALSE) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


### CROPPING ###

#saving the bounding box of calanus 
bb <- (sf::st_bbox(calanus) + c(-.5, -.5, .5, .5)) 

# cropping climate data 
clim <- climate[bb |> st_as_sfc(crs = st_crs(climate))]

#Hmmm, cropping is not a thing for curvilinear grids. 
# but points outside of the cropped shape are assigned to NA
dim(clim)
#    x    y 
# 1001 1250 
dim(climate)
#    x    y 
# 1001 1250

# why does calanus only have 264 records?

### PLOTTING #####################

### REFERENCE PLOTTING CODE ###
library(leaflet)
leaflet(data = calanus) |> addTiles() |> addCircleMarkers(radius=3)
### 

#plotting cropped climate data 
plot(clim[1], axes = TRUE, xlim = bb$xlim, ylim = bb$ylim)

#plotting uncropped climate data
#this code takes ages
plot(climate, attr = 'SST_ann', 
     #limiting plot scope to calanus bounds, although actual data not cropped
     xlim = bb$xlim,
     ylim = bb$ylim,
     axes = TRUE)

#attempting to put calanus data over climate data
#problem: cannot get the axes to match up 
#these two pieces of code sometimes work, but not always 
plot(sf::st_geometry(calanus), col = 'orange', add=TRUE)
points(st_coordinates(calanus), col="orange")





