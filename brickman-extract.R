suppressPackageStartupMessages(
  {
    library(dplyr) #data transformation
    library(readr)  # CSV and kin I/O
    library(sf) #simple features -- storing and managing georeferences
                # https://en.wikipedia.org/wiki/Simple_Features
    library(stars) #spatial data "Space Time ARrayS"
    library(calanusthreshold) #calanus data
    library(brickman) #brickman data
    library(ncdf4)
  })

#use query_layers to determine available variable names (exclude _ann)
climate <- brickman::read_layers(scenario="PRESENT", vars=c("SST", "SSS"), interval="ann")

##### PROCURING CALANUS DATA #####

#Bounding box:  xmin: -68.75417 ymin: 43.88309 xmax: -58.17 ymax: 50.104

#specifying the path to the calanus data
gsts_filename <- file.path("/mnt/ecocast/projects/calanus/calanus-threshold/data",
                           "GSTS_Calanus_consolidated.csv")

###### this code currently isn't being used, but could be useful 
# we wanted to further specify calanus dataset

#going to lump all stages of cfin together
lump_var <- "Calanus finmarchicus"
#defining variables to drop 
#note that complement_species returns species other than given
drop_var = c("geometry", "station", "year", "siconc",
             "sithick", calanusthreshold::complement_species(lump_var))

calanus <- readr::read_csv(gsts_filename, show_col_types = FALSE) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Grrrrr
# vals <- stars::st_extract(climate, calanus)
# Error in colrow_from_xy(pts, x, NA_outside = TRUE) : 
# colrow_from_xy not supported for curvilinear objects

# So we roll our own
# vals <- extract_points(brickman::compose_filename(),
#                                 c("dSST_ann","dSSS_ann"),
#                                 calanus)

# but now there is a function brickman::extract_points
vals <- brickman::extract_points(brickman::compose_filename(),
                                c("dSST_ann","dSSS_ann"),
                                calanus,
                                complete = TRUE)





