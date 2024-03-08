# suppressPackageStartupMessages(
#   {
#     library(dplyr) #data transformation
#     library(sf) #simple features -- storing and managing georeferences
#     library(stars) #spatial data
#     library(calanusthreshold) #calanus data
#     library(brickman) #brickman data
#     library(ncdf4) #querying data 
#     library(rsample) #tidymodels
#     library(recipes)
#     library(parsnip)
#     library(yardstick)
#     library(broom)
#     library(workflows)
#     library(tidyverse)
#   })
source("setup.R") #using that code 

###### creating model fit ######

### get patch data
gsts_filename <- file.path("/mnt/ecocast/projects/calanus/calanus-threshold/data",
                           "GSTS_Calanus_consolidated.csv")
lump_var <- "Calanus finmarchicus"
#note that complement_species returns species other than given
drop_var = c("geometry", "station", "year", "siconc",
             "sithick", calanusthreshold::complement_species(lump_var))
calanus <- readr::read_csv(gsts_filename, show_col_types = FALSE) |>
  #adding prep_dataset to drop unnecessary variables and lump calanus 
  calanusthreshold::prep_dataset(
    lump_var = lump_var,
    drop_var = drop_var,
    complete_cases_only = FALSE) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
# bounding box information for later plotting
bb <- (sf::st_bbox(calanus) + c(-.5, -.5, .5, .5)) 

### reading in tbl climate data from obs
present_data <- readr::read_csv(get_path("obs/PRESENT_ann.csv.gz"))

####### creating model data set 
# adding patch[] to present data set 
present_data$patch <- calanus$patch
# there should probably be a cleaner way to do this
present_data <- select(present_data, -olon, -olat, -index, -row, -col)

### data split
set.seed(409)
data_split <- initial_split(present_data, prob = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)
set.seed(NULL)

### recipe
# assigning id roles to longitude and latitude, manipulating data 
calanus_rec <-
  recipe(patch ~ ., data=train_data) |>
  update_role(lon, lat, new_role="ID") |>
  step_corr(all_numeric_predictors()) |> 
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  prep()
# use summary to view current set of variables and roles
summary(calanus_rec)

### model
calanus_rf <- rand_forest(trees=500, mode="classification") |>
  set_engine("ranger") 

### bundling model and recipe together into a workflow, fitting to data
calanus_fit <-
  workflows::workflow() |>
  add_model(calanus_rf) |>
  add_recipe(calanus_rec) |>
  fit(data=train_data)

### testing
calanus_testing <- augment(calanus_fit, test_data)
# displaying testing results
heat_map(conf_mat(calanus_testing, patch, .pred_class), 
         title="C. Finmarchicus prediction accuracy")

####### applying to RCP 8.5 2055 ######
# this is a separate process, requiring fitted workflow as input

### preparing input data set
#there must be an easier way of doing this
preproc_rec <- extract_preprocessor(calanus_fit)$last_term_info
predictors <- preproc_rec$role == "predictor"
vars <- preproc_rec$variable[predictors]
#reading in present data 
climate_pre <- brickman::read_brickman(scenario="PRESENT",
                                       vars=vars,
                                       interval="ann",
                                       form="stars")
#reading in future data 
climate_fut <- brickman::read_brickman(scenario="RCP85", 
                                       year=2055, 
                                       vars=vars, 
                                       add = climate_pre,
                                       interval="ann",
                                       form="tibble")
#initial data prep
climate_data <- climate_fut |>
  dplyr::rename(lon = x, lat = y) |>
  na.omit()

### predicting from dataset
climate_pred <- augment(calanus_fit, climate_data) |>
  st_as_sf(coords=c("lon", "lat"), crs=4326)
  
###### plotting results ######
plot(climate_pred['.pred_1'], 
     xlim = bb$xlim,
     ylim = bb$ylim,
     pch=15,
     cex=.3,
     axes=TRUE,
     main = "RCP8.5 2055 Calanus Presence")  

if(false){
#alternately, although this takes a while 
ggplot() + 
  geom_sf(data=climate_pred, mapping=aes(col=.pred_1)) 
  #coord_sf(xlim=bb$xlim, ylim=bb$ylim)
#plotting calanus
ggplot() + 
  geom_sf(data=calanus, mapping=aes(col=patch)) + 
  facet_wrap(~month, nrow=4)
}

#### Things to do 
# how to efficiently extract vars
# consistent file format rather than constant shifting?
# how to find variable importance 
# abstract into functions 
# double check that 85, 45 datasets are accurate
# results
#   save to folder
#   run for other climate scenarios
# model improvements
#   introduce folds 
#   ensemble model?

#### Timeline
# Figure out organization system
# Improve model accuracy
# Merge with Ecomon2
# Examine monthly datasets








  
  
  
