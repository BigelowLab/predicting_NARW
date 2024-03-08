suppressPackageStartupMessages(
  {
    library(dplyr) #data transformation
    library(sf) #simple features -- storing and managing georeferences
    library(stars) #spatial data
    library(calanusthreshold) #calanus data
    library(brickman) #brickman data
    library(ncdf4) #query 
  })

## idea: for future data functions allow desired save name to be optionally 
##  passed in as an argument

#' Retrieves specified variables from brickman to match azmp calanus data.
#'
#' @param vars the desired variables to retrieve
#' @return a tibble of specified variables for the azmp data
get_data <- function(vars) {
  root <- "/mnt/ecocast/projectdata/students/ojohnson/brickman"
  calanus <- readr::read_csv(file.path(root, "calanus/cfin_patch_10000.csv.gz")) |> 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  get_climate_vars(calanus, vars, scenario="PRESENT")
}

#' Retrieves and saves variables froms specified climate dataset that correspond
#'   to geographic points from a given sf object
#'
#' @param calanus_sf the calanus data from which geographic information will 
#'   be extracted
#' @param vars the list of variable names to be extracted from the brickman 
#'   dataset -> "ann" can subsitute for all annual variables
#' @param scenario the desired climate scenario
#' @param year the desired climate year
#' @return A tibble of the specified variables from the desired climate situation, 
#'   where each point corresponds to a geographic calanus datapoint
get_climate_vars <- 
  function(calanus_sf, vars,
           scenario = c("RCP45", "RCP85", "PRESENT")[1],
           year = c(2055, 2075, NA)[1]) {
    
    source_file <- brickman::compose_filename(scenario, year)
    ## producing save file name 
    save_file <- scenario
    if(!(scenario == "PRESENT")) {
      save_file <- paste(save_file, year, sep="_")
    } 
    save_file <- paste(save_file, paste(vars, collapse=""), sep="_")
    save_file <- paste0("obs/", save_file, "_azmp.csv.gz")
  
    ## if vars contains "ann", add all annual variables (excluding bathymetry)
    if (is.element("ann", vars)) { 
      nm <- query_layers(scenario = scenario, year=year)
      ix <- grepl("_ann", nm, fixed=TRUE)
      vars <- c(vars[!is.element(vars, "ann")], nm[ix])
    }
    
    #actually extracting the points using brickman::extract_points
    variables <- brickman::extract_points(source_file, 
                                          vars, 
                                          calanus_sf, 
                                          complete = TRUE,
                                          simplify_names = TRUE) |> 
      select(-olon, -olat, -index, -row, -col) |>
      dplyr::mutate(patch = calanus_sf$patch) |>
      readr::write_csv(file = get_path(save_file))
    
    #returning tbl of variables 
    message(paste("Save file name: ", save_file))
    variables
  }





