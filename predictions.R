source("setup.R")

##### MAIN FUNCTION 

#' Generates and saves prediction data for a given version.
#' 
#' @param v chr, the version to be run
#' @param verbose boolean, if execution should produce messages
#' @param save_months vct, list of integers corresponding to which months to 
#'   save. Ignored if processing an annual model.
#' @param save_scenarios numeric vct, list of integers btwn 1 and 4
#'   corresponding to which climate predictions should be saved. Present 
#'   predictions are automatically included. 
#' @param downsample int, downsample factor each xth row will be kept
#' @return generates and saves prediction data. If data is monthly, generates
#'   a separate prediction file for each desired month. Returns a tibble for each 
#'   situation with boolean value representing if code ran without errors. 
get_predictions <- function(v,
                            verbose = FALSE,
                            save_months = 1:12,
                            save_scenarios = 1:4, 
                            downsample = c(0, 1, 2)[1],
                            augment_preds = FALSE) {
  
  if (verbose) {message("Beginning Brickman Retrieval")}
  
  # retrieving variables and workflow
  vars <- read_config(v)$training_data$brickman_data$vars
  wkf <- get_v_wkf(v)
  
  # monthly interval requires static/non-static separation
  bathy <- NULL
  not_bathymetry <- !vars %in% "Bathy_depth"
  
  if (!all(not_bathymetry)) {
    bathy <- brickman::read_brickman(scenario = "PRESENT", 
                                     vars = "Bathy_depth", 
                                     interval = "ann", 
                                     form = "stars")
  }
  
  # creating new list of variables omitting bathymetry
  mon_vars <- vars[not_bathymetry] 
  
  # retrieving present base dataset
  present_data <- brickman::read_brickman(scenario="PRESENT", 
                                          vars = mon_vars, 
                                          interval = "mon", 
                                          form = "stars")
  
  if (verbose) {message("Finished Brickman Retrieval")}
  
  process_scenario <- function(year, scenario) {
    
    # initializing file location, if necessary
    path <- pred_path(v, year, scenario)
    if (!dir.exists(path)) {
      dir.create(path, recursive=TRUE)
    }
    
    # helper that generates predictions for a month
    process_month <- function(mon) {
      
      # taking brickman data and slicing to desired month/merging with static
      mon_data <- c(brickman_data |> dplyr::slice(month, mon),
                    bathy) |>
        st_downsample(n = downsample) |>
        as_tibble() |>
        rename(lon = x, lat = y) |>
        mutate(month = as.factor(mon)) |>
        na.omit()
      
      non_augment <- function(wkf, data) {
        predict(wkf, data, type = "prob") |>
          bind_cols(select(data, lon, lat, month)) |>  
          mutate(.pred_class = (.pred_1 > .5) |> as.numeric() |> as.factor())
      }
      # using input data to generate predictions
      preds <- ifelse(augment_preds, augment, non_augment)(wkf, mon_data)
      
      file <- file.path(path, paste0("predictions", mon, ".csv.gz"))
      readr::write_csv(preds, file)
      if (verbose) {message(file)}
      
      preds
    }
    
    # retrieving data
    if (scenario == "PRESENT") {
      brickman_data <- present_data
    } else {
      brickman_data <- brickman::read_brickman(scenario = scenario, 
                                               year = year, 
                                               vars = mon_vars, 
                                               add = present_data,
                                               interval = "mon",
                                               form = "stars")
    }
    
    # returning prediction list
    list("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, 
         "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8,
         "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)[save_months] |>
      lapply(process_month)
    
    TRUE
  }
  
  # using chosen helper function to process brickman data
  success_table <- save_scenarios |>
    climate_table() |>
    mutate(results = list(process_scenario(year, scenario)))
  
  # returning success table
  success_table
}
