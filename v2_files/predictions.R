source("setup.R")

######## HELPER FUNCTIONS 

#' Retrieves Brickman data for the given scenario and year. 
#' 
#' @param vars list, the desired variables to retrieve
#' @param year int, the desired climate year 
#' @param scenario chr, the desired climate scenario
#' @param interval chr, whether retrieving monthly or annual variables
#' @param add stars, present climate data to be added to output
#' @return Brickman data for the desired climate situation. 
get_brickman <- function(vars,
                         year=c(2055, 2075)[1], 
                         scenario=c("RCP45", "RCP85", "PRESENT")[1],
                         interval = c("ann", "mon")[1],
                         add = NULL) {
  
  if (scenario == "PRESENT") {
    base <- add
  } else {
    base <- brickman::read_brickman(scenario = scenario, 
                                    year = year, 
                                    vars = vars, 
                                    add = add,
                                    interval = interval,
                                    form="stars")
  }
  
  # applying necessary conversions based on monthly or yearly
  if (interval == "ann") {
    base |>
      as_tibble() |>
      rename(lon = x, lat = y) |>
      na.omit()
  } else {
    base
  }
}

#' Saves predictions to the file system.
#' 
#' @param pred tibble, the climate prediction data being saved 
#' @param v chr, the version being saved
#' @param year int, the climate year being saved
#' @param scenario chr, the climate scenario being saved
#' @param save_name chr, the name to of saved file
#' @param verbose boolean, whether to print save name
#' @return prediction data
save_pred <- function(pred,
                      v, 
                      year=c(2055, 2075)[1], 
                      scenario=c("RCP45", "RCP85", "PRESENT")[1], 
                      save_name = "predictions.csv.gz",
                      verbose = FALSE) {
  #constructing path
  path <- pred_path(v = v, 
                    year = year, 
                    scenario = scenario)
  pred_file <- file.path(path, save_name)
  
  if (!dir.exists(path)) {
    dir.create(path, recursive=TRUE)
  }
  readr::write_csv(pred, pred_file)
  
  if (verbose) {message(pred_file)}
  
  pred
}

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
                            downsample = c(0, 1, 2)[1]) {
  
  # retrieving configuration information
  config <- read_config(v)$training_data$brickman_data
  vars <- config$vars
  interval <- config$interval
  
  # retrieving calanus workflow
  calanus_wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz"))
  
  # monthly interval requires static/non-static separation
  static <- NULL
  if (interval == "mon") {
    v_mon <- vars %in% mon_vars()
    
    # if there are static variables, read them in separately
    if (!all(v_mon)) {
      static <- brickman::read_brickman(scenario="PRESENT", 
                                        vars = vars[!v_mon], 
                                        interval = "ann", 
                                        form = "stars")
      
      vars <- vars[v_mon]
    }
  }
  
  # retrieving present base dataset
  add <- brickman::read_brickman(scenario="PRESENT", 
                                 vars = vars, 
                                 interval = interval, 
                                 form = "stars")
  
  # Defining two helper functions - one to process monthly data, one for annual
  # both functions return TRUE if ran successfully 
  
  # annual -- helper that calls predict_cfin and save_pred
  process_ann <- function(brickman, year, scenario) {
    calanus_wkf |> 
      augment(brickman) |>
      save_pred(v, year, scenario, verbose)
    
    return(TRUE)
  }
  
  # monthly -- helper to run a separate prediction per each month
  process_months <- function(brickman, year, scenario) {
    
    is_wkf_list <- !is_workflow(calanus_wkf)
    
    for (mon in save_months) {
      # taking brickman data and slicing to desired month/merging with static
      mon_pred <- c(brickman |> dplyr::slice(month, mon),
                    static) |>
        # downsampling, if desired
        st_downsample(n = downsample) |>
        # converting to tibble and making necessary modifications
        as_tibble() |>
        rename(lon = x, lat = y) |>
        mutate(month = as.factor(mon)) |>
        na.omit()
        
        # if there are separate workflows per month, retrieve that 
        mon_wkf <- (if(is_wkf_list) {calanus_wkf[[mon]]}
                   else {calanus_wkf})
        
        # generating patch predictions and saving to file
        mon_wkf |> 
          predict(mon_pred, type = "prob") |>
          bind_cols(select(mon_pred, month, lon, lat)) |>  
          # remove after fixes to GAM 
          mutate(.pred_0 = as.vector(.pred_0),
                 .pred_1 = as.vector(.pred_1),
                 .pred_class = (.pred_1 > .5) |> as.numeric() |> as.factor()) |>
          save_pred(v, 
                    year, 
                    scenario, 
                    save_name = paste0("predictions", mon, ".csv.gz"),
                    verbose)
    }
    
    return(TRUE)
  }
  
  # choosing which helper function we'll use 
  process_fun <- ifelse(interval == "ann", process_ann, process_months)
  
  # using chosen helper function to process brickman data
  success_table <- save_scenarios |>
    climate_table() |>
    mutate(success = get_brickman(vars, year, scenario, interval, add) |>
             process_fun(year, scenario))
  
  # returning success table
  success_table
}


get_diapause_predictions <- function(v,
                                     verbose = FALSE,
                                     save_months = 1:12,
                                     save_scenarios = 1:4, 
                                     downsample = c(0, 1, 2, 3)[1]) {
  
  # retrieving configuration information
  config <- read_config(v)$training_data$brickman_data
  vars <- config$vars
  interval <- config$interval
  
  # retrieving calanus workflow
  calanus_wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz"))
  
  # monthly interval requires static/non-static separation
  static <- NULL
  if (interval == "mon") {
    v_mon <- vars %in% mon_vars()
    
    # if there are static variables, read them in separately
    if (!all(v_mon)) {
      static <- brickman::read_brickman(scenario="PRESENT", 
                                        vars = vars[!v_mon], 
                                        interval = "ann", 
                                        form = "stars")
      
      vars <- vars[v_mon]
    }
  }
  
  # retrieving present base dataset
  add <- brickman::read_brickman(scenario="PRESENT", 
                                 vars = vars, 
                                 interval = interval, 
                                 form = "stars")
  
  # Defining two helper functions - one to process monthly data, one for annual
  # both functions return TRUE if ran successfully 
  
  # annual -- helper that calls predict_cfin and save_pred
  process_ann <- function(brickman, year, scenario) {
    calanus_wkf |> 
      augment(brickman) |>
      save_pred(v, year, scenario, verbose)
    
    return(TRUE)
  }
  
  # monthly -- helper to run a separate prediction per each month
  process_months <- function(brickman, year, scenario) {
    
    for (mon in save_months) {
      # taking brickman data and slicing to desired month/merging with static
      mon_pred <- c(brickman |> dplyr::slice(month, mon),
                    static) |>
        # downsampling, if desired
        st_downsample(n = downsample) |>
        # converting to tibble and making necessary modifications
        as_tibble() |>
        rename(lon = x, lat = y) |>
        mutate(month = as.factor(mon)) |>
        na.omit()
      
      # if there are separate workflows per month, retrieve that 
      mon_wkf <- (if(mon > 6) {calanus_wkf[[2]]}
                  else {calanus_wkf[[1]]})
      
      # generating patch predictions and saving to file
      mon_wkf |> 
        augment(mon_pred) |>
        # remove after fixes to GAM 
        mutate(.pred_0 = as.vector(.pred_0),
               .pred_1 = as.vector(.pred_1)) |>
        save_pred(v, 
                  year, 
                  scenario, 
                  save_name = paste0("predictions", mon, ".csv.gz"),
                  verbose)
    }
    
    return(TRUE)
  }
  
  # choosing which helper function we'll use 
  process_fun <- ifelse(interval == "ann", process_ann, process_months)
  
  # using chosen helper function to process brickman data
  success_table <- save_scenarios |>
    climate_table() |>
    mutate(success = get_brickman(vars, year, scenario, interval, add) |>
             process_fun(year, scenario))
  
  # returning success table
  success_table
}

# old code
if (FALSE) {
  #' Generates calanus probability predictions for a desired climate scenario
  #' 
  #' @param brickman tibble, input climate data 
  #' @param calanus_wkf workflow or list of workflows, use to predict data
  #' @return tibble, brickman data with .pred_class, .pred_1, and .pred_0 
  predict_cfin <- function(brickman, calanus_wkf) {
    # is the calanus workflow a single model or a list of multiple?
    if (is(calanus_wkf, "workflow")) {
      return(augment(calanus_wkf, brickman))
      
    } else { # predicting for each workflow and merging results
      climate_pred <- calanus_wkf |> 
        #generating a big table of predictions
        map(~predict(.x, brickman, type="prob")) |>
        bind_cols() |>
        select(starts_with(".pred_1")) |>
        #finding mean of predictions
        rowMeans()
      mutate(brickman, .pred_class = as.numeric(climate_pred > 0), 
             .pred_0 = 1 - climate_pred,
             .pred_1 = climate_pred)
    }
  }
  
  # code for a 1 file predictions monthly format
  if (FALSE) {
    get_brickman_modded <- function(vars, 
                                    add, 
                                    year=c(2055, 2075)[1], 
                                    scenario=c("RCP45", "RCP85", "PRESENT")[1],
                                    interval = c("ann", "mon")[1],
                                    static = NULL) {
      
      # if retrieving present data, convert existing add object
      if (scenario == "PRESENT") {
        base <- add |>
          as_tibble() |>
          dplyr::rename(lon = x, lat = y)
      } else {
        # want to retrieve variables that match add object
        base <- brickman::read_brickman(scenario = scenario, 
                                        year = year, 
                                        vars = vars, 
                                        add = add,
                                        interval = interval,
                                        form="tibble") |> 
          dplyr::rename(lon = x, lat = y)
      }
      
      # if there are static variables to add, add them now
      if (!is.null(static)) {
        base <- base |> 
          bind_cols(static) |>
          mutate(month = as.factor(month))
      }
      
      # remove null values
      na.omit(base)
    }
    
    get_predictions_new <- function(version = "v1.30",
                                    calanus_wkf = NULL) {
      
      # retrieving configuration information
      config <- read_config(version)$training_data$brickman_data
      interval <- config$interval
      vars <- config$vars
      
      # retrieving and analyzing calanus workflow 
      if (is.null(calanus_wkf)) {
        calanus_wkf <- readRDS(v_path(version, "model", "model_fit.csv.gz"))
      }
      
      # defining static as null - override later if needed 
      static <- NULL
      
      # monthly interval requires static/non-static separation
      if (interval == "mon") {
        v_mon <- vars %in% mon_vars()
        
        # if there are static variables, read them in separately
        if (!all(v_mon)) {
          static <- brickman::read_brickman(scenario="PRESENT", 
                                            vars = vars[!v_mon], 
                                            interval = "ann", 
                                            form = "tibble") |>
            select(-x, -y) |>
            dplyr::slice(rep(1:n(), 12))
          
          vars <- vars[v_mon]
        }
      }
      
      # retrieving present base dataset
      add <- brickman::read_brickman(scenario="PRESENT", 
                                     vars = vars, 
                                     interval = interval, 
                                     form = "stars")
      
      # generating table of predictions 
      climate_preds <- climate_table() |>
        mutate(climate_pred = list(get_brickman_modded(vars, add, year, scenario, 
                                                       interval, static) |> 
                                     predict_cfin(calanus_wkf)),
               path = save_pred(version, climate_pred, year, scenario))
    }
  }
  
  # code for previous purely annual predictions workflow 
  if (FALSE) {
    #' Generates and saves annual prediction data from a given model for all climate 
    #'   situations. 
    #'   
    #' @param version the model version to use when generating predictions
    #' @param calanus_wkf the workflow to be predicted -- read from file if NULL
    #' @return a data frame, with columns for year, scenario, 
    #'   model predictions, and file name of the save path. 
    get_predictions <- function(version="v1.00",
                                calanus_wkf = NULL) {
      # retrieving configuration information
      config <- read_config(version)
      vars <- config$training_data$brickman_data$vars
      
      if (is.null(calanus_wkf)) {
        calanus_wkf <- readRDS(v_path(version, "model", "model_fit.csv.gz"))
      }
      
      # read in present climate data
      add <- brickman::read_brickman(scenario="PRESENT",
                                     vars=vars,
                                     interval="ann",
                                     form="stars")
      
      # constructing data frame of prediction scenarios (plus present)
      climate_preds <- climate_table() |>
        mutate(climate_pred = list(get_brickman(vars, year, scenario, add) |>
                                     predict_cfin(calanus_wkf)),
               path = save_pred(version, climate_pred, year, scenario))
    }
  }
  
  # alternative implementation of get_predictions
  if (FALSE) {
    # retrieving base dataset
    brickman_data <- climate_table() |>
      mutate(brickman = list(get_brickman(vars, year, scenario, 
                                          interval, add)))
    
    if (interval == "ann") {
      # annual - simply run predictions and save 
      success_table <- brickman_data |>
        transmute(year, scenario, 
                  success = brickman |> 
                    predict_cfin(calanus_wkf) |>
                    save_pred(v, year, scenario))
    } else {
      # monthly -- define helper to run a separate prediction per each month
      process_months <- function(brickman, 
                                 year, 
                                 scenario,
                                 verbose = FALSE) {
        
        split_base <- split(brickman, brickman$month)
        
        for (mon in 1:12) {
          mon_pred <- split_base[mon] |>
            bind_cols(static) |>
            na.omit()
          
          mon_pred |> 
            predict_cfin(calanus_wkf) |>
            save_pred(v, 
                      year, 
                      scenario, 
                      save_name = paste0("predictions", mon, ".csv.gz"))
          
          if (verbose) {message(paste(year, scenario, mon))}
        }
        
        return(TRUE)
      }
      
      # using new function to run predictions 
      success_table <- brickman_data |>
        transmute(year, scenario, 
                  success = brickman |> 
                    process_months(year, scenario, verbose))
    }
  }
}
