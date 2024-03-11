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
#' @param post function of bathymetry, correction factor
#' @param crop boolean, cropping data?
#' @return generates and saves prediction data. If data is monthly, generates
#'   a separate prediction file for each desired month. Returns a tibble for each 
#'   situation with boolean value representing if code ran without errors. 
get_predictions <- function(v,
                            verbose = FALSE,
                            save_months = 1:12,
                            save_scenarios = 1:4, 
                            downsample = c(0, 1, 2)[1],
                            post = NULL,
                            crop = FALSE) {
  
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
      
      if (crop) {
        mon_data <- mon_data |>
          filter(lon >= -77 & lon <= -42.5 & lat >= 36.5 & lat <= 56.7)
      }
      
      if (!is.null(post)) {
        predict_month <- function(wkf, data) {
          predict(wkf, data, type = "prob") |>
            bind_cols(select(data, lon, lat, month, Bathy_depth)) |>
            mutate(.pred_1 = .pred_1 * post(Bathy_depth),
                   .pred_0 = 1 - .pred_1,
                   .pred_class = (.pred_1 > .5) |> as.numeric() |> as.factor())
        }
      } else {
        predict_month <- function(wkf, data) {
          predict(wkf, data, type = "prob") |>
            bind_cols(select(data, lon, lat, month)) |>  
            mutate(.pred_class = (.pred_1 > .5) |> as.numeric() |> as.factor())
        }
      }

      # using input data to generate predictions
      preds <- predict_month(wkf, mon_data)
      
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

 #' Generates and save quantiles for a given version based on K duplicates
get_quantile_data <- function(v, data, 
                              K = 100, 
                              save_scenarios = 1:4,
                              save_months = 1:12,
                              downsample = 3,
                              desired_quants = c(0, .025, .25, .5, .75, .975, 1),
                              post = NULL,
                              crop = FALSE,
                              verbose = TRUE) {

  # Acquiring workflow list
  if (verbose) {message("Beginning workflows training")}
  folds <- data |>
    mc_cv(prop = .75, times = K, strata = patch)
  
  wkf <- get_v_wkf(v)
  wkfs <- folds$splits |>
    map(~fit(wkf, training(.x)))
  
  ###### Retrieving prediction data
  if (verbose) {message("Beginning Brickman Retrieval")}
  
  brickman_vars <- read_config(v)$training_data$brickman_data$vars
  
  bathy <- NULL
  not_bathymetry <- !brickman_vars %in% "Bathy_depth"
  mon_vars <- brickman_vars[not_bathymetry]
  
  if (!all(not_bathymetry)) {
    bathy <- brickman::read_brickman(scenario = "PRESENT", 
                                     vars = "Bathy_depth", 
                                     interval = "ann", 
                                     form = "stars")
  }
  
  present_data <- brickman::read_brickman(scenario="PRESENT", 
                                          vars = mon_vars, 
                                          interval = "mon", 
                                          form = "stars")
  
  if (verbose) {message("Finished Brickman Retrieval")}
  
  ###### Helper function to process climate scenario
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
      
      if (crop) {
        mon_data <- mon_data |>
          filter(lon >= -77 & lon <= -42.5 & lat >= 36.5 & lat <= 56.7)
      }

      # WE NEED TO PREDICT ACROSS ALL MODELS WITHIN SET
      # EACH MODEL SHOULD BE ONE COLUMN IN OUTPUT DATASET
      get_wkf_column <- function(wkf) {
        predict(wkf, mon_data, type = "prob") |>
          select(.pred_1)
      }
      
      wkf_quantiles <- wkfs |>
        lapply(get_wkf_column) |>
        bind_cols() |>
        suppressMessages() |>
        apply(1, function(x) quantile(x, 
                                      probs = desired_quants)) |>
        t() |>
        as_tibble() |>
        bind_cols(select(mon_data, lat, lon, month))
      
      if (!is.null(post)) {
        wkf_quantiles <- wkf_quantiles |>
          bind_cols(select(mon_data, Bathy_depth)) |>
          mutate(across(c(-lat, -lon, -month), 
                        function(x) {x * post(Bathy_depth)})) |>
          select(-Bathy_depth)
      }
      
      ### Saving to file
      file <- file.path(path, paste0("quant_preds", mon, ".csv.gz"))
      readr::write_csv(wkf_quantiles, file)
      if (verbose) {message(file)}
      
      TRUE
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
  
  # processing each scenario
  success_table <- save_scenarios |>
    climate_table() |>
    mutate(results = list(process_scenario(year, scenario)))
  
  # returning success table
  success_table
}

