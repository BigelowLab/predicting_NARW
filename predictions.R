##### MAIN FUNCTION 

# Helper that retrieves quantile predictions for specific wkfs and data
apply_quantile_preds <- function(wkfs, data, desired_quants) {
  get_wkf_column <- function(wkf) {
    predict(wkf, data, type = "prob") |>
      select(.pred_1)
  }
  
  pred_quantiles <- wkfs |>
    lapply(get_wkf_column) |>
    bind_cols() |>
    suppressMessages() |>
    apply(1, function(x) quantile(x, probs = desired_quants))
  
  if (length(desired_quants) > 1) {
    pred_quantiles |>
    t() |>
    as_tibble(.name_repair = "unique")
  } else {
    pred_quantiles |>
      as_tibble_col(paste0(desired_quants * 100, "%"))
  }
}

#' Generates and saves prediction data for a given version.
#' 
#' @param v chr, the version to be run
#' @param save_scenarios int, list of integers btwn 1 and 4
#'   corresponding to which climate predictions should be saved. Present 
#'   predictions are automatically included. 
#' @param save_months int, list of integers corresponding to which months to 
#'   save. Ignored if processing an annual model.
#' @param downsample int, downsample factor each xth row will be kept
#' @param desired_quants dbl, quantile percentiles to save
#' @param post function of bathymetry, correction factor
#' @param crop boolean, cropping data to study range?
#' @param verbose boolean, if execution should produce messages
#' @return generates and saves quantile prediction data. Returns a tibble for each 
#'   situation with boolean value representing if code ran without errors. 
get_quantile_preds <- function(v,
                               save_scenarios = 1:4,
                               save_months = 1:12,
                               downsample = 3,
                               desired_quants = c(0, .025, .25, .5, .75, .975, 1),
                               post = NULL,
                               crop = FALSE,
                               verbose = TRUE) {

  # Acquiring workflow list
  wkfs <- get_v_wkfs(v)
  
  ###### Retrieving Brickman data
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
          filter(lon >= -77 & lon <= -42.5 & lat >= 35.2 & lat <= 57.6)
      }
      
      wkf_quantiles <- apply_quantile_preds(wkfs, mon_data, desired_quants) |>
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
