
#' Retrieves training data for a model
#'  
#'  @param vars chr, the names of brickman variables to be retrieved
#'  @param interval chr, whether the data should be annual or monthly. 
#'    interval = ann currently not supported. 
#'  @param date_start chr, the starting date for retrieved 
#'    data or NULL if no starting date 
#'  @param date_end chr, the ending date for retrieved 
#'    data or NULL if no ending date 
#'  @param threshold int, the abundance count that defines a patch
#'  @param species chr, species 
#'  @param vertical_correction boolean, do we use Caroline's 
#'      vertical distribution correction? for Cfin only
#'  @param add_source chr, optional additional source files
#'  @return a tibble with lat, lon, month, patch, and desired brickman vars  
get_data <- function(vars = c("Bathy_depth", "MLD", "Sbtm", "SSS", 
                              "SST", "Tbtm", "U", "V"), 
                     interval = c("ann", "mon")[2],
                     date_start = "1990-01-01", # artifact
                     date_end = "2015-12-31", # artifact
                     base = 10000*264,
                     slope = 10*264,
                     species = c("C. finmarchicus", 
                                 "C. hyperboreus",
                                 "C. glacialis")[1],
                     vertical_correction = c(TRUE, FALSE)[2]) {
  
  if (interval == "ann") {stop("Annual data retrieval not supported")}
  
  ogdata <- get_species_raw(species, filter = TRUE)
  
  # performing threshold conversion
  threshold_method <- function(Bathy_depth, base, slope) {
    pmax(Bathy_depth - 300, 0) * slope + base
  }
  
  data <- ogdata |>
    mutate(patch = (dry_weight >= threshold_method(Bathy_depth, base, slope)) |>
             as.numeric(), 
           across(c("patch", "month", "src", "land_mask"), as.factor)) |>
    select(patch, month, lat, lon, all_of(vars))

  data
}

#' Retrieves training data based on the provided configuration
#' 
#' @param config a configuration for training data 
#' @return training data matching configuration
data_from_config <- function(training_data_config,
                             vars_override = NULL) {
  #splitting into two base groups
  brickman <- training_data_config$brickman_data
  species <- training_data_config$species_data
  
  # retrieving add sources
  sources <- species$source
  
  # vars_override? 
  if (is.null(vars_override)) {
    vars <- brickman$vars
  } else {
    vars <- vars_override
  }
  
  get_data(vars = vars, 
           interval = brickman$interval,
           date_start = species$date_range$start,
           date_end = species$date_range$end,
           base = species$base,
           slope = species$slope,
           species = species$species,
           vertical_correction = species$vertical_correction)
}

#' Retrieves additional variables as desired and binds them 
#'   to testing results for analysis
#'   
#' @param v chr, version
#' @param desired_vars chr, list of desired variables to add to testing set
#' @return df, testing dataset with additional variables added 
retrieve_vars <- function(v, desired_vars) {
  base <- get_testing(v)
  config <- read_config(v)
  
  # retrieving matching data by reproducing split 
  set.seed(config$model$seed)
  config$training_data |>
    data_from_config(vars_override = desired_vars) |>
    initial_split(prop = 3/4, strata = patch) |>
    testing() |>
    select(all_of(desired_vars)) |>
    bind_cols(base)
}


get_species_raw <- function(species = "C. finmarchicus", 
                            filter = TRUE) {
  root <- "/mnt/ecocast/projectdata/calanusclimate/src"
  filename <- list("C. finmarchicus" = "tc_datasets/dry_weight_ae.csv.gz",
                   "C. hyperboreus" = "tc_datasets/ae_dryweight_nooutliers_chyp.csv.gz",
                   "C. glacialis" = "glacialis_a.csv.gz")[species]
  
  d <- file.path(root, filename[[1]]) |> 
    readr::read_csv(col_types = readr::cols())
  
  if (filter) {d <- filter_dates(d, "1990-01-01", "2015-12-31")}
  
  d
}
























