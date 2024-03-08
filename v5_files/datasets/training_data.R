
###### THRESHOLD METHODS
base_slope_tm <- function(base = 40000*195, slope = 100*195) {
  function(Bathy_depth) {
    pmax(Bathy_depth - 300, 0) * slope + base
  }
}

flat_tm <- function(value = 40000*195) {
  function(Bathy_depth) {value}
}

stephane_prelim <- function(post = FALSE) {
  function(Bathy_depth) {
    max_prey_density <- 0.147235074555841 + 
      0.980070557219308 * exp(-0.0101656880852439 * Bathy_depth)
    
    foraging_time <- 
      0.468771131602175 + (1.01254035661976 - 0.468771131602175)/
      (1 + exp(0.0162983129075121 * (Bathy_depth - 306.362620087218)))
    
    combined_effects <- max_prey_density * foraging_time
    
    if (post) {
      combined_effects # factor to multiply with .pred_1
    } else {
      (30000*195)/combined_effects # effect on flat threshold
    }
  }
}

get_foraging_table <- function(state_val = c("rest", "preg")[1]) {
  og <- read.table("datasets/Time_spent_foraging.txt", header = TRUE) |>
    filter(state == state_val, param == "mean") |>
    na.omit()
  
  fullvalue <- og[1, 2]
  
  og |>
    transmute(depth.layer,
              foraging.correction = Time.foraging/fullvalue)
}

stephane_final <- function(state_val = c("rest", "preg")[1],
                           post = TRUE) {
  function(Bathy_depth) {
    max_prey_density <- 0.147235074555841 + 
      0.980070557219308 * exp(-0.0101656880852439 * Bathy_depth)
    
    Bathy_indices <- round(pmin(1000, pmax(10, Bathy_depth))/10, 0)
    
    foraging_time <- get_foraging_table(state_val)[Bathy_indices, 2]
    
    combined_effects <- max_prey_density * foraging_time #combined effects
    
    if (post) {
      combined_effects # factor to multiply with .pred_1
    } else {
      (30000*195)/combined_effects # effect on flat threshold
    }
  }
}

#' Retrieves training data for a model
#'  
#'  @param vars chr, the names of brickman variables to be retrieved
#'  @param threshold function accepting Bathy_depth argument and returning int
#'  @param species chr, species 
#'  @return a tibble with lat, lon, month, patch, and desired brickman vars  
get_data <- function(vars = c("Bathy_depth", "MLD", "Sbtm", "SSS", 
                              "SST", "Tbtm", "U", "V"),
                     threshold_method,
                     species = c("C. finmarchicus", 
                                 "C. hyperboreus",
                                 "C. glacialis")[1]) {
  
  ogdata <- get_species_raw(species, filter = TRUE)
  
  data <- ogdata |>
    mutate(patch = (dry_weight >= threshold_method(Bathy_depth)) |>
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
                             threshold_method,
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
           threshold = threshold_method,
           species = species$species)
}

#' Retrieves additional variables as desired and binds them 
#'   to testing results for analysis
#'   
#' @param v chr, version
#' @param desired_vars chr, list of desired variables to add to testing set
#' @return df, testing dataset with additional variables added 
retrieve_vars <- function(v, desired_vars, threshold_method) {
  base <- get_testing(v)
  config <- read_config(v)
  
  # retrieving matching data by reproducing split 
  set.seed(config$model$seed)
  config$training_data |>
    data_from_config(vars_override = desired_vars,
                     threshold_method = threshold_method) |>
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


# plotting stephane_prelim
if (FALSE) {
  bathy <- brickman::read_brickman(scenario = "PRESENT", 
                                   vars = c("Bathy_depth", "land_mask"), 
                                   interval = "ann", 
                                   form = "stars")
  
  brickman <- bathy |>
    st_downsample(n = 1) |>
    as_tibble() |>
    rename(lon = x, lat = y) |>
    filter(land_mask == 1) |>
    na.omit()
  
  brickman <- brickman |>
    mutate(post_factor = post(Bathy_depth))
  
  plot_ae(brickman, plot_col = "post_factor", title = "Altering coefficient")
}


















