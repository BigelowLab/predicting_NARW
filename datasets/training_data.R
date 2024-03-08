
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
                     date_start = "1990-01-01",
                     date_end = "2015-12-31",
                     threshold = 10000,
                     species = c("C. Finmarchicus", 
                                 "C. Hyperboreus",
                                 "C. Glacialis")[1],
                     vertical_correction = c(TRUE, FALSE)[1],
                     add_source = c("gill", "pelagic", "carnegie")[NULL]) {
  
  if (interval == "ann") {stop("Annual data retrieval not supported")}
  
  wd <- getwd()
  setwd("/mnt/ecocast/projectdata/calanusclimate/src")
  
  filename <- list("C. Finmarchicus" = ifelse(vertical_correction, 
                                              "vertcorr_merged_ae.csv.gz", 
                                              "ecomon_azmp_brickman.csv.gz"),
                   "C. Hyperboreus" = "hyperboreus_a.csv.gz", # CHANGE
                   "C. Glacialis" = "glacialis_a.csv.gz")[species] 
  
  # reading in initial dataset
  data <- filename[[1]] |>
    readr::read_csv(col_types = readr::cols()) |>
    # filtering dates
    filter_dates(date_start, date_end)
  
  # add additional datasets 
  for (file in add_source) {
    file_data <- readr::read_csv(paste0(file, ".csv.gz"), 
                                 col_types = readr::cols())
    data <- data |>
      bind_rows(file_data)
  }
  
  # mutating select variables to factor 
  data <- data |>
    mutate(patch = (abundance >= threshold) |> as.numeric(),
           across(c("patch", "month", "season", "src", "land_mask"), as.factor))
  
  data <- data |>
    select(patch, month, lat, lon, all_of(vars))
  
  setwd(wd)
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
  add_source <- sources[!sources %in% c("ecomon", "azmp")]
  
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
           threshold = species$threshold,
           species = species$species,
           vertical_correction = species$vertical_correction,
           add_source = add_source)
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

# code to create the vertically corrected merged dataset
if (FALSE) {
  library(brickman)
  
  setwd("/mnt/ecocast/projectdata/calanusclimate/src")
  ecomon <- readr::read_csv("vertical_correction_ecomon.csv.gz")
  azmp <- azmpcfin::read_calanus()
  
  # altering both datasets to have like columns 
  final_vars <- c("src", "id", "date", "longitude", "latitude", 
                  "depth", "abundance")
  
  ecomon <- ecomon |>
    mutate(src = "ecomon", 
           id = sprintf("%s_%i", cruise_name, station)) |>
    rename(depth = "sta_depth", abundance = "corrected_CIV_CVI_m2") |>
    select(all_of(final_vars))
  
  azmp <- azmp |>
    mutate(src = "azmp",
           id = sprintf("%s_%s", transect, station),
           date = as.Date(sprintf("%0.4i-%0.2i-%0.2i", year, month, day)),
           abundance = calanus_finmarchicus_iv + calanus_finmarchicus_v + 
             calanus_finmarchicus_vi) |>
    dplyr::select(dplyr::all_of(final_vars)) 
    
  # adding season, month columns 
  merged <- dplyr::bind_rows(ecomon, azmp)
  
  szn <- seq(from = as.Date("1970-12-01"), to = Sys.Date(), by = "3 months")
  szn_names <- rep(1:4, times = length(szn))
  ix <- findInterval(merged$date, szn)
  
  merged <- merged |>
    mutate(season = szn_names[ix],
           month = as.numeric(format(date, "%m"))) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # extracting brickman datapoints to match calanus
  data <- extract_points(compose_filename("PRESENT"), 
                         query_layers(scenario = "PRESENT"),
                         merged, 
                         complete = TRUE,
                         simplify_names = FALSE) |> 
    bind_cols(merged) |>
    select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    mutate_at(mon_vars(), ~.x[[month]]) |>
    ungroup()
  
  readr::write_csv(data, "vertcorr_merged_ae.csv.gz")
  
}

# hyperboreus dataset?
# creating hyperboreus data
if (FALSE) {
  root <- "/mnt/ecocast/projectdata/calanusclimate/src"
  
  setwd("/mnt/ecocast/projects/calanus/calanus-nwa/scripts")
  source("setup.R")
  hyp <- merge_calanus(e = ecomon::read_ecomon(simplify = FALSE), 
                       e_vars = "hyper_m2", 
                       a_vars = c("calanus_hyperboreus_iv", 
                                  "calanus_hyperboreus_v", 
                                  "calanus_hyperboreus_vi")) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  setwd(root)
  
  brickman_source <- brickman::compose_filename("PRESENT")
  brickman_vars <- brickman::query_layers(scenario = "PRESENT")
  
  data <- brickman::extract_points(brickman_source, 
                                   brickman_vars, 
                                   hyp, 
                                   complete = TRUE,
                                   simplify_names = FALSE) |> 
    bind_cols(hyp) |>
    select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    # mon_vars from setup.R
    mutate_at(mon_vars(), ~.x[[month]]) |>
    ungroup()
  
  # saving to file 
  readr::write_csv(data, file = "hyperboreus_ae.csv.gz")
}

# hyperboreus dataset?
# creating hyperboreus data azmp only
if (FALSE) {
  setwd("/mnt/ecocast/projectdata/calanusclimate/src")
  
  x <- dplyr::mutate(azmpcfin::read_calanus(),
                     src = "azmp",
                     id = sprintf("%s_%s", .data$transect, .data$station),
                     date = as.Date(sprintf("%0.4i-%0.2i-%0.2i", .data$year, .data$month,.data$day)),
                     .before = 1) |>
    dplyr::mutate(abundance = (.data$calanus_finmarchicus_iv + 
                                 .data$calanus_finmarchicus_v + 
                                 .data$calanus_finmarchicus_vi)) |>
    dplyr::select(dplyr::all_of(c("src", "id", "date", "longitude", "latitude", 
                                  "depth", "abundance"))) 
  
  szn <- seq(from = as.Date("1970-12-01"), to = Sys.Date(), by = "3 months")
  szn_names <- rep(1:4, times = length(szn))
  ix <- findInterval(x$date, szn)
  
  x <- dplyr::mutate(x,
                     season = szn_names[ix],
                     month = as.numeric(format(.data$date, "%m")),
                     .after = dplyr::all_of("date"))
  
  if (tolower("sf") == "sf"){
    x <- sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326)
  }
  x
  
  brickman_source <- brickman::compose_filename("PRESENT")
  brickman_vars <- brickman::query_layers(scenario = "PRESENT")
  
  data <- brickman::extract_points(brickman_source, 
                                   brickman_vars, 
                                   x, 
                                   complete = TRUE,
                                   simplify_names = FALSE) |> 
    bind_cols(x) |>
    select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    # mon_vars from setup.R
    mutate_at(mon_vars(), ~.x[[month]]) |>
    ungroup()
  
  # saving to file 
  readr::write_csv(data, file = "hyperboreus_a.csv.gz")
}

# glacialis dataset
if (FALSE) {
  root <- "/mnt/ecocast/projectdata/calanusclimate/src"
  
  setwd("/mnt/ecocast/projects/calanus/calanus-nwa/scripts")
  source("setup.R")
  # code copied from merge.R
  a_vars = c("calanus_glacialis_iv", 
             "calanus_glacialis_v", 
             "calanus_glacialis_vi")
  a <- azmpcfin::read_calanus()
  a <- dplyr::mutate(a,
                     src = "azmp",
                     id = sprintf("%s_%s", .data$transect, .data$station),
                     date = as.Date(sprintf("%0.4i-%0.2i-%0.2i", 
                                            .data$year, .data$month,.data$day)),
                     .before = 1) |>
    dplyr::mutate(abundance = dplyr::select(a, dplyr::any_of(a_vars)) |>
                    rowSums(na.rm = TRUE)) |>
    dplyr::rename_with(function(x) substring(x, 1, 3), 
                       dplyr::contains("itude")) |>
    dplyr::select(dplyr::all_of(c("src", "id", "date", "lon", "lat", 
                                  "depth", "abundance"))) 
  szn <- seq(from = as.Date("1970-12-01"), to = Sys.Date(), by = "3 months")
  szn_names <- rep(1:4, times = length(szn))
  ix <- findInterval(a$date, szn)
  a <- dplyr::mutate(a,
                     season = szn_names[ix],
                     month = as.numeric(format(.data$date, "%m")),
                     .after = dplyr::all_of("date"))
  a <- a |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  setwd(root)
  
  brickman_source <- brickman::compose_filename("PRESENT")
  brickman_vars <- brickman::query_layers(scenario = "PRESENT")
  
  data <- brickman::extract_points(brickman_source, 
                                   brickman_vars, 
                                   a, 
                                   complete = TRUE,
                                   simplify_names = FALSE) |> 
    bind_cols(a) |>
    select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    # mon_vars from setup.R
    mutate_at(mon_vars(), ~.x[[month]]) |>
    ungroup()
  
  # saving to file 
  readr::write_csv(data, file = "glacialis_a.csv.gz")
}


