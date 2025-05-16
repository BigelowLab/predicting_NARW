suppressPackageStartupMessages(
  {
    library(dplyr) # data transformation
    library(stars) # spatial data
    #library(sf) broken until further notice
    library(calanusthreshold) #calanus data
    library(brickman) # brickman data
    library(ncdf4) # querying data 
    library(tidymodels)
    library(yaml)
    library(purrr)
    library(gridExtra)
    library(bundle) # for reading in/out keras models
    library(grDevices) # for printing tau character to pdf
  })

### DATA HELPERS

# defining local function that will filter the data based on date 
# data must have a "date" column
filter_dates <- function(data, date_start, date_end) {
  if (!is.null(date_start)) {
    data <- filter(data, date >= as.Date(date_start))
  }
  if (!is.null(date_end)) {
    data <- filter(data, date <= as.Date(date_end))
  }
  data
}

### PREDICTION AND PLOT HELPERS

plot_ae <- function(data, plot_col = "patch", title = "Plot", size = .3) {
  ggplot(data, aes(x = lon, y = lat)) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    geom_point(aes(col = get(plot_col)), alpha = .7, size = size) +
    coord_quickmap(xlim = c(-76, -40), ylim = c(35, 60), expand = TRUE) +
    labs(col = plot_col) +
    theme_bw() + 
    ggtitle(title)
}

# Returns a list of month names
mon_names <- function() {
  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
}

# Returns a list of variable abbreviations
var_abb <- function() {
  list(Bathy_depth = "Bathymetry", 
       MLD = "Mixed layer depth", 
       SST = "Surface temperature", 
       Tbtm = "Bottom temperature", 
       Sbtm = "Bottom salinity", 
       SSS = "Surface salinity", 
       Vel = "Velocity", 
       month = "Month", 
       lat = "Latitude")
}

# converts a month number to the corresponding string 
as_month <- function(mon_num = NA) {
  
  if (is.na(mon_num)) {return("")}
  
  mon_names()[as.numeric(mon_num)]
}

# returns a list of monthly variables in Brickman dataset
mon_vars <- function() {
  c("Xbtm", "MLD", "Sbtm", "SSS", "SST", "Tbtm", "U", "V")
}

#' creates a data frame of all climate scenarios
#'
#' @param ... numerics, which climate scenarios should be retrieved
#'   if no argument is provided, all five scenarios are returned
#' @return a dataframe of all five climate scenarios
climate_table <- function(...) {
  y <- c(2055, 2075, 2055, 2075, NA)
  s <- c("RCP45", "RCP45", "RCP85", "RCP85", "PRESENT")
  
  data.frame(scenario = s, year = y) |>
    dplyr::slice(...) |>
    rowwise()
}

#' returns the file path to a desired climate prediction folder
#' 
#' @param v the version of the desired prediction
#' @param year the year of the desired prediction
#' @param scenario the scenario of the desired prediction
#' @param ... additional path specifiers
#' @return chr, the path to the desired prediction folder
pred_path <- function(v = "v1.00", 
                      year = c(2055, 2075)[1], 
                      scenario = c("RCP45", "RCP85", "PRESENT")[1],
                      ...) {
  #constructing path
  path <- file.path(v_path(v = v), "pred") 
  if (scenario != "PRESENT") {
    path <- file.path(path, year)
  }
  path <- file.path(path, scenario, ...)
  return(path)
}

### VERSION HELPERS
# lists out versions within a specific number file
available_versions <- function(vNum = "v5") {
  file.path("/mnt/ecocast/projectdata/students/ojohnson/brickman/versions",
            vNum) |>
    list.files()
}

#' Retrieves fitted workflows for desired version. 
get_v_wkfs <- function(v) {
  model_obj <- readRDS(v_path(v, "model", "model_fits.csv.gz"))
  
  if (any(class(model_obj) == "bundle")) {
    model_obj <- unbundle(model_obj)
  }
  
  model_obj
}

# retrieves testing data for a desired version. v3.00 and higher
get_testing <- function(v) {
  v_path(v, "model", "testing_results.csv.gz") |>
    readr::read_csv(col_types = readr::cols()) |>
    mutate(across(c("month", "patch", ".pred_class"), as.factor))
}

#' Parse a version string into subparts
#' Versions have format vMajor.Minor
#' 
#' @param v version string to parse
#' @return named character vector of version subparts
parse_version <- function(v = "v1.00") {
  vsplit = strsplit(v, '.', fixed=TRUE) |> unlist()
  c(major = vsplit[1], minor = vsplit[2])
}

#' Constructs a file path to given version folder
#' 
#' @param v model version
#' @param ... additional path specifiers
#' @param root root path to project dir
#' @return file path to version folder
v_path <- function(v = "v1.00", ..., 
                   root = "/mnt/ecocast/projectdata/students/ojohnson/brickman/versions") {
  major <- parse_version(v)["major"]
  file.path(root, major, v, ...)
}

### YAML HELPERS

#' Reads the yaml configuration for the given version
#' 
#' @param v the desired version
#' @return list of configuration values
read_config <- function(v = "v1.00") {
  yaml::read_yaml(v_path(v = v, paste0(v, ".yaml")))
}


#' Writes the given configuration to file
#' 
#' @param config the configuration list
#' @param overwrite whether to allow overwrite of existing files
#' @return list of config values
write_config <- function(config, 
                         overwrite = FALSE) {
  v <- config$version
  path = v_path(v)
  if (!dir.exists(path)) {
    okay <- dir.create(file.path(path), recursive = TRUE)
  }
  yaml_file <- file.path(path, paste0(v, ".yaml"))
  if(overwrite == FALSE && file.exists(yaml_file)) {
    stop('configuration already exists:', version)
  }
  
  yaml::write_yaml(config, yaml_file)
  return(config)
}





