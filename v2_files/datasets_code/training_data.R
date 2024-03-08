source("/mnt/ecocast/projects/students/ojohnson/brickman/v2_files/setup.R")
library(ecomon)
library(azmpcfin)

#' Retrieves training data for unstaged cfin sourced from both azmp
#'  and ecomon
#'  
#'  @param b_vars chr, the names of brickman variables to be retrieved
#'  @param interval chr, whether the data should be annual or monthly
#'  @param date_start chr, the starting date for retrieved 
#'    data or NULL if no starting date 
#'  @param date_end chr, the ending date for retrieved 
#'    data or NULL if no ending date 
#'  @param threshold int, the abundance count that defines a patch
#'     if null, return raw patch values
#'  @param source chr, primary species source file - merged azmp, ecomon
#'  @param add chr, additional source files for cfin to include
#'  @return a tibble with lat, lon, patch, and desired brickman vars   
get_ae_vars <- function(b_vars = c("Bathy_depth", "MLD", "Sbtm", "SSS", 
                                   "SST", "Tbtm", "U", "V"),
                        interval = c("ann", "mon")[1],
                        date_start = "1990-01-01",
                        date_end = "2015-12-31",
                        threshold = 10000,
                        source = c("cfin", "hyperboreus")[1],
                        add = NULL,
                        downsample = FALSE) {
  
  root <- "/mnt/ecocast/projectdata/calanusclimate/src"
  file <- list(cfin = "ecomon_azmp_brickman.csv.gz", 
               hyperboreus = "hyperboreus_ae.csv.gz")[source]
  # reading in initial dataset
  data <- readr::read_csv(file.path(root, file),
                          col_types = readr::cols()) |>
    # filtering dates
    filter_dates(date_start, date_end)
  
  # create patch variable 
  if (!is.null(threshold)) {
    data <- data |>
    # mutating abundance metric into patch value
    mutate(patch = (abundance > threshold) |> as.numeric() |> as.factor())
    
    if (downsample) {
      data <- (data |>
                 caret::downSample(data$patch, list = TRUE))$x
    }
  } else {
    data <- data |>
      rename(patch = abundance)
  }
  
  # add additional datasets 
  if (!is.null(add)) {
    for (file in add) {
      file_data <- readr::read_csv(paste0(root, "/", file, ".csv.gz"),
                                   col_types = readr::cols())
      data <- data |>
        bind_rows(file_data) |>
        mutate(patch = patch |> as.factor())
    } 
  }
  
  data <- data |>
    mutate_at(c("month", "season", "src", "land_mask"), as.factor)
  
  # modding variable names and selecting columns to keep 
  if (interval == "ann") { #annual
    b_vars_ann <- brickman::add_suffix(b_vars, "_ann")
    new_data <- select(data, patch, lat, lon, b_vars_ann)
    names(new_data)[-(1:3)] <- b_vars
    return (new_data)
    
  } else { # monthly
    data |>
      select(patch, month, lat, lon, all_of(b_vars))
  }
}

#' Retrieves training data based on the provided configuration
#' 
#' @param config a configuration for training data 
#' @return training data matching configuration
get_training_data <- function(config) {
  #splitting into two base groups
  brickman <- config$brickman_data
  species <- config$species_data
  
  # specifying action based on source for calanus
  if (setequal(species$source, c("ecomon", "azmp"))) {
    # call to get_ae_vars
    get_ae_vars(b_vars = brickman$vars,
                interval = brickman$interval,
                date_start = species$date_range$start,
                date_end = species$date_range$end,
                threshold = species$threshold)
  } else {
    stop("Error: source other than c('ecomon', 'azmp') currently not supported")
  }
}

# code to saved merged azmp/ecomon/brickman data 
if (FALSE) {
  # script to generate source dataset for merged ecomon and azmp data 
  root <- "/mnt/ecocast/projectdata/calanusclimate/src"
  
  setwd("/mnt/ecocast/projects/calanus/calanus-nwa/scripts")
  source("setup.R")
  merge_calanus() |>
    readr::write_csv(file = file.path(root, "merged_cfin.csv.gz"))
  
  # reading in calanus data
  ae <- readr::read_csv(file.path(root, "merged_cfin.csv.gz"), col_types = readr::cols()) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # defining brickman identifiers - variables, file
  brickman_source <- brickman::compose_filename("PRESENT")
  brickman_vars <- brickman::query_layers(scenario = "PRESENT")
  
  savefile <- file.path(root, "ecomon_azmp_brickman.csv.gz")
  
  # extracting brickman datapoints to match calanus
  data <- brickman::extract_points(brickman_source, 
                                   brickman_vars, 
                                   ae, 
                                   complete = TRUE,
                                   simplify_names = FALSE) |> 
    bind_cols(ae) |>
    select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    # mon_vars from setup.R
    mutate_at(mon_vars(), ~.x[[month]]) |>
    ungroup()
  
  # saving to file 
  readr::write_csv(data, file = savefile)
}

#### OTHER

get_unstaged_ecomon <- function(threshold = 10000) {
  #read in ecomon data and filter out data not matching brickman timeframe 
  # brickman timeframe is 1990 - 2015
  ecomon <- ecomon::read_staged(species = "calfin", 
                                form = "sf") |>
    filter(date < as.Date("2015-12-31") & date >= as.Date("1990-01-01")) |>
    transmute(patch = (total_m2 > threshold) |> as.numeric() |> as.factor())
}

# creating set of "dud" points
if(FALSE) {
  #20-30 deg North/South, 
  # 70 to 20 deg east/west
  
  lon <- seq(-70, -20, length.out = 5)
  lat <- seq(20, 30, length.out = 3)
  month <- 1:12
  
  dud_cfin <- expand.grid(lon = lon, lat = lat, month = month) |>
    mutate(patch = as.factor(0),
           src = "dud") |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # defining brickman identifiers - variables, file
  brickman_source <- brickman::compose_filename("PRESENT")
  brickman_vars <- brickman::query_layers(scenario = "PRESENT")
  
  root <- "/mnt/ecocast/projectdata/calanusclimate/src"
  savefile <- file.path(root, "dud_midatlantic.csv.gz")
  
  # extracting brickman datapoints to match calanus
  data <- brickman::extract_points(brickman_source, 
                                   brickman_vars, 
                                   dud_cfin, 
                                   complete = TRUE,
                                   simplify_names = FALSE) |> 
    bind_cols(dud_cfin) |>
    select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    # mon_vars from setup.R
    mutate_at(mon_vars(), ~.x[[month]]) |>
    ungroup()
  
  # saving to file 
  readr::write_csv(data, file = savefile)
}

# creating cargenie data
if (FALSE) {
  setwd("/mnt/ecocast/projectdata/calanusclimate/src/carnegie")
  
  files <- list.files("short-format")
  
  carnegie <- files |>
    map(~ readr::read_csv(file.path("short-format", .x),
                          col_types = readr::cols()) |>
          mutate(file_source = .x)) |>
    bind_rows() |>
    filter(startsWith(`Taxa-Name`, "Calanus finmarchicus") &
             `Original-VALUE` == "absent") |>
    filter(between(LONGITUDE, -101.50, 24.46) &
             between(LATITUDE, 15.96, 75.25)) |>
    select(MON, LONGITUDE, LATITUDE) |>
    mutate(patch = 0 |> as.factor(), src = "carnegie") |>
    rename(month = MON) |>
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  brickman_source <- brickman::compose_filename("PRESENT")
  brickman_vars <- brickman::query_layers(scenario = "PRESENT")
  bcarm <- brickman::extract_points(brickman_source, 
                                    brickman_vars, 
                                    carnegie, 
                                    complete = TRUE,
                                    simplify_names = FALSE) |> 
    bind_cols(carnegie) |>
    select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    # mon_vars from setup.R
    mutate_at(mon_vars(), ~.x[[month]]) |>
    ungroup()
  
  readr::write_csv(bcarm, file = "carnegie.csv.gz")
}

# creating pelagic data
if (FALSE) {
  setwd("/mnt/ecocast/projectdata/calanusclimate/src/pelagic")
  
  files <- list.files()
  
  prepelagic <- files |>
    map(~ readr::read_csv(.x,
                          skip = 15,
                          col_types = readr::cols()) |>
          mutate(file_source = .x)) |>
    bind_rows() |>
    rename(lon = LONGITDE, lat = LATITUDE, 
           month = MON, abundance = `VALUE-per-area`) |>
    mutate_at(c("lon", "lat", "abundance", "month"), as.numeric) |>
    filter(between(lon, -101.50, 24.46) &
             between(lat, 15.96, 75.25))
  
  pelagic <- prepelagic |>
    filter(!(`Taxa-Modifiers` %in% c("-[ c1 ]-", "-[ c2 ]-", "-[ c3 ]-")) &
             `Taxa-Name` %in% c("Calanus finmarchicus", "Copepoda")) |>
    group_by(month, lat, lon, `Taxa-Name`) |>
    dplyr::summarize(abundance = sum(abundance)) |>
    group_by(month, lat, lon) |>
    dplyr::summarize(patch = (abundance[[1]] > 10000) |> as.numeric()) |>
    as.data.frame() |>
    filter(patch == 0) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  ######
  pelagic <- prepelagic |>
    filter(!(`Taxa-Modifiers` %in% c("-[ c1 ]-", "-[ c2 ]-", "-[ c3 ]-")) &
             `Taxa-Name` %in% c("Calanus finmarchicus", "Copepoda")) |>
    group_by(month, lat, lon, `Taxa-Name`) |>
    dplyr::summarize(abundance = sum(abundance)) |>
    ungroup()
  
  count(pelagic, `Taxa-Name`)
  
  ggplot(pelagic, aes(x = lon, y = lat, col = `Taxa-Name`)) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group), 
                 fill = "gray85", 
                 colour = "gray70",
                 size = .2) +
    coord_quickmap(xlim = c(-105, 25),
                   ylim = c(15, 75),
                   expand = TRUE) +
    theme_bw() +
    geom_point(alpha = .5)
  
  brickman_source <- brickman::compose_filename("PRESENT")
  brickman_vars <- brickman::query_layers(scenario = "PRESENT")
  bcarm <- brickman::extract_points(brickman_source, 
                                    brickman_vars, 
                                    pelagic, 
                                    complete = TRUE,
                                    simplify_names = FALSE) |> 
    bind_cols(pelagic) |>
    select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    # mon_vars from setup.R
    mutate_at(mon_vars(), ~.x[[month]]) |>
    ungroup()
  
  readr::write_csv(bcarm, file = "pelagic.csv.gz")
}

# creating gill data
if (FALSE) {
  setwd("/mnt/ecocast/projectdata/calanusclimate/src/gill")
  
  files <- list.files()
  
  rawgill <- files |>
    map(~ readr::read_csv(.x,
                          skip = 15,
                          col_types = readr::cols()) |>
          mutate(file_source = .x)) |>
    bind_rows() |>
    filter(UNITS...31 == "#/m2" & 
             `Taxa-Name` == "Copepoda") |>
    mutate_at(c("LATITUDE", "LONGITDE", "MON", "VALUE-per-area"), 
              as.numeric) |>
    transmute(lon = LONGITDE, 
              lat = LATITUDE, 
              month = MON, 
              patch = `VALUE-per-area`)
  
  gill <- rawgill |>
    filter(between(lon, -101.50, 24.46) &
             between(lat, 15.96, 75.25)) |>
    filter(patch < 10000) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  ggplot(gill, aes(x = lon, y = lat, col = patch > 10000)) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group), 
                 fill = "gray85", 
                 colour = "gray70",
                 size = .2) +
    coord_quickmap(xlim = c(-85, -70),
                   ylim = c(25, 38),
                   expand = TRUE) +
    theme_bw() +
    geom_point(alpha = .5)
  
  brickman_source <- brickman::compose_filename("PRESENT")
  brickman_vars <- brickman::query_layers(scenario = "PRESENT")
  bcarm <- brickman::extract_points(brickman_source, 
                                    brickman_vars, 
                                    gill, 
                                    complete = TRUE,
                                    simplify_names = FALSE) |> 
    bind_cols(gill) |>
    select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    # mon_vars from setup.R
    mutate_at(mon_vars(), ~.x[[month]]) |>
    ungroup()
  
  readr::write_csv(bcarm, file = "src/gill.csv.gz")
}

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




