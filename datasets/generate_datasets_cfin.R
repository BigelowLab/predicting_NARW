root <- "/mnt/ecocast/projectdata/calanusclimate/src"

# C. FINMARCHICUS
# generates to "tc_datasets/dry_weight_ae.csv.gz"
comparison_cfin <- readr::read_csv(file.path(root, "tc_datasets/dry_weight_ae.csv.gz"))

azmp <- azmpcfin::read_calanus()
# corrected Ecomon
e_corr <- readr::read_csv(file.path(root, "vertical_correction_ecomon.csv.gz"),
                          col_types = readr::cols())

# plotting vertically corrected ecomon
if (FALSE) {
  
  corrected_pts <- e_corr |>
    filter(corrected_CIV_CVI_m2 != CIV_CVI_m2) |>
    mutate(large_correction = findInterval(corrected_CIV_CVI_m2/CIV_CVI_m2, 
                                           c(1.5, 3)) |> 
             factor(labels = c("< 50%", "50% - 300%", "> 300%")))
  
  ggplot(corrected_pts, aes(x = CIV_CVI_m2, y = corrected_CIV_CVI_m2)) + 
    geom_point(color = "blue") + 
    geom_abline()
  
  ggplot(corrected_pts, aes(x = longitude, y = latitude, 
                            col = large_correction)) + 
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    coord_quickmap(xlim = c(-76, -60), ylim = c(35, 47.5), expand = TRUE) +
    geom_point(alpha = .3, size = 1) + 
    theme_bw() + 
    labs(col = "% correction") + 
    ggtitle("Percent correction for C. finmarchicus Ecomon")
}

# Assign each point to a region
e_corr <- mutate(e_corr, region_tc = "gom")
get_region_vd <- function(transect, station) {
  if (is.na(transect)) {
    "gom"
  } else if (transect == "BBL") {
    "wss"
  } else if (transect %in% c("HL", "LL", "SESPB", "SWSPB") || 
             station == "HL2") {
    "ess"
  } else if (station == "P5") {
    "gom"
  } else {
    "gsl"
  }
}
azmp <- azmp |>
  mutate(region_tc = purrr::map2(transect, station, get_region_vd) |>
           unlist())

# Optional plot of AZMP regions
if (FALSE) {
  regions <- read_sf(dsn = "datasets/threshold_conversions/shapefiles/Regions_dw_vd_poly_all.shp") |>
    st_make_valid() |>
    st_transform(crs = 4326)
  
  ggplot() +
    geom_sf(data = regions, alpha = 0, col = "black") +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    geom_point(data = azmp, aes(x = longitude, y = latitude, col = region_tc, alpha = .2), alpha = .7) +
    coord_sf(xlim = c(-70, -40), ylim = c(40, 60), expand = TRUE) +
    theme_bw() + 
    ggtitle("AZMP points by threshold conversion region")
}

# Bind datasets together 
desired_vars <- c("src", "id", "date", "region_tc",
                  "longitude", "latitude", "abundance")

e_corr <- e_corr |>
  mutate(src = "ecomon",
         id = sprintf("%s_%i", .data$cruise_name, .data$station), 
         abundance = corrected_CIV_CVI_m2) |>
  select(all_of(desired_vars)) |>
  filter(!is.na(abundance))

azmp <- azmp |>
  mutate(src = "azmp", 
         id = sprintf("%s_%s", .data$transect, .data$station),
         date = as.Date(sprintf("%0.4i-%0.2i-%0.2i", .data$year, .data$month,.data$day)),
         abundance = (.data$calanus_finmarchicus_iv + 
                        .data$calanus_finmarchicus_v + 
                        .data$calanus_finmarchicus_vi)) |>
  select(all_of(desired_vars))
# no null values

ae <- bind_rows(azmp, e_corr) |>
  mutate(month = as.numeric(format(date, "%m"))) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Merge with Brickman
brickman_source <- brickman::compose_filename("PRESENT")
brickman_vars <- brickman::query_layers(scenario = "PRESENT")
brickman_vars <- brickman_vars[!grepl("_ann", brickman_vars)]

bdat <- brickman::extract_points(brickman_source, 
                                 brickman_vars, 
                                 ae, 
                                 complete = TRUE,
                                 simplify_names = FALSE)

ae_brickman <- bdat |>
  bind_cols(ae) |>
  select(-nav_lat, -nav_lon, -olon, -olat, -index, -row, -col, -geometry) |>
  rowwise() |>
  mutate_at(c("Xbtm", "MLD", "Sbtm", "SSS", "SST", "Tbtm", "U", "V"), 
            ~.x[[month]]) |>
  ungroup()

# Saving to files
readr::write_csv(ae_brickman, file = file.path(root, "11_25_finaldraft/cfinmarchicus.csv.gz"))









