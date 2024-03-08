root <- "/mnt/ecocast/projectdata/calanusclimate/src"

# C. HYPERBOREUS
# generates to "tc_datasets/ae_dryweight_nooutliers_chyp.csv.gz"

azmp <- azmpcfin::read_calanus()
# uncorrected Ecomon
ecomon <- ecomon::scale_ecomon()

# plotting vertically corrected ecomon
if (FALSE) {
  # Vertically corrected C. Hyperboreus Ecomon
  ecomon2 <- readr::read_csv(file.path(root, "vertical_correction_ecomon_hyp.csv.gz"))
  
  corrected_pts <- readr::read_csv(file.path(root, "vertical_correction_ecomon_hyp.csv.gz")) |>
    filter(corrected_CIV_CVI_m2 != CIV_CVI_m2) |>
    mutate(large_correction = findInterval(corrected_CIV_CVI_m2/CIV_CVI_m2, 
           c(1.5, 3)) |> 
             factor(labels = c("< 50%", "50% - 300%", "> 300%")))
  
  super_high_correction <- corrected_pts |>
    filter(corrected_CIV_CVI_m2 == max(corrected_pts$corrected_CIV_CVI_m2))
  
  ggplot(corrected_pts, aes(x = CIV_CVI_m2, y = corrected_CIV_CVI_m2)) + 
    geom_point(color = "blue") + 
    geom_abline()
  
  ggplot(corrected_pts, aes(x = lon, y = lat, 
                            col = large_correction)) + 
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    coord_quickmap(xlim = c(-76, -60), ylim = c(35, 47.5), expand = TRUE) +
    geom_point(alpha = .3, size = 1) + 
    geom_point(data = super_high_correction, mapping = aes(x = lon, y = lat), 
               color = "black", size = 1) + 
    theme_bw() + 
    labs(col = "% correction") + 
    ggtitle("Percent correction for C. Hyperboreus Ecomon")
}

# Assign each point to a region
ecomon <- mutate(ecomon, region_tc = "gom")
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
desired_vars <- c("src", "id", "date", "region_tc", "transect", "station", 
                  "longitude", "latitude", "abundance")

# Plot calspp_m2
if (FALSE) {
  ecomon_filtered <- ecomon |> 
    filter_dates("1990-01-01", "2015-12-31") |>
    select(lon, lat, calspp_m2)
  
  ggplot(filter(ecomon_filtered, calspp_m2 > 0), aes(x = lon, y = lat, 
                            col = log10(calspp_m2 + 1))) + 
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    coord_quickmap(xlim = c(-76, -60), ylim = c(35, 47.5), expand = TRUE) +
    geom_point(alpha = .3, size = 1) + 
    scale_color_viridis(option = "inferno") +
    theme_bw() + 
    labs(col = "log10(ind + 1)") + 
    ggtitle("Calanus spp. records above 0")
  
  ggplot(filter(ecomon_filtered, calspp_m2 > 0)) + 
    geom_histogram(aes(x = log10(calspp_m2 + 1)), 
                   bins = 50, col = "white", fill = "darkgreen") +
    theme_bw() + 
    ggtitle("Histogram of abundance counts in Ecomon calspp")
  
  ggplot(filter(ecomon_filtered,
                calspp_m2 * 1145 > 30000*195), aes(x = lon, y = lat)) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    coord_quickmap(xlim = c(-76, -60), ylim = c(35, 47.5), expand = TRUE) +
    geom_point(size = 1, col = "red") + 
    theme_bw() + 
    ggtitle("Calanus spp. patches")
}

ecomon <- ecomon |>
  mutate(src = "ecomon", 
         transect = NA,
         station = NA,
         id = sprintf("%s_%i", .data$cruise_name, .data$station), 
         abundance = calspp_m2) |> # 0
  rename(longitude = lon, latitude = lat) |>
  select(all_of(desired_vars)) |>
  filter(!is.na(abundance))

azmp <- azmp |>
  mutate(src = "azmp", 
         id = sprintf("%s_%s", .data$transect, .data$station),
         date = as.Date(sprintf("%0.4i-%0.2i-%0.2i", .data$year, .data$month,.data$day)),
         abundance = (.data$calanus_hyperboreus_iv + 
                        .data$calanus_hyperboreus_v + 
                        .data$calanus_hyperboreus_vi)) |>
  select(all_of(desired_vars))
# no null values

ae <- bind_rows(azmp, ecomon) |>
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

# Remove AZMP outliers
ae_brickman |>
  filter(Tbtm > 5, abundance > 0, src == "azmp", 
         (transect %in% c("BBL", "HL") | station %in% c("HL2", "RIM")))

ae_nooutliers <- ae_brickman |>
  filter(!(Tbtm > 5 & abundance > 0 & src == "azmp" &
             (transect %in% c("BBL", "HL") | station %in% c("HL2", "RIM"))))

ae_dw_nooutliers <- ae_nooutliers |>
  mutate(dry_weight = abundance * 
           map(region_tc, ~ifelse(.x == "gsl", 1390, 1145)) |> unlist())

# Saving to files
readr::write_csv(ae_dw_nooutliers, file = file.path(root, "1_5_final_chyp/chyp_src_zeros.csv.gz"))

# investigating positives in the MAB
if(FALSE) {
  inv <- ae_dw_nooutliers |>
    mutate(patch = dry_weight > 30000*195)
  
  phelp <- function(data) {
    ggplot(data, aes(x = lon, y = lat)) +
      geom_polygon(data = ggplot2::map_data("world"), 
                   aes(long, lat, group = group),
                   fill = "lightgray", col = "gray") +
      coord_quickmap(xlim = c(-76, -63), ylim = c(35, 45), expand = TRUE)
  }
  
  phelp(inv |> filter(patch == TRUE)) +
    geom_point(size = .7, alpha = .5)
  
  phelp(inv, aes(x = lon, y = lat)) +
    geom_point(aes(col = abundance), size = .7, alpha = .5)
  
  positive_patches <- inv |> filter(patch == TRUE, src == "ecomon") |>
    mutate(rank_p = rank(abundance))
  
  phelp(positive_patches) +
    geom_point(aes(col = log10(abundance + 1)), size = 1) +
    scale_color_viridis(option = "turbo") + 
    labs(col = "log(ind)") + 
    ggtitle("C.Hyp ecomon records > abundance threshold (n = 948)")
  
  ecomon_abu <- ecomon::scale_ecomon() |>
    select(lat, lon, hyper_m2) |>
    filter(!is.na(hyper_m2))
  
  nrow(ecomon_abu) * (1-.96683)
  
  ecdf(ecomon_abu$hyper_m2)(30000*195/1145)
  
  ## SPATIAL ANALYSIS
  library(ggarrange)
  
  # Hex distribution of occurences 
  ggarrange(
  ggplot(positive_patches, aes(x = lon, y = lat)) + 
    geom_hex(bins = 800) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    coord_quickmap(xlim = c(-76, -63), ylim = c(35, 45), expand = TRUE) + 
    theme_bw() + 
    scale_fill_viridis(option = "turbo") + 
    ggtitle("high abundance"),
  ggplot(inv |> filter(src == "ecomon"), aes(x = lon, y = lat)) + 
    geom_hex(bins = 800) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    coord_quickmap(xlim = c(-76, -63), ylim = c(35, 45), expand = TRUE) + 
    theme_bw() + 
    scale_fill_viridis(option = "turbo") + 
    ggtitle("all points"),
  ncol = 2)
  
  library(spdep)
  library(sf)
  library(rgdal)
  library(spatialEco)
  
  invc <- inv |> select(lat, lon, abundance) |>
    group_by(lat, lon) |>
    summarize(abundance = mean(abundance), .groups = "keep")
  
  coordinates(invc) <- c("lon", "lat")
  
  high_ab <- invc[invc$abundance > 5109,]
  
  moran.test(invc$abundance, listw = nb2listw(knn2nb(knearneigh(invc)), style = "W"))
}










