
# Figure S4
if (FALSE) {
  combined_present <- get_combined_data(
    read_preds("v6.01", NULL, "PRESENT", 1:12, quantile = TRUE),
    read_preds("v6.03", NULL, "PRESENT", 1:12, quantile = TRUE),
    quantile = "50%"
  )
  
  combined_future <- get_combined_data(
    read_preds("v6.01", 2075, "RCP85", 1:12, quantile = TRUE),
    read_preds("v6.03", 2075, "RCP85", 1:12, quantile = TRUE),
    quantile = "50%"
  )
  
  combined_threshold <- get_threshold_data(combined_future, combined_present, 
                                           threshold = .2, quantile = .5)
  
  big_dataset <- combined_threshold[5:10] |> # summer months
    reduce(bind_rows) |>
    select(lat, lon, month, REL_PRESENCE)
  
  regions <- read_sf(dsn = "model_figures/shapefiles/Regions_dw_vd_poly_all.shp") |>
    st_make_valid() |>
    st_transform(crs = 4326)
  
  regions_NEW <- read_sf(dsn = "model_figures/shapefiles/johnson2024_regions.shp") |>
    st_make_valid()
  
  regions_vec <- c("ESS", "WSS", "GoM", "swGSL", "nGSL", "nGSL", "NLS", "NLS", "Other")
  
  regions_all <- 
    mutate(big_dataset, region = big_dataset |>
             st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
             st_intersects(regions, sparse = FALSE) |>
             apply(1, function(u) regions_vec[ifelse(any(u), which(u), 9)]))
  
  regions_all <- regions_all |>
    mutate(month = factor(month.abb[month], level=month.abb))
  
  feedstatus <- list(FALSE_FALSE = "No Habitat",
                     TRUE_FALSE = "Lost Habitat", 
                     TRUE_TRUE = "Retained Habitat",
                     FALSE_TRUE = "New Habitat")
  
  # palette for colors 
  pal <- c(FALSE_FALSE = "white", 
           TRUE_FALSE = "#CD0000", 
           TRUE_TRUE = "#FFD82E",
           FALSE_TRUE = "#0295d9")
  
  p <- ggplot(regions_all |> filter(region != "Other")) +
    theme_bw() +
    geom_bar(aes(x = region, group = REL_PRESENCE, fill = REL_PRESENCE), 
             position = "fill") + 
    geom_bar(aes(x = region), col = "black", fill = "transparent", 
             position = "fill") +
    facet_wrap(~month, ncol = 2, nrow = 3) +
    guides(fill = guide_legend(override.aes = list(color = "black"))) +
    theme(legend.position = "bottom", panel.spacing = unit(1, "lines")) +
    labs(x = "Region", y = "Percentage of Predictions", fill = NULL) +
    scale_fill_manual(labels = feedstatus, values = pal)
  
  save_analysis(p, "v6.01", "v6.01_v6.03_threshold_bar_charts")
}

# Figure 4: Model Performance and Data Composition
if (FALSE) {
# Bar chart
get_species_barchart <- function(v) {
  src_data <- data_from_config(read_config(v)$training_data, 
                               vars_override = "src",
                               threshold_method = flat_tm(30000*195)) |>
    select(month, src, lat, lon)
  
  regions <- read_sf(dsn = "model_figures/shapefiles/Regions_dw_vd_poly_all.shp") |>
    st_make_valid() |>
    st_transform(crs = 4326)
  
  regions_vec <- c("ESS", "WSS", "GoM", "swGSL", "nGSL", "nGSL", "NLS", "NLS", "Other")
  
  regions_data <- 
    mutate(src_data, region = src_data |>
             st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
             st_intersects(regions, sparse = FALSE) |>
             apply(1, function(u) regions_vec[ifelse(any(u), which(u), 9)]))
  
  ggplot(regions_data |> filter(region != "Other")) +
    theme_bw() +
    geom_bar(aes(x = month, group = src, fill = src)) + 
    geom_bar(aes(x = month), col = "black", fill = "transparent") +
    guides(fill = guide_legend(override.aes = list(color = "black"))) +
    theme(legend.position = "bottom", panel.spacing = unit(1, "lines")) +
    labs(x = "Month", y = "No. Records", fill = "Data Source") +
    scale_fill_manual(labels = c("AZMP", "EcoMon"), values = c("orange", "blue"))
}

cfin_barchart <- get_species_barchart("v6.01") + 
  theme(legend.position = "none")
chyp_barchart <- get_species_barchart("v6.03") + 
  theme(legend.position = "none")

cfin_auc <- roc_curves_w_ci("v6.01", save = FALSE)
chyp_auc <- roc_curves_w_ci("v6.03", save = FALSE)

# go to run_version.R
model_preds_cfin <- get_v_wkfs(v) |>
  apply_quantile_preds(select(data, -patch), c(.5, 1))
cfin_dryweight_g <- pred_v_dryweight_g(v, tm, model_preds_cfin, save = FALSE)
chyp_dryweight_g <- pred_v_dryweight_g(v, tm, model_preds_chyp, save = FALSE)

ggarrange(ggplot() + theme_void(), ggplot() + theme_void(), cfin_barchart, 
          chyp_barchart + 
            theme(axis.title.y = element_blank()), 
          cfin_auc +
            theme(plot.title = element_blank()), 
          chyp_auc +
            theme(plot.title = element_blank(),
                  axis.title.y = element_blank()), 
          cfin_dryweight_g +
            theme(plot.title = element_blank()), 
          chyp_dryweight_g +
            theme(plot.title = element_blank(),
                  axis.title.y = element_blank()),
          labels = c("", "", "a)", "b)", "c)", "d)", "e)", "f)"),
          hjust = -.5, vjust = .8,
          nrow = 4, ncol = 2, heights = c(1, 2, 2, 3), align = "hv")
}

# Make regions shapefiles for Figure 1, S4
if (FALSE) {
# fucking around with shapefiles
target_regions <- c("MAB", "GoM", "WSS", "ESS", "swGSL", "nGSL", "NLS")

regions_og <- read_sf(dsn = "model_figures/shapefiles/Regions_dw_vd_poly_all.shp") |>
  st_make_valid() |>
  st_transform(crs = 4326)

regions_MEOW <- read_sf(dsn = "model_figures/shapefiles/meow_ecos.shp") |>
  st_make_valid() |>
  st_transform(crs = 4326)
desired_MEOW <- regions_MEOW[regions_MEOW$ECO_CODE_X %in% 39:42, "ECO_CODE_X"]

# Getting shelf line
bathy <- marmap::getNOAA.bathy(-80, -40, 34,  59, resolution = 4) |>
  marmap::fortify.bathy() 
bathy_sf <- bathy |>
  st_as_sf(coords = c("x", "y"), crs = 4326)
shelf_sf <- bathy_sf[bathy_sf$z >= -500,] |> select(-z) |>
  st_union() |>
  st_concave_hull(ratio = .005, allow_holes = FALSE)

plot(shelf_sf)

shelf_MEOW <- st_intersection(desired_MEOW, shelf_sf) |>
  select(-ECO_CODE_X)
shelf_MEOW$id <- c("MAB", "GoM", "WSS", "MAB")

shelf_og <- st_intersection(regions_og, shelf_sf) |>
  nngeo::st_remove_holes() |>
  select(id, geometry)

# defining regions
NLS <- shelf_og[shelf_og$id %in% c("NL", "sNL"),] |>
  st_union() |>
  st_simplify(dTolerance = 3000)

nGSL <- shelf_og[shelf_og$id %in% c("neGSL", "nwGSL"),] |>
  st_union()

swGSL <- filter(shelf_og, id == "sGSL")

ESS <- filter(shelf_og, id == "ESS")

MAB <- filter(shelf_MEOW, id == "MAB") |>
  st_union() 

GoM <- filter(shelf_MEOW, id == "GoM")

WSS <- filter(shelf_MEOW, id == "WSS") |>
  st_geometry() |>
  st_difference(ESS |> st_convex_hull()) |>
  st_sf()
WSS <- WSS[2,]

# combining
reg <- list(MAB, GoM, WSS, ESS, swGSL, nGSL, NLS)
casted_reg <- map(reg, st_geometry)
reg_sf <- tibble(id = target_regions, 
                 geometry = flatten(casted_reg)) |>
  st_sf(crs = 4326)

st_write(reg_sf, "model_figures/shapefiles/johnson2024_regions.shp")

# Plotting
ggplot() +
  geom_sf(data = reg_sf, aes(fill = id), alpha = .5) + 
  geom_polygon(data = ggplot2::map_data("world"),
               aes(long, lat, group = group),
               fill = "lightgray", col = "gray") + 
  coord_sf(xlim = c(-76, -40), ylim = c(35, 60), crs = 4326) +
  theme_bw()
}
