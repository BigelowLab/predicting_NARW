
# Figure S3
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
  
  big_dataset <- combined_threshold[5:10] |>
    reduce(bind_rows) |>
    select(lat, lon, month, REL_PRESENCE)
  
  regions <- read_sf(dsn = "model_figures/shapefiles/Regions_dw_vd_poly_all.shp") |>
    st_make_valid() |>
    st_transform(crs = 4326)
  
  regions_vec <- c("ESS", "WSS", "GoM", "swGSL", "nGSL", "nGSL", "NLS", "NLS", "Other")
  
  regions_all <- 
    mutate(big_dataset, region = big_dataset |>
             st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
             st_intersects(regions, sparse = FALSE) |>
             apply(1, function(u) regions_vec[ifelse(any(u), which(u), 9)]))
  
  feedstatus <- list(FALSE_FALSE = "No Habitat",
                     FALSE_TRUE = "New Habitat", 
                     TRUE_FALSE = "Lost Habitat", 
                     TRUE_TRUE = "Retained Habitat")
  
  # palette for colors 
  pal <- c(FALSE_FALSE = "white",
           FALSE_TRUE = "#0295d9", 
           TRUE_FALSE = "#CD0000", 
           TRUE_TRUE = "#FFD82E")
  
  p <- ggplot(regions_all |> filter(region != "Other")) +
    theme_bw() +
    geom_bar(aes(x = region, group = REL_PRESENCE, fill = REL_PRESENCE), 
             position = "fill") + 
    geom_bar(aes(x = region), col = "black", fill = "transparent", 
             position = "fill") +
    facet_wrap(~mon_names()[month], ncol = 2, nrow = 3) +
    guides(fill = guide_legend(override.aes = list(color = "black"))) +
    theme(legend.position = "bottom", panel.spacing = unit(1, "lines")) +
    labs(x = "Region", y = "Percentage of Predictions", fill = "Habitat Status") +
    scale_fill_manual(labels = feedstatus, values = pal)
  
  save_analysis(p, "v6.01", "v6.01_v6.03_threshold_bar_charts")
}

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

