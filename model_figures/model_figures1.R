
# Figure 1. Map of Calanus spp. sampling stations
if (FALSE) {
  library(marmap)
  samplingstations <- get_species_raw(species = "C. finmarchicus", filter = TRUE)
  
  regions <- read_sf(dsn = "model_figures/shapefiles/johnson2024_regions.shp") |>
    st_make_valid()
  
  ordered_reg <- c("MAB", "GoM", "WSS", "ESS", "swGSL", "nGSL", "NLS")
  
  ggplot(data = samplingstations) +
    geom_sf(data = regions, aes(fill = factor(id, levels = ordered_reg)), 
            col = "NA", alpha=.3, show.legend = FALSE) +
    scale_fill_manual(values = c("#00B6EB", "#FB61D7", "#00C094", "#A58AFF",
                                 "#53B400", "#F8766D", "#C49A00")) +
    geom_point(aes(x = lon, y = lat, col = src), cex = .3, alpha = .4) +
    geom_sf(data = regions, alpha = .5, fill = "NA",
            col = "black", linewidth = .3) +
    scale_color_manual(values = c("blue", "orange"), 
                       labels = c("AZMP", "EcoMon")) + 
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 col = "black", fill = "grey40", linewidth = .3) +
    coord_sf(xlim = c(-77.0, -42.5),
             ylim = c(35.2,  57.6),
             crs = 4326,
             expand = FALSE) + 
    theme_bw() +
    labs(fill = "Region", col = "Data Source") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom") + 
    guides(color = guide_legend(override.aes = list(size = 2, alpha = 1)))
}

# Figure 2. Threshold Corrections and Combined Effects
if (FALSE) {
  Bathy_test <- 0:1000
  b <- as.data.frame(x = Bathy_test)
  plot_data <- 
    rbind(mutate(b, type = 'Relative prey density',
                 val = 0.147235074555841 + 
                   0.980070557219308 * exp(-0.0101656880852439 * Bathy_test)),
          mutate(b, type = 'Relative foraging time',
                 val = get_foraging_table("rest")[round(pmin(1000, pmax(10, Bathy_test))/10, 0), 2]),
          mutate(b, type = 'Correction factor',
                 val = stephane_final()(Bathy_test))) |>
    mutate(type = factor(type, levels = c('Relative foraging time',
                                          'Relative prey density',
                                          'Correction factor')))
  # in one figure
  as.data.frame(x = Bathy_test) |>
    mutate(`Correction Factor` = stephane_final()(Bathy_test),
           `Relative Prey Density` = 0.147235074555841 + 
             0.980070557219308 * exp(-0.0101656880852439 * Bathy_test),
           `Relative Foraging Time` = 
             get_foraging_table("rest")[round(pmin(1000, pmax(10, Bathy_test))/10, 0), 2]) |> 
    ggplot(aes(x = Bathy_test)) +
    geom_line(aes(y = `Relative Prey Density`), color = 'grey50') +
    geom_line(aes(y = `Relative Foraging Time`), color = 'grey50') +
    geom_line(aes(y = `Correction Factor`), color = 'red', linewidth = 1) + 
    theme_bw() +
    labs(y = "Correction factor", x = "Bathymetry")  
  
  # facet wrap
  ggplot(plot_data, aes(x = Bathy_test, y = val, col = type)) +
    scale_color_manual(values = c("grey50", "grey50", "red")) +
    geom_line(linewidth = 1) +
    facet_wrap(~type) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(y = "Value", x = "Bathymetry (m)")
}

# Figure 3. Model Performance and Data Composition
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
      labs(x = "Month", y = "No. records", fill = "Data source") +
      scale_fill_manual(labels = c("AZMP", "EcoMon"), values = c("orange", "blue"))
  }
  
  cfin_barchart <- get_species_barchart("v6.01") + 
    theme(legend.position = "none")
  chyp_barchart <- get_species_barchart("v6.03") + 
    theme(legend.position = "none")
  
  cfin_auc <- roc_curves_w_ci("v6.01", save = FALSE)
  chyp_auc <- roc_curves_w_ci("v6.03", save = FALSE)
  
  # go to run_version.R
  model_preds_ <- get_v_wkfs(v) |>
    apply_quantile_preds(select(data, -patch), c(.5, 1))
  cfin_dryweight_g <- pred_v_dryweight_g(v, tm, model_preds_, save = FALSE)
  chyp_dryweight_g <- pred_v_dryweight_g(v, tm, model_preds_, save = FALSE)
  
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

# Figure 4. Response curves generated by response_curves_data() at run_version.R

# Figure 5. Combined Present Day data
get_quant_raw_plots(v = "v6.01", 
                    0,
                    plot_scenarios = c(5), # Present day
                    save_months = 1:12, 
                    cropped = TRUE,
                    gridded = TRUE,
                    quant_col = .5,
                    top_limit = .5,
                    combining_v = "v6.03")

# Figure 6. Combined threshold future data
get_quant_threshold_plots("v6.01", 
                          0,
                          plot_scenarios = 4, # RCP 8.5 2075
                          threshold = .2,
                          save_months = c(5:10), 
                          cropped = TRUE, 
                          gridded = TRUE, 
                          quant_col = .5,
                          combining_v = "v6.03")

# Figure S1. Vertical correction defined in correct_ecomon_data.R

# Figure S2. Variable Importance
if (FALSE) {
  chyp_imp <- var_imp("v6.03", plot = FALSE)
  cfin_imp <- var_imp("v6.01", plot = FALSE)
  
  #' This code is not adaptable, it hardcodes variable importance. 
  #' Ideal solution would utilise var_abb() from setup.R
  variable_name_map <- c(
    "Vel"="Velocity",
    "month"="Month",
    "SST"="Sea surface temperature",
    "SSS"="Sea surface salinity",
    "MLD"="Mixed layer depth",
    "Tbtm"="Sea bottom temperature",
    "Sbtm"="Sea bottom salinity",
    "Bathy_depth"="Bathymetry"
  )
  
  both_species <- bind_rows(
    chyp_imp |> mutate(type = "C. hyperboreus"),
    cfin_imp |> mutate(type = "C. finmarchicus")
  ) |>
    mutate(Variable = variable_name_map[Variable] |> 
             factor(levels = variable_name_map)) |>
    arrange(desc(Importance))
  
  ggplot(both_species, aes(x = Importance, y = Variable)) +
    facet_wrap(~type) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          strip.text = element_text(face = "italic")) +
    labs(x = "Variable importance") +
    geom_bar(stat = 'identity', fill = "dodgerblue4")
  
}

# Figure S3. Cfin present predictions
get_quant_raw_plots(v = "v6.01", 0,
                    plot_scenarios = c(5), 
                    save_months = c(1:12), 
                    cropped = TRUE,
                    gridded = TRUE,
                    quant_col = .5,
                    top_limit = .5)

# Figure S4. Chyp present predictions
get_quant_raw_plots(v = "v6.03", 0,
                    plot_scenarios = c(5), 
                    save_months = c(1:12), 
                    cropped = TRUE,
                    gridded = TRUE,
                    quant_col = .5,
                    top_limit = .5)

# Figure S5. Uncertainty
get_quant_pIQR_plots("v6.01", 
                     0,
                     plot_scenarios = 5,
                     save_months = 1:12,
                     cropped = TRUE, 
                     gridded = TRUE,
                     top_limit = .3,
                     combining_v = "v6.03")

# Figure S6. Scenario comparison assembled piecemeal. 

# Figure S7. Winter habitat shift maps
get_quant_threshold_plots("v6.01", 
                          0,
                          plot_scenarios = 4, # RCP 8.5 2075
                          threshold = .2,
                          save_months = c(11, 12, 1, 2, 3, 4), 
                          cropped = TRUE, 
                          gridded = TRUE, 
                          quant_col = .5,
                          combining_v = "v6.03")

# Figure S8 Barcharts of region abundance
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
  
  regions <- read_sf(dsn = "model_figures/shapefiles/johnson2024_regions.shp") |>
    st_make_valid()
  
  regions_names <- 
    c("MAB", "GoM", "WSS", "ESS", "swGSL", "nGSL", "NLS", "Other")
  
  regions_all <- 
    mutate(big_dataset, region = big_dataset |>
             st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
             st_intersects(regions, sparse = FALSE) |>
             apply(1, function(u) regions_names[ifelse(any(u), which(u), 8)]))
  
  regions_all <- regions_all |>
    mutate(month = factor(month.abb[month], level=month.abb),
           REL_PRESENCE = factor(REL_PRESENCE,
                                 levels = c("FALSE_FALSE", "TRUE_FALSE", 
                                            "TRUE_TRUE", "FALSE_TRUE")),
           region = factor(region, levels = regions_names))
  
  feedstatus <- list(FALSE_FALSE = "No habitat",
                     TRUE_FALSE = "Lost habitat", 
                     TRUE_TRUE = "Retained habitat",
                     FALSE_TRUE = "New habitat")
  
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
    labs(x = "Region", y = "Percentage of predictions within region", 
         fill = NULL) +
    scale_fill_manual(labels = feedstatus, values = pal)
  p
  
  save_analysis(p, "v6.01", "v6.01_v6.03_threshold_bar_charts")
}

# Figure S9. Phi threshold comparison
if (FALSE) {
  combined_present <- get_combined_data(
    read_preds("v6.01", NULL, "PRESENT", 8, quantile = TRUE),
    read_preds("v6.03", NULL, "PRESENT", 8, quantile = TRUE),
    quantile = "50%"
  )
  
  combined_future <- get_combined_data(
    read_preds("v6.01", 2075, "RCP85", 8, quantile = TRUE),
    read_preds("v6.03", 2075, "RCP85", 8, quantile = TRUE),
    quantile = "50%"
  )
  
  thresholds <- list("\u03C6 = 10%"=.1, "\u03C6 = 15%"=.15, 
                     "\u03C6 = 20%"=.2, "\u03C6 = 25%"=.25)
  
  thresholds_datas <- thresholds |>
    map(~get_threshold_data(combined_future, combined_present, 
                            threshold = .x, quantile = .5)[[1]])
  
  plot <- plot_data_list(thresholds_datas, 0, plot_threshold, "REL_PRESENCE", 
                         TRUE, gridded = TRUE)
  save_analysis(plot, "v6.01", "FigureS9_phithresholdcomparison")
}

# OTHER FIGURES NOT INCLUDED IN MANUSCRIPT

# Formatted plots for Poster presented at North Atlantic Right Whale consortium, 2024
if (FALSE) {
  cfin_data <- data
  chyp_data <- data
  
  cfin_data <- cfin_data |>
    select(lat, lon, patch) |>
    rename(cfin_patch = patch)
  
  chyp_data <- chyp_data |>
    select(lat, lon, patch) |>
    rename(chyp_patch = patch)
  
  cfin_data |>
    mutate(patch = patch == 1) |>
    ggplot(aes(x = lon, y = lat, col = patch)) +
    geom_point(alpha = .5, size = .6) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "grey", col = "grey60") +
    scale_color_manual(values = c("darkslategray3", "red1")) +
    coord_quickmap(xlim = c(-76, -40), ylim = c(35, 60), expand = TRUE) +
    labs(x = "Longitude", y = "Latitude", col = "C. finmarchicus aggregation") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.position = "top") + 
    guides(col = guide_legend(override.aes = list(size = 3, alpha = 1) ) )
  
}

# Make regions shapefiles
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

# [No longer included] Map of tau-patch presences and absences for cfin/chyp
if (FALSE) {
  # run lines 15-25 of run_version.R
  chyp_data <- data |>
    mutate(month = factor(month.abb[month], level=month.abb))
  
  cfin_data <- data |>
    mutate(month = factor(month.abb[month], level=month.abb))
  
  
  ggplot(chyp_data, aes(x = lon, y = lat)) +
    geom_polygon(data = ggplot2::map_data("world"),
                 aes(long, lat, group = group),
                 fill = "gray", col = "gray",
                 linewidth = .2) +
    geom_point(aes(col = patch), alpha = .4, size = .3) +
    scale_color_manual(values = c("orange", "darkblue")) +
    coord_quickmap(xlim = c(-78, -40), ylim = c(34, 60), expand = FALSE) +
    theme_bw() + 
    theme(axis.title = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank()) +
    ggtitle(expression(paste("b) ", italic("C. hyperboreus")))) +
    facet_wrap(~month, nrow = 2, ncol = 6) +
    theme(legend.position = "none")
}

# [Experimental Figure] Scatter plots of Measured Dry Weight vs. Modeled patch probability
if (FALSE) {
  
  # PREP
  v <- "v6.01" #cfin, "v6.02" chyp
  config <- read_config(v)
  set.seed(config$model$seed)
  tm <- flat_tm(30000*195)
  
  data <- data_from_config(config$training_data, 
                           threshold_method = tm)
  
  model_preds <- get_v_wkfs(v) |>
    apply_quantile_preds(select(data, -patch), c(.5, 1))
  
  # ANALYSIS
  # retrieves dry weight data and binds to 50% predictions
  ref <- data_from_config(read_config(v)$training_data, 
                          vars_override = "dry_weight",
                          threshold_method = tm) |>
    select(patch, dry_weight) |>
    bind_cols(.pred_1 = model_preds$`50%`)
  
  # performing correlation test
  cortest <- cor.test(ref$dry_weight, ref$.pred_1, 
                      method = "spearman", exact = FALSE)
  
  cortest
  
  # Calculate Accuracy
  caret::confusionMatrix(data = factor(ref$.pred_1 >= .5), 
                         reference = factor(ref$dry_weight >= 30000*195))
  
  #' CFIN
  #'            Reference
  # Prediction  FALSE  TRUE
  # FALSE       16194  1468
  # TRUE        395  1165
  # 
  # Accuracy : 0.9031          
  # 95% CI : (0.8988, 0.9072)
  # True Positive Rate: .4425
  
  #' CHYP
  #'            Reference
  # Prediction  FALSE  TRUE
  # FALSE       19495   342
  # TRUE          244  1263
  # 
  # Accuracy : 0.9725          
  # 95% CI : (0.9703, 0.9747)
  # True Positive Rate: .7869
  
  # creating plottable objects
  p <- ggplot(ref, aes(x = log10(dry_weight + 1), y = .pred_1)) +
    theme_bw() +
    labs(y = "τ-patch probability", x = "Dry weight (ug)(log10(x + 1))") +
    ggtitle("τ-patch probability vs. dry weight") +
    coord_cartesian(expand = TRUE) +
    geom_point(alpha = .1) + # adjust based on screen size
    geom_hline(yintercept = .5, color = "white", linewidth = 1.1) + 
    geom_vline(xintercept = log10(30000*195 + 1), color = "white", 
               linewidth=1.1) +
    geom_hline(yintercept = .5, color = "red", linewidth = .75) + 
    geom_vline(xintercept = log10(30000*195 + 1), color = "red", linewidth=.75) +
    annotate("text", x = 1, y = c(.9, .85, .8), 
             label = c(paste("rho =", round(cortest$estimate, 4)), 
                       "p < 2.2e-16", 
                       paste("n =", nrow(ref))))
  
  p |> save_analysis(v, "pred_dryweight_paper_figure")
}
