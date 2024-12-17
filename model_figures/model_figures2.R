
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

# Threshold Corrections and Combined Effects
if (FALSE) {
Bathy_test <- 0:1000
b <- as.data.frame(x = Bathy_test)
plot_data <- 
  rbind(mutate(b, type = 'Relative Prey Density',
               val = 0.147235074555841 + 
                 0.980070557219308 * exp(-0.0101656880852439 * Bathy_test)),
        mutate(b, type = 'Relative Foraging Time',
               val = get_foraging_table("rest")[round(pmin(1000, pmax(10, Bathy_test))/10, 0), 2]),
        mutate(b, type = 'Correction Factor',
               val = stephane_final()(Bathy_test))) |>
  mutate(type = factor(type, levels = c('Relative Foraging Time',
                                        'Relative Prey Density',
                                        'Correction Factor')))
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
  labs(y = "Correction Factor", x = "Bathymetry")  

# facet wrap
ggplot(plot_data, aes(x = Bathy_test, y = val, col = type)) +
  scale_color_manual(values = c("grey50", "grey50", "red")) +
  geom_line(linewidth = 1) +
  facet_wrap(~type) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Value", x = "Bathymetry (m)")
}

# Scatter plots of Measured Dry Weight vs. Modeled patch probability
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

# Variable Importance
if (FALSE) {
  chyp_imp <- var_imp("v6.03", plot = FALSE)
  cfin_imp <- var_imp("v6.01", plot = FALSE)
  
  variable_name_map <- c(
    "Vel"="Velocity",
    "month"="Month",
    "SST"="SST",
    "SSS"="SSS",
    "MLD"="MLD",
    "Tbtm"="SBT",
    "Sbtm"="SBS",
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
    labs(x = "Variable Importance") +
    geom_bar(stat = 'identity', fill = "dodgerblue4")
  
}

# POSTER 
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





