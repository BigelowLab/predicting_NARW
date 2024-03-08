source("/mnt/ecocast/projects/students/ojohnson/brickman/setup.R")
library(groupdata2)
library(viridis)
library(RColorBrewer)
library(lubridate)

# old eda files found in brickman/v2_files/datasets_code/eda.R

setwd("/mnt/ecocast/projectdata/calanusclimate")

library(corrplot)

cor_mat <- readr::read_csv(file.path("/mnt/ecocast/projectdata/calanusclimate",
                                     "src/tc_datasets/ae_dryweight_nooutliers_chyp.csv.gz")) |>
  select(Bathy_depth:V, -land_mask) |>
  cor()

corrplot(cor_mat, addCoef.col = "black", 
         method = "circle", type = "full", order = "alphabet")

save_eda <- function(plot, filename) {
  pdf(file.path("plots", filename))
  print(plot)
  dev.off()
}

# threshold v. performance
if (FALSE) {
  set.seed(413)
  v <- "v3.10"
  species <- "C. Hyperboreus"
  
  get_folds <- function(threshold, species) {
    get_data(vars = c("Bathy_depth", "MLD", "Sbtm", "SSS", 
                      "SST", "Tbtm", "U", "V"), 
             interval = "mon",
             date_start = "1990-01-01",
             date_end = "2015-12-31",
             threshold = threshold,
             species = species, 
             vertical_correction = TRUE,
             add_source = NULL) |>
      vfold_cv(strata = patch, v = 5, repeats = 1)
  }
  
  wkf <- get_v_wkf(v)
  
  helper <- function(threshold) {
    folds <- get_folds(threshold, species)
    wkf |> 
      fit_resamples(folds, metrics = metric_set(roc_auc)) |>
      collect_metrics(summarize = FALSE) |>
      mutate(threshold = threshold)
  }
  
  thresholds <- seq(1000, 10000, by = 1000)
  thresholds <- c(1000, 2000, 3333, 5000, 10000)
  
  res <- thresholds |>
    lapply(helper) |>
    bind_rows()
  
  p <- ggplot(res, aes(x = threshold, y = .estimate)) +
    geom_point() +
    labs(x = "AUC", y = "Patch Threshold") +
    ggtitle(paste(species, "AUC vs. Threshold"))
  
  p
  
  p |> save_analysis(v, "auc_vs_threshold")
}

# date analyses 
if (FALSE) {
  
  # read in dataset 
  base <- readr::read_csv("src/vertcorr_merged_ae.csv.gz")
  
  date_histogram <- ggplot(base, aes(x = date, fill = src)) +
    theme_bw() + 
    geom_histogram(bins = 40, size = .2, col = "white") +
    annotate("rect", xmin = as.Date("1990-01-01"), 
             xmax = as.Date("2015-12-31"), 
             ymin = -Inf, ymax = Inf, alpha = .2) +
    ggtitle('Count of Calanus datapoints by Year') +
    labs(x = "Date", y = "Count", fill = "Source")
  
  save_eda(date_histogram, "date_histogram.pdf")
  
  # decade split
  decade <- seq(from = as.Date("1970-01-01"), to = as.Date("2030-01-01"), 
                by = "10 years")
  wdecade <- base |> 
    mutate(decade = cut(date, breaks = decade))
  decade_split <- split(wdecade, wdecade$decade)
  
  # helper method to plot a decade
  plot_decade <- function(data, decade_label) {
    ggplot(data, aes(x = lon, y = lat, col = log10(abundance + 1))) +
      geom_polygon(data = ggplot2::map_data("world"), 
                   aes(long, lat, group = group), 
                   fill = "gray85", colour = "gray70", size = .2) +
      coord_quickmap(xlim = c(-76, -38), ylim = c(35, 58), expand = FALSE) +
      theme_bw() +
      geom_point(size = .1, alpha = .8) +
      scale_color_viridis() +
      guides(colour = guide_legend(override.aes = list(size=2))) +
      theme(panel.grid.major = element_blank()) +
      ggtitle(paste0("Cfin Abundance - ", format(year(decade_label)), "s")) +
      annotate("text", x = -45, y = 55, label = paste("n = ", nrow(data)))
  }
  
  imap(decade_split, plot_decade) |>
    save_eda("cfin_abundance_by_decade.pdf")
  
}

# correlation matrix
if (FALSE) {
  brickman <- read_brickman(scenario = "PRESENT", 
                            vars = c("Bathy_depth", "MLD", "SST", "SSS", 
                                     "Tbtm", "Sbtm", "U", "V"),
                            add = NULL, 
                            interval = "ann",
                            form = "tibble") |>
    na.omit() |>
    select(-x, -y)
  
  cor_mat <- cor(brickman)
  
  library(corrplot)
  corrplot(cor_mat, addCoef.col = "black", 
           method = "circle", type = "full", order = "alphabet") |>
    save_eda("brickman_correlation_matrix_annotated.pdf")
}

# abundance bar charts 
if (FALSE) {
  data <- get_data(vars = "abundance", 
           interval = "mon",
           species = "C. Finmarchicus",
           vertical_correction = TRUE,
           add_source = NULL)
  
  data |>
    mutate(value = cut(abundance, c(-Inf, 0, 10000, Inf),
                       labels = c("0", "1-10k", ">10k"))) |>
    count(value) |>
    mutate(percent = n/sum(n)) |>
    readr::write_csv("plots/cfin_abundance_table.csv")
  
  base <- ggplot(data, aes(x = log10(abundance + 1))) +
    theme_bw() +
    ggtitle("C. Finmarchicus abundance - vertically corrected")
  
  plot_agg <- base +
    geom_histogram(bins = 75, col = "white", fill = "orange") +
    geom_vline(xintercept = 4)
  
  plot_mon <- base +
    geom_histogram(bins = 50, fill = "orange") +
    facet_wrap(~month, nrow = 4, ncol = 3) +
    geom_vline(xintercept = 4)
  
  save_eda(list(plot_agg, plot_mon), "species abundance/cfin_abundance.pdf")
  
  ###
  
  gla <- get_data(vars = "abundance", 
                  interval = "mon",
                  species = "C. Glacialis",
                  vertical_correction = FALSE,
                  add_source = NULL)

  gla |>
    mutate(value = cut(abundance, c(-Inf, 0, 1750, 5000, 10000, Inf))) |>
    count(value) |>
    mutate(percent = n/sum(n)) |>
    readr::write_csv("plots/species abundance/cgla_abundance_table.csv")
  
  gla_nonzero <- gla |>
    filter(abundance != 0)
  
  quantile(gla_nonzero$abundance, probs = .7)
  
  base <- ggplot(gla_nonzero, aes(x = log10(abundance + 1))) +
    theme_bw() +
    ggtitle("C. Glacialis abundance")
  
  plot_agg <- base +
    geom_histogram(bins = 75, col = "white", fill = "orange")
  
  plot_mon <- base +
    geom_histogram(bins = 50, fill = "orange") +
    facet_wrap(~month, nrow = 4, ncol = 3)
  
  save_eda(list(plot_agg, plot_mon), "species abundance/cgla_abundance.pdf")
}

# Plot Threshold
if (FALSE) {
  brickman <- brickman::read_brickman(scenario = "PRESENT", 
                                      vars = c("Bathy_depth", "land_mask"), 
                                      interval = "ann", 
                                      form = "stars") |>
    st_downsample(n = 1) |>
    st_as_sf(coords = c("x", "y"), crs = 4326) |>
    filter(land_mask == 1) |>
    na.omit()
  
  base <- 40000 * 195
  slope <- 100 * 195
  
  # performing threshold conversion
  threshold_method <- function(Bathy_depth, base, slope) {
    pmin(pmax(Bathy_depth - 300, 0) * slope + base,
         1500000*195)
  }
  
  brickman <- brickman |>
    mutate(threshold = threshold_method(Bathy_depth, base, slope))
  
  regions <- read_sf(dsn = "src/shapefiles/Regions_dw_vd_poly_all.shp") |>
    st_make_valid() |>
    st_transform(crs = 4326)
  
  b_crop <- brickman[st_intersects(brickman, regions, sparse = FALSE) |>
                       apply(1, function(u) any(u)),]
  
  plot <- ggplot() +
    geom_sf(data = brickman, aes(fill = log10(threshold + 1),
                                 col = log10(threshold + 1)), size = .1) +
    viridis::scale_fill_viridis() +
    viridis::scale_color_viridis() +
    guides(color = 'none') +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray") +
    coord_sf(xlim = c(-77.0, -42.5), ylim = c(36.5, 56.7), expand = TRUE) +
    theme_bw() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(1.05, "cm")) +
    ggtitle("E. Glacialis Energetic Biomass Threshold") +
    labs(fill = "Threshold (ug) (log10(x+1))", x = "Longitude", y = "Latitude")
  
  save_eda(plot, "Threshold_log.pdf")
}

# Threshold graph
if (FALSE) {
  threshold_method <- function(Bathy_depth, base, slope) {
    pmin(pmax(Bathy_depth - 300, 0) * slope + base,
         1500000*195)
  }
  
  Bathymetries <- c(0, 300, 1000)
  
  data.frame (x = Bathymetries, 
              y = threshold_method(Bathymetries, 40000*195, 100*195)) |>
  ggplot() +
    geom_line(aes(x, y))  +
    labs (x = "Bathymetry (m)", y = "Calanus Biomass Threshold (ug)") +
    theme_bw() +
    ggtitle("Calanus Biomass Threshold Relative to Depth") + 
    ylim(c(0, NA))
}

  