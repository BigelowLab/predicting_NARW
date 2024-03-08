source("/mnt/ecocast/projects/students/ojohnson/brickman/setup.R")
library(groupdata2)
library(viridis)
library(RColorBrewer)
library(lubridate)

# old eda files found in brickman/v2_files/datasets_code/eda.R

setwd("/mnt/ecocast/projectdata/calanusclimate")

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

(ggplot(data) +
  geom_polygon(data = ggplot2::map_data("world"), 
               aes(long, lat, group = group)) +
  geom_point(aes(x = lon, y = lat, col = patch), alpha = .5, size = .3) +
  coord_quickmap(xlim = c(-76, -40),
                 ylim = c(35, 60),
                 expand = TRUE) +
  theme_bw()) |>
  save_eda("glacialis_locations.pdf")

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

# pelagic exploration
if (FALSE) {
  setwd("/mnt/ecocast/projectdata/calanusclimate/src/pelagic")
  files <- list.files()
  
  pelagic_data <- files |>
    map(~ readr::read_csv(.x,
                          skip = 15,
                          col_types = readr::cols()) |>
          mutate(file_source = .x)) |>
    bind_rows() |>
    rename(lon = LONGITDE, lat = LATITUDE, 
           month = MON, abundance = `VALUE-per-area`) |>
    mutate_at(c("lon", "lat", "abundance", "month"), as.numeric)
  
  brickman_pel <- pelagic_data |>
    filter(between(lon, -101.50, 24.46) &
             between(lat, 15.96, 75.25))
  
  b_cop <- brickman_pel |>
    filter(!(`Taxa-Modifiers` %in% c("-[ c1 ]-", "-[ c2 ]-", "-[ c3 ]-")) &
             `Taxa-Name` %in% c("Calanus finmarchicus", "Copepoda"))
  
  g <- b_cop |>
    group_by(`#SHP-CRUISE`, YEAR, month, DAY, TIMEgmt, lat, lon)
  
  b_cop |> filter(`Taxa-Name` == "Calanus finmarchicus") |>
    pull(abundance),
  
  
  g |> summarize (cfin = ~ filter(.x, `Taxa-Name` == "Calanus finmarchicus") |>
                    pull(abundance) |>
                    sum())
  
  gt <- g |> summarize(abundance = sum(abundance), by = `Taxa-Name`)
  
  
  
  g_taxa |> group_by("Calanus finmarchicus" %in% `Taxa-Name`) |> tally()
  
  gt |> filter("Calanus finmarchicus" %in% by)
  
  ggplot(g, aes(x = lon, y = lat)) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group), 
                 fill = "gray85", 
                 colour = "gray70",
                 size = .2) +
    geom_point(col = "red") +
    coord_quickmap(xlim = c(-105, 25),
                   ylim = c(15, 75),
                   expand = TRUE)
  
  unique(g$`Taxa-Name`)
  
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
}

  