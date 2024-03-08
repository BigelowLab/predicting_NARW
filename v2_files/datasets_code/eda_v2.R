source("/mnt/ecocast/projects/students/ojohnson/brickman/setup.R")
library(groupdata2)
library(viridis)
library(RColorBrewer)

setwd("/mnt/ecocast/projectdata/calanusclimate")

# plot change in brickman variables
if (FALSE) {
setwd("plots/Brickman Variables")
ncol = 51
palette <- colorRampPalette(c("midnightblue", #cool end
                              "midnightblue",
                              "midnightblue",
                              "deepskyblue1",
                              "gray94", 
                              "goldenrod1",
                              "orangered3",
                              "orangered3", 
                              "orangered3"))(ncol)

# plotting change in each variable 
for (var in mon_vars()) {
  var_data <- brickman::read_brickman(scenario = "RCP85", 
                                      year = 2075, 
                                      vars = var, 
                                      add = NULL,
                                      interval = "mon",
                                      form="stars")
  
  data_list <- 1:12 |>
    map(~var_data |> 
          slice(month, .x) |>
          st_downsample(n = 3) |>
          st_as_sf(x, as_points = TRUE, na.rm = TRUE, crs = 4326))
  
  bound <- data_list |>
    map(~list(max(.x[[var]]), min(.x[[var]]))) |>
    unlist() |>
    abs() |>
    max() |>
    ceiling()
  
  pdf(paste0(var, ".pdf"))
  for (month in 1:12) {
    plot(data_list[[month]],
         cex = .3,
         pch=15, 
         breaks = seq(-bound, bound, length.out = ncol + 1),
         pal = palette,
         main = paste(var, "(RCP85 2075)", as_month(month))) |>
      print()
  }
  dev.off()
}
}

# plot current brickman variable values
if (FALSE) {
  dir.create("plots/Brickman Variables/PRESENT")
  setwd("plots/Brickman Variables/PRESENT")
  
  present_vars <- brickman::read_brickman(scenario = "PRESENT", 
                                         year = NA, 
                                         vars = mon_vars(),
                                         add = NULL, 
                                         interval = "mon", 
                                         form = "stars")
  
  data_list <- 1:12 |>
    map(~present_vars |> 
          slice(month, .x) |>
          st_downsample(n = 3) |>
          st_as_sf(x, as_points = TRUE, na.rm = TRUE, crs = 4326) |>
          na.omit())
  
  for(var in mon_vars()) {
    variable_data <- data_list |>
      map(~select(.x, var))
    
    pdf(paste0(var, ".pdf"))
    for (month in 1:12) {
      plot(variable_data[[month]],
           cex = .3,
           pch = 15, 
           nbreaks = 21,
           main = paste(var, "(PRESENT)", as_month(month))) |>
        print()
    }
    dev.off()
  }
                                
  
  # bathymetry
  if (FALSE) {
    bathy <- brickman::read_brickman(scenario = "PRESENT", 
                                     year = NA, 
                                     vars = "Bathy_depth", 
                                     add = NULL,
                                     interval = "ann",
                                     form="stars") |>
      st_downsample(n = 1) |>
      st_as_sf(x, as_points = TRUE, na.rm = FALSE, crs = 4326) |>
      filter(Bathy_depth != 0)
    
    ncol = 20
    pdf("Bathymetry.pdf")
    plot(bathy["Bathy_depth"],
         cex = .17,
         pch=15,
         breaks = seq(0, 6500, length.out = ncol+1),
         pal = viridis(ncol, direction = -1),
         main = "Bathymetry")
    dev.off()
  }
}

#############

# plot of calanus points
if(FALSE) {
source("/mnt/ecocast/projects/students/ojohnson/brickman/datasets_code/training_data.R")
data <- get_ae_vars(b_vars = NULL, interval = "mon")

pdf("plots/ecomon_azmp_faceted.pdf")
ggplot(data, aes(x = lon, y = lat, col = log10(abundance))) +
  facet_wrap(~month, nrow = 3, ncol = 4) +
  geom_polygon(data = ggplot2::map_data("world"), 
               aes(long, lat, group = group), 
               fill = "gray85", 
               colour = "gray70",
               size = .2) +
  coord_quickmap(xlim = c(-76, -38),
                 ylim = c(35, 58),
                 expand = TRUE) +
  theme_bw() +
  geom_point(size = .1, alpha = .8) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.x= element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.title.y= element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank()) +
  labs(col = "Patch")
dev.off()
}

# plot of hyperboreus points
if(FALSE) {
  data <- readr::read_csv("src/hyperboreus_ae.csv.gz", 
                          col_types = readr::cols())
  pdf("plots/hyperboreus_patch.pdf")
  ggplot(data, aes(x = lon, y = lat, col = abundance > 5000)) +
    facet_wrap(~month, nrow = 3, ncol = 4) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group), 
                 fill = "gray85", 
                 colour = "gray70",
                 size = .2) +
    coord_quickmap(xlim = c(-76, -38),
                   ylim = c(35, 58),
                   expand = TRUE) +
    theme_bw() +
    geom_point(size = .1, alpha = .8) +
    guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),
          axis.title.x= element_blank(),
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank(),
          axis.title.y= element_blank(),
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_blank()) +
    labs(col = "Patch") + 
    ggtitle("Hyperboreus Patch (threshold : 5000)")
  dev.off()
}

# plot of dud points
if(FALSE) {
duddata <- readr::read_csv(file.path(root, "dud_midatlantic.csv.gz"),
                           col_types = readr::cols())

pdf("plots/dud_locations.pdf")
ggplot(duddata, aes(x = lon, y = lat)) +
  geom_polygon(data = ggplot2::map_data("world"), 
               aes(long, lat, group = group), 
               fill = "gray85", 
               colour = "gray70",
               size = .2) +
  coord_quickmap(xlim = c(-80, 0),
                 ylim = c(0, 60),
                 expand = TRUE) +
  theme_bw() +
  geom_point(size = 1, color = "red") +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.x= element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.title.y= element_blank()) +
  ggtitle("Dud data points locations")
dev.off()
}

# defining local function that will filter the data based on date 
filter_dates <- function(data, date_start, date_end) {
  if (!is.null(date_start)) {
    data <- filter(data, date >= as.Date(date_start))
  }
  if (!is.null(date_end)) {
    data <- filter(data, date <= as.Date(date_end))
  }
}

data <- readr::read_csv("src/ecomon_azmp_brickman.csv.gz",
                        col_types = readr::cols()) |>
  filter_dates("1990-01-01", "2015-12-31")

# actual data vs. Brickman data 
if(FALSE) {
  ecomon_vars <- read_ecomon(list_data(id = "0187513"), simplify = FALSE) |>
    na.omit() |>
    mutate(month = lubridate::month(date),
           year = lubridate::year(date)) |>
    rename(abundance = calfin_10m2) |>
    select(year, depth, month, abundance, sfc_temp:btm_salt)
  
  brickman_source <- brickman::compose_filename("PRESENT")
  brickman_vars <- c("SST", "SSS", "Tbtm", "Sbtm")
  mvb <- brickman::extract_points(brickman_source, 
                                  brickman_vars, 
                                  ecomon_vars, 
                                  complete = FALSE,
                                  simplify_names = TRUE) |> 
    bind_cols(ecomon_vars) |>
    #filtering so that monthly data matches given month of calanus
    rowwise() |>
    mutate_at(brickman_vars, ~.x[[month]]) |>
    ungroup()
  
  compare_vals <- function(names) {
    brick <- names[[1]]
    eco <- names[[2]]
    
    test <- mvb |> 
      split(mvb$patch) |>
      map(~cor.test(.x[[brick]], .x[[eco]])) |>
      map_dfr(tidy) |>
      mutate(class = c("NO PATCH", "PATCH")) |>
      rename(corr = estimate) |>
      select(class, corr, p.value, conf.low, conf.high)
    
    table <- tableGrob(test, theme = ttheme_minimal())
    plot <- (ggplot(mvb, 
           aes(x = get(eco), y = get(brick))) +
      facet_wrap(~ patch) +
      theme_bw() +
      geom_point(alpha = .12) +
      geom_abline(color = "red") +
      tune::coord_obs_pred() + 
      ylab("Brickman estimate") +
      xlab("Measured") +
      ggtitle(paste0("Ecomon vs. Brickman ", brick)))
    
    grid.arrange(plot, table, ncol = 1)
  }
  
  names_lists <- list(
    c("SST", "sfc_temp"),
    c("SSS", "sfc_salt"),
    c("Tbtm", "btm_temp"),
    c("Sbtm", "btm_salt")
  )
  
  pdf("plots/bve_cortest.pdf")
  for(names in names_lists) {
    compare_vals(names)
  }
  dev.off()
}

# abundance vs. monthly variables 
if(FALSE) {
plotMonVar <- function(var) {
  value <- data[[var]]
  ggplot(data, aes(x = value, y = abundance)) +
    facet_wrap(~ month, nrow = 4, ncol = 3) +
    theme_bw() +
    ylab("abundance(log)") +
    scale_y_log10() +
    geom_point(alpha = .1, cex = 1) + 
    geom_hline(yintercept = 10000, color = "red") +
    ggtitle(var)
}

pdf("plots/abundance_v_monvars.pdf")
ggplot(data, aes(x = abundance)) +
  facet_wrap(~ month, nrow = 4, ncol = 3) +
  theme_bw() + 
  geom_histogram(bins = 40, size = .2, col = "white", fill = "orange") +
  geom_vline(xintercept = 10000, color = "red", size = .3) +
  xlab("abundance(log)") +
  scale_x_log10()

plotMonVar("MLD") + scale_x_log10()

# bathymetry
plotMonVar("Bathy_depth") + 
  scale_x_log10() +
  xlab("Bathymetry(log)")

for (var in mon_vars()) {
  print(plotMonVar(var))
}
dev.off()
}

# abundance vs. annual variables
if(FALSE){
pdf("plots/abundance_v_annvars.pdf")

# histogram of raw abundance
ggplot(data, aes(x = abundance)) + 
  theme_bw() + 
  geom_histogram(bins = 50, col = "white", fill = "orange") +
  geom_vline(xintercept = 10000, color = "black") +
  scale_x_log10()

ggplot(jan, aes(x = MLD)) + 
  theme_bw() + 
  geom_histogram(bins = 50, col = "white", fill = "orange")

#sbtm
ggplot(data, mapping = aes(x = abundance, y = Sbtm_ann)) +
  geom_point(alpha = .01, cex = 2) +
  geom_vline(xintercept = 10000, color = "red") +
  scale_x_log10()

#tbtm
ggplot(data, mapping = aes(x = abundance, y = Tbtm_ann)) +
  geom_point(alpha = .01, cex = 2) +
  geom_vline(xintercept = 10000, color = "red") +
  scale_x_log10()

#v
ggplot(data, mapping = aes(x = abundance, y = V_ann)) +
  geom_point(alpha = .01, cex = 2) +
  geom_vline(xintercept = 10000, color = "red") +
  scale_x_log10()

#u
ggplot(data, mapping = aes(x = abundance, y = U_ann)) +
  geom_point(alpha = .01, cex = 2) +
  geom_vline(xintercept = 10000, color = "red") +
  scale_x_log10()

#sss
ggplot(data, mapping = aes(x = abundance, y = SSS_ann)) +
  geom_point(alpha = .01, cex = 2) +
  geom_vline(xintercept = 10000, color = "red") +
  scale_x_log10()

#sst
ggplot(data, mapping = aes(x = abundance, y = SST_ann)) +
  geom_point(alpha = .01, cex = 2) +
  geom_vline(xintercept = 10000, color = "red") +
  scale_x_log10()

# mld
ggplot(data, mapping = aes(x = abundance, y = MLD_ann)) +
  geom_point(alpha = .03, cex = 2) +
  geom_vline(xintercept = 10000, color = "red") + 
  scale_x_log10()

# bathymetry
ggplot(data, mapping = aes(x = abundance, y = Bathy_depth)) +
  geom_point(alpha = .03, cex = 2) +
  geom_vline(xintercept = 10000, color = "red") +
  scale_x_log10() +
  scale_y_log10()

dev.off()
}

# creating bar charts
if (FALSE) {
  library(groupdata2)
  
  root <- "/mnt/ecocast/projectdata/calanusclimate"
  
  data <- readr::read_csv(file.path(root, "src/ecomon_azmp_brickman.csv.gz"),
                          col_types = readr::cols()) |>
    mutate(patch = abundance > 10000) |>
    na.omit()
  
  ggplot(data, aes(x = Sbtm_ann)) + 
    theme_bw() + 
    geom_histogram(bins = 50, col = "white", fill = "red") +
    scale_x_log10()
  
  downdata <- downsample(data, "patch")
  
  # patch v nopatch
  dpatch <- downdata |> filter(patch)
  dnopatch <- downdata |> filter(!patch)
  
  V <- ggplot(mapping = aes(x = V_ann)) +
    theme_bw() +
    geom_histogram(data = dpatch, bins = 50, col = "white", alpha = .6, 
                   mapping = aes(fill = "Patch")) +
    geom_histogram(data = dnopatch, bins = 50, col = "white", alpha = .6,
                   mapping = aes(fill = "No Patch")) + 
    ggtitle("V for Calanus Data - patch vs. no patch (downsampled)") +
    scale_x_log10() +
    xlab("SSS (log10)")
  
  pdf(file = file.path(root, "plots", "plots.pdf"))
  Bathymetry
  SST
  MLD
  SSS
  Sbtm
  Tbtm
  U
  V
  dev.off()
  
  # changes to the recipe don't really affect random forest
  make_leaflet <- function(mon) {
    leaflet(data = data |> filter(month == mon)) |> 
      addTiles() |> 
      addCircleMarkers(radius=.4, 
                       color = ~colorFactor(cm.colors(2), data$patch)(patch))
  }
}

###### Carnegie data exploration
if (FALSE) {
data <- readr::read_csv("src/carnegie/carnegie.csv.gz") |>
  mutate(month = month |> as.factor())
pdf("plots/carnegie_locations.pdf")
ggplot(data, aes(x = lon, y = lat, col = month)) +
  geom_polygon(data = ggplot2::map_data("world"), 
               aes(long, lat, group = group), 
               fill = "gray85", 
               colour = "gray70",
               size = .2) +
  coord_quickmap(xlim = c(-105, 25),
                 ylim = c(15, 75),
                 expand = TRUE) +
  theme_bw() +
  geom_point(size = 1, alpha = .8) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  theme(legend.position = "bottom") +
  ggtitle("Carnegie Absences")
dev.off()
}

##### Pelagic and gill data exploration
if (FALSE) {
  pdf("plots/gill_locations.pdf")
  ggplot(bcarm |> mutate(month = as.factor(month)), 
         aes(x = lon, y = lat, col = month)) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group), 
                 fill = "gray85", 
                 colour = "gray70",
                 size = .2) +
    coord_quickmap(xlim = c(-105, 25),
                   ylim = c(15, 75),
                   expand = TRUE) +
    theme_bw() +
    geom_point(size = 1, alpha = .1) +
    guides(colour = guide_legend(override.aes = list(size=2, alpha = 1))) +
    theme(legend.position = "bottom") +
    ggtitle("Gill negatives")
  dev.off()
}

e_sf <- ecomon_vars |> st_as_sf(coords = c("lon", "lat"), crs = 4326)
bathy <- brickman::extract_points(compose_filename("PRESENT"), 
                                  "Bathy_depth", 
                                  e_sf, 
                                  complete = FALSE,
                                  simplify_names = TRUE) |> 
  bind_cols(ecomon_vars)

readr::write_csv(bathy, "ecomon_data.csv.gz")


