if (FALSE) {
  source("setup.R")
  source("datasets/training_data.R")
  source("run_version_help.R")
  source("analyse.R")
}

#' DOUBLE CHECK:
#' - correct version number?
#' - correct model specifications + ENGINE FILE? 
#' - correct recipe specifications? 
#' - has the yaml already been run? 

v <- "v4.02.03"
config <- read_config(v)

set.seed(config$model$seed)

data <- data_from_config(config$training_data)

count(data, patch) |> mutate(prop = n/sum(n))

data_split <- data |>
  initial_split(data, prop = 3/4, strata = patch)

# creating workflow and saving analyses to file
wkf_fit <- wkf_version(data_split, 
                       config$model$model_list, 
                       v = v, 
                       overwrite = TRUE)
#  scenario  year
# 1 RCP45     2055
# 2 RCP45     2075
# 3 RCP85     2055
# 4 RCP85     2075
# 5 PRESENT     NA
if (FALSE) {
ds_master <- 2
# plots_predictions(v,
#                   downsample = ds_master,
#                   save_months = 1:12,
#                   scenarios = c(4, 5),
#                   verbose = TRUE,
#                   comparison_scenario = 5)

get_predictions(v, verbose = TRUE, save_scenarios = c(4,5),
                downsample = ds_master)
get_predictions(v, verbose = TRUE, save_scenarios = c(1, 2, 3),
                save_months = 7,
                downsample = ds_master)

get_plots(v = v, plot_scenarios = c(4, 5), 
          comparison_scenario = 5, downsample = ds_master)

get_plots(v = v, plot_scenarios = c(1, 2, 3),
          save_months = 7,
          comparison_scenario = 4, downsample = ds_master)

# get_threshold_plots(v,
#                     plot_scenarios = 4,
#                     threshold = .333,
#                     comparison_scenario = 5,
#                     downsample = ds_master)

save_plots_gridded(v, NULL, "PRESENT", downsample = ds_master,
                   cex_override = .16)
save_plots_gridded(v, 2075, "RCP85", downsample = ds_master,
                   cex_override = .16)

gridded_threshold(v, 2075, "RCP85", downsample = ds_master,
                  threshold = .333, 
                  months = c("Jan", "Apr", "Jul", "Oct"),
                  cex_override = .16)


## ADDITIONAL ANALYSES: PICK AND CHOOSE AT LEISURE 
# var_cont(v, 
#          training(data_split), 
#          wkf = wkf_fit, 
#          monthly = c(TRUE, FALSE)) # hidden parameters: formula, num_folds

vi_list <- var_imp(v, wkf = NULL)
vi_list

heatmap_geography(v)

#pred_v_abundance(v)
pred_v_dryweight(v)

folds <- data |>
  vfold_cv(v = 10, repeats = 1, strata = patch)

#folds <- mc_cv(data, prop = 3/4, times = 20, strata = patch)

# annual_response_curves(v, wkf = NULL, 
#                        folds = folds, #NULL
#                        var_imp = vi_list)

annual_response_curves_data(v,
                            data,
                            folds = folds,
                            vimp = vi_list, 
                            vars = c("Bathy_depth", "SST", "Tbtm", "SSS",
                                     "MLD", "Vel", "Sbtm"))

# accuracy_vs_variables(v, position = "fill")
# accuracy_vs_variables(v, position = "stack")
}

# compare new versions to old one
if (FALSE) {
  compare_versions(old_v = "v4.01.00", new_v = "v4.02.02", 
                   year = NA, scenario = "PRESENT",
                   cropped = TRUE,
                   downsample = ds_master)
  
  combined_preds(vcfin = "v4.02.02", vchyp = "v4.03", 
                 year = NA, scenario = "PRESENT", downsample = ds_master)
}

# paper plots
if (FALSE) {
ggplot(mutate(data, 
              month = factor(month.abb[month],
                                levels = month.abb)), 
       aes(x = lon, y = lat)) +
  geom_polygon(data = ggplot2::map_data("world"), 
               aes(long, lat, group = group),
               fill = "lightgray", col = "gray", 
               linewidth = .1) +
  geom_point(aes(col = patch), alpha = .7, size = .03) +
  coord_quickmap(xlim = c(-76, -40), ylim = c(35, 60), expand = TRUE) +
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  ggtitle("b) C. hyperboreus") + 
  facet_wrap(~month, nrow = 2, ncol = 6)
  
ggplot(data, aes(x = lon, y = lat)) +
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "lightgray", col = "gray", 
                 linewidth = .3) +
    geom_point(aes(col = src), alpha = .4, size = .1) +
    scale_color_manual(values = c("azmp" = "purple", "ecomon" = "orange")) +
    coord_quickmap(xlim = c(-76, -40), ylim = c(35, 60), expand = TRUE) +
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")
}