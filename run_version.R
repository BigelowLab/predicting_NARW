if (FALSE) {
  source("setup.R")
  source("datasets/training_data.R")
  source("run_version_help.R")
  source("analyse.R")
}

#' DOUBLE CHECK:
#' - correct version number?
#' - correct data specs? Vars + species
#' - correct model specifications + ENGINE FILE? 
#' - correct threshold method?
#' - has the yaml already been run? 

v <- "v6.03" # chyp real
# v <- "v6.02" # chyp fake
v <- "v6.01" #cfin
config <- read_config(v)

set.seed(config$model$seed)

# THRESHOLD METHOD DEFINITION!!!!
tm <- flat_tm(30000*195)
post <- stephane_final(state_val = "rest", post = TRUE)

data <- data_from_config(config$training_data, 
                         threshold_method = tm)

count(data, patch) |> mutate(prop = n/sum(n))

# data_split <- data |>
#   initial_split(data, prop = 3/4, strata = patch)
K = 100
folds <- data |>
  mc_cv(prop = .75, times = K, strata = patch)

# creating workflow and saving analyses to file
wkf_fits <- wkf_version(folds, 
                       config$model$model_list, 
                       v = v, 
                       overwrite = TRUE)

#  scenario  year
# 1 RCP45     2055
# 2 RCP45     2075
# 3 RCP85     2055
# 4 RCP85     2075
# 5 PRESENT     NA

ds_master <- 0

get_quantile_preds(v, 
                   save_scenarios = c(4, 5), 
                   save_months = 1:12,
                   downsample = ds_master,
                   post = post, 
                   crop = TRUE)

get_quantile_preds(v, 
                   save_scenarios = c(1, 2, 3), 
                   save_months = 8,
                   downsample = ds_master,
                   post = post, 
                   crop = TRUE)

# Raw and threshold plots

get_quant_raw_plots("v6.03", 0,
                    plot_scenarios = c(5), 
                    save_months = c(1:12), 
                    cropped = TRUE,
                    gridded = TRUE,
                    quant_col = .5,
                    top_limit = .5)

get_quant_threshold_plots(v, 
                          ds_master,
                          plot_scenarios = c(4), 
                          threshold = .2,
                          save_months = 1:12, 
                          cropped = TRUE, 
                          gridded = FALSE, 
                          quant_col = .5)

# Combined plots
get_quant_raw_plots(v = "v6.01", 
                    0,
                    plot_scenarios = c(5), 
                    save_months = 1:12, 
                    cropped = TRUE,
                    gridded = TRUE,
                    quant_col = .5,
                    top_limit = .5,
                    combining_v = "v6.03")

get_quant_threshold_plots("v6.01", 
                          0,
                          plot_scenarios = 4, 
                          threshold = .2,
                          save_months = c(11, 12, 1, 2, 3, 4), 
                          cropped = TRUE, 
                          gridded = TRUE, 
                          quant_col = .5,
                          combining_v = "v6.03")

get_quant_pIQR_plots("v6.01", 
                     0,
                     plot_scenarios = 5,
                     save_months = 1:12,
                     cropped = TRUE, 
                     gridded = TRUE,
                     top_limit = .3,
                     combining_v = "v6.03")

get_quant_diff_plots("v6.01", 
                     0, 
                     plot_scenarios = c(4), 
                     comparison_scenario = 1, 
                     save_months = 8, 
                     cropped = TRUE, gridded = FALSE,
                     quant_col = .5,
                     combining_v = "v6.03")

## ADDITIONAL ANALYSES: PICK AND CHOOSE AT LEISURE 
varimp <- var_imp(v, plot = FALSE)
vi_list <- varimp$Variable

roc_curves_w_ci(v)

response_curves_data(v,
                     data,
                     vimp = vi_list, 
                     vars = c("Bathy_depth", "SST", "Tbtm", #"SSS",
                              "MLD", "Vel", "Sbtm"),
                     num_pts = 100,
                     mid_mon = 8,
                     log_bathy = TRUE,
                     same_y = FALSE,
                     save_plot = FALSE,
                     show_no_post = TRUE,
                     patch_only_medians = FALSE,
                     bottom_latitude = 42,
                     post = post)

response_curve_2var(v,
                    data,
                    all_vars = c("Bathy_depth", "SST", "SSS", 
                                 "Tbtm", "MLD", "Sbtm", "Vel"),
                    var1 = "SSS",
                    var2 = "Bathy_depth",
                    mid_mon = 8,
                    patch_only_medians = FALSE,
                    post_func = post)

model_preds <- get_v_wkfs(v) |>
  apply_quantile_preds(select(data, -patch), c(.5, 1))

heatmap_geography(v, data, model_preds)

pred_v_dryweight(v, tm, model_preds)


