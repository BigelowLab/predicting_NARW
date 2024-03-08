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

v <- "v5.20.1" #chyp
v <- "v5.10" #cfin
config <- read_config(v)

set.seed(config$model$seed)

# THRESHOLD METHOD DEFINITION - CHANGE THIS!!!!
tm <- flat_tm(30000*195)
post <- stephane_final(state_val = "rest", post = TRUE)

data <- data_from_config(config$training_data, 
                         threshold_method = tm)

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

ds_master <- 0

get_predictions(v, verbose = TRUE, save_scenarios = c(1, 2, 3), #c(4, 5)
                downsample = ds_master,
                save_months = 7,
                post = post,
                crop = TRUE)

# Raw and threshold plots

get_raw_plots(v, ds_master,
              plot_scenarios = c(4), 
              save_months = c(1, 4, 7, 10), 
              cropped = TRUE,
              gridded = TRUE,
              top_limit = .5)

get_threshold_plots(v, ds_master,
                    plot_scenarios = c(4), 
                    threshold = .1,
                    save_months = 1:12, 
                    cropped = TRUE, 
                    gridded = FALSE)

get_threshold_percent_plots(v, ds_master, 
                            plot_scenarios = c(4), 
                            threshold_perc = .05, 
                            filter_bathy = TRUE, 
                            save_months = 1:12, 
                            cropped = TRUE, 
                            gridded = FALSE)

if (FALSE) {
  quant_ds_master <- 1
  
  get_quantile_data(v, data, K = 100, save_scenarios = c(4, 5), 
                    save_months = 1:12,
                    downsample = quant_ds_master,
                    post = post, 
                    crop = TRUE)
  
  get_quant_perc_plots(v, downsample = quant_ds_master, 
                       year = NA, scenario = "PRESENT", 
                       save_months = 1:12, 
                       cropped = TRUE, gridded = FALSE,
                       quant_col = .50,
                       top_limit = .5)
  
  get_quant_range_plots(v, downsample = quant_ds_master, 
                        year = NA, scenario = "PRESENT", 
                        save_months = 1:12, 
                        cropped = TRUE, gridded = FALSE, ci = .95,
                        top_limit = .25)
  
  get_combined_plots("v5.10",
                     "v5.20.1", 
                     0,
                     year = 2075, 
                     scenario = "RCP85",
                     save_months = 1:12,
                     cropped = FALSE, gridded = FALSE,
                     top_limit = .5)
  
  get_combined_difference_plots("v5.10", 
                                "v5.20.1",
                                0,
                                year = 2075,
                                scenario = "RCP45", 
                                save_months = 7,
                                cropped = FALSE, gridded = FALSE)
  
  get_combined_threshold_plots("v5.10",
                               "v5.20.1", 
                               0,
                               year = 2075, 
                               scenario = "RCP85",
                               threshold = .25,
                               save_months = 1:12,
                               cropped = FALSE, gridded = FALSE)
  
  get_combined_threshold_percent_plots("v5.10",
                               "v5.20.1", 
                               0,
                               year = 2075, 
                               scenario = "RCP85",
                               threshold_perc = .1,
                               filter_bathy = FALSE,
                               save_months = 1:12,
                               cropped = FALSE, gridded = FALSE)
}

## ADDITIONAL ANALYSES: PICK AND CHOOSE AT LEISURE 
vi_list <- var_imp(v, wkf = NULL)
vi_list

heatmap_geography(v)

pred_v_dryweight(v, tm)

folds <- data |>
  vfold_cv(v = 10, repeats = 1, strata = patch)

annual_response_curves_data(v,
                            data,
                            folds = folds,
                            vimp = vi_list, 
                            vars = c("Bathy_depth", "SST", "Tbtm", "SSS",
                                     "MLD", "Vel", "Sbtm"))
