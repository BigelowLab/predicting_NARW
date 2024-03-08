source("setup.R")
source("datasets/training_data.R")
source("run_version_help.R")
source("analyse.R")

#' DOUBLE CHECK:
#' - correct version number?
#' - correct model specifications? 
#' - correct recipe specifications? 
#' - has the yaml already been run? 

v <- "v3.11.01"
config <- read_config(v)

set.seed(config$model$seed)

data <- data_from_config(config$training_data)

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
plots_predictions(v, 
                  downsample = 3,
                  save_months = 1:12,
                  scenarios = c(4, 5),
                  verbose = TRUE, 
                  comparison_scenario = 5)

get_threshold_plots(v, 
                    plot_scenarios = 4,
                    threshold = .5,
                    comparison_scenario = 5,
                    downsample = 3)

## ADDITIONAL ANALYSES: PICK AND CHOOSE AT LEISURE 
# var_cont(v, 
#          training(data_split), 
#          wkf = wkf_fit, 
#          monthly = c(TRUE, FALSE)) # hidden parameters: formula, num_folds

vi_list <- var_imp(v, wkf = NULL)
vi_list

heatmap_geography(v)

pred_v_abundance(v)

folds <- data |>
  vfold_cv(v = 5, repeats = 1, strata = patch)

annual_response_curves(v, wkf = NULL, 
                       folds = folds, #NULL
                       num_pts = 50,
                       mid_mon = 6,
                       var_imp = vi_list)
# response_curves("v3.21", NULL) # if creating complex model, use folds = NULL

# accuracy_vs_variables(v, position = "fill")
# accuracy_vs_variables(v, position = "stack")

# compare new versions to old one
if (FALSE) {
  compare_versions(old_v = "v3.11.01", new_v = "v3.11", 
                   year = NA, scenario = "PRESENT")
}


