setwd("/mnt/ecocast/projects/students/ojohnson/brickman")
source("v2/v2_setup.R")
library(ggforce)

recipe <- recipe |>
  step_dummy(all_nominal_predictors())

# code to demonstrate grid patterns
if (FALSE) {
  mlp_param <- extract_parameter_set_dials(mlp_spec)
  mlp_param |> extract_parameter_dials("hidden_units")
  # Hidden Units (quantitative)
  #Range: [1, 10]
  
  # regular grid 
  # advantage: relationships btwn tuning parameters & metrics r easily understood
  mlp_param |> 
    grid_regular(levels = c(hidden_units = 3, penalty = 2, epochs = 2)) |>
    ggplot(aes(x = .panel_x, y = .panel_y)) +
    geom_point() +
    geom_blank() +
    ggforce::facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) +
    labs(title = "Regular grid ")
  
  
  # space filling grid - more effective at representing
  # parameter space
  mlp_param |> 
    grid_latin_hypercube(size = 20, original = FALSE) |>
    ggplot(aes(x = .panel_x, y = .panel_y)) +
    geom_point() +
    geom_blank() +
    ggforce::facet_matrix(vars(hidden_units, penalty, epochs), layer.diag = 2) +
    labs(title = "Latin hypercube design w/ 20 candidates")
}

model_spec <- 
  mlp(mode = "classification",
      engine = "keras",
      hidden_units = tune(),
      dropout = tune(),
      epochs = tune(),
      activation = "softmax")

model_spec <- 
  mlp(mode = "classification",
      engine = "keras",
      hidden_units = 300,
      dropout = .75,
      epochs = 25,
      activation = "softmax")

# keras code
if (FALSE) {
  verbose = TRUE
  save_months = 1:12
  save_scenarios = 5
  downsample = 2
  calanus_wkf <- fitted
  get_plots(v, c(5), 2)
}


