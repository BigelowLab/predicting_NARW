setwd("/mnt/ecocast/projects/students/ojohnson/brickman")
source("v2/v2_setup.R")
library(xgboost)

### MODEL DEFINITION 

# boosted regression trees require creation of dummy variables
recipe <- recipe |>
  step_dummy(all_nominal_predictors())

#v2.02
model_spec <- boost_tree() |>
  set_mode("classification") |>
  set_engine("xgboost") |>
  set_args(trees = 25.0,
           tree_depth = 7.0,
           learn_rate = 0.3,
           min_n = 1.0,
           loss_reduction = 0.04)

### HELPER FUNCTIONS ###

# retrieve the number of splits in the tree
get_splits <- function() {
  strings <- workflow() |>
    add_recipe(recipe) |>
    add_model(brt) |> 
    fit(training) |> 
    extract_fit_engine() |>
    xgb.dump(dump_format = "text")
  total_splits = 0
  for(tree_string in strings) {
    leaves = stringr::str_count(tree_string, "leaf")
    total_splits <- total_splits + 1 - leaves
  }
  total_splits
}

# assumes train, workflow
get_splits <- function(params) {
  strings <- workflow |>
    finalize(params) |>
    fit(train) |>
    extract_fit_engine() |>
    xgb.dump(dump_format = "text")
  total_splits = 0
  for(tree_string in strings) {
    leaves = stringr::str_count(tree_string, "leaf")
    total_splits <- total_splits + 1 - leaves
  }
  total_splits
}


