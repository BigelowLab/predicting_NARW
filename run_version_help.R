#source("setup.R")
source("predictions.R")
source("plots.R")
library(ggridges)

### MODEL INITIATION 

#' Creates a fitted workflow
#' 
#' @param folds df, the data used for the model 
#' @param model_name, chr, name of desired model
get_workflows <- function(folds, model_name) {
  
  template_data <- get_rsplit(folds, 1) |> training()
  # recipe
  recipe_spec <- recipe(patch ~ ., data = template_data) |>
    update_role(lat, lon, U, V, new_role = "ID") |>
    step_log(Bathy_depth, offset = 1, base = 10) |>
    step_mutate(Vel = sqrt(U^2 + V^2), role = "predictor") |>
    step_normalize(all_numeric_predictors())

  if (model_name %in% c("Boosted Regression Tree", 
                        "GAM", "Logistic Regression",
                        "MLP Neural Network")) {
    recipe_spec <- recipe_spec |>
      step_dummy(all_nominal_predictors())
  }
  
  # model spec (+ any recipe changes)
  model_file <- list("Random Forest" = "random_forest.R",
       "GAM" = "gam.R", 
       "Boosted Regression Tree" = "boosted_trees.R", 
       "MLP Neural Network" = "mlp.R",
       "Logistic Regression" = "logistic_reg.R")[[model_name]]
  source(file.path("engines", model_file))
  
  workflow_template <- workflow() |>
    add_recipe(recipe_spec) |>
    add_model(model_spec)
  
  wkfs <- folds$splits |>
    map(~fit(workflow_template, training(.x)))
}

#' Creates a workflow version 
#' 
#' @param folds rsplit, the data split object
#' @param model_types list, configuration for model
#' @param v chr, version
#' @param overwrite boolean, overwriting existing version?
#' @return a fitted workflow. version, analysis plots, and 
#'   testing results are saved to file. 
wkf_version <- function(folds, 
                        model_types, 
                        v, 
                        overwrite = FALSE) {
  
  #creating workflow
  fit_wkfs <- get_workflows(folds, model_types[[1]]$name)
  
  # creating testing analysis dataset
  wkf_augs <- pmap(list(fit_wkfs, folds$splits, 1:length(fit_wkfs)),
                   function(wkf, split, id) augment(wkf, testing(split)) |>
                     mutate(wkf_id = id)) |>
    bind_rows() |>
    select(-(MLD:V))
  
  # saving to file
  # initializing path
  path <- v_path(v, "model")
  if (dir.exists(path)) {
    if (!overwrite) {stop("Version already exists")}
  } else {
    dir.create(path, recursive = TRUE)
  }
  wd <- getwd()
  setwd(path)
  
  # saving objects
  if (model_types[[1]]$name == "MLP Neural Network") {
    fit_wkfs <- map(fit_wkfs, bundle)
  }
  
  fit_wkfs |>
    saveRDS("model_fits.csv.gz")
  wkf_augs |>
    readr::write_csv("testing_results.csv.gz")
  
  setwd(wd)
  
  fit_wkfs
}

