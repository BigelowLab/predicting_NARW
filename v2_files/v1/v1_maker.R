root <- "/mnt/ecocast/projects/students/ojohnson/brickman"
# helper functions
source(file.path(root, "model/model_help.R"))
source(file.path(root, "datasets_code/training_data.R"))

# INCOMPLETE
# no compatibility for engines other than RF
# cannot generate results for mean merge folds 

#' Builds, fits, and saves a model from given configuration
#' 
#' @param v chr, the version to be built 
#' @return saves a workflow/list of workflows and analysis to the file system 
#'   and returns the auc for the model -- monthly auc if monthly 
make_version <- function(v = "v1.30") {
  config <- read_config(v)
  model <- config$model 
  
  #retrieving training data
  training_data <- get_training_data(config$training_data)
  
  # generating an unfitted workflow using training_data as a template
  calanus_wkf <- generate_workflow(training_data)
  
  set.seed(model$seed) 
  # are there folds? 
  if(!is.null(model$folds)) {
    # generating folds object
    result <- rsample::vfold_cv(training_data, 
                                v = model$folds$n, 
                                strata = patch) |>
      # generating predictions for each model
      augment_folds(calanus_wkf) |>
      #  saving results to file system and returning auc results
      save_results(v, method = model$folds$merge_method)
    
  } else {
    # single split
    data_split <- initial_split(training_data, prob = 3/4)
    
    # generating fit and saving to file
    fit <- fit(calanus_wkf, data = training(data_split)) |>
      save_model(v)
    
    # generating augmented predictions 
    result <- augment(calanus_fit, testing(data_split)) |>
      analyse(v)
  }
  set.seed(NULL)
  
  result
}







