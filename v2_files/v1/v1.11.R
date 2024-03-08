source("/mnt/ecocast/projects/students/ojohnson/brickman/model/model_help.R")
version <- "v1.21" #v1.12 #v1.20 #v1.21
path <- v_path(version, "model")

#reading in data 
present_data <- retrieve_data("d1.21") # d1.00 # d1.20 #d1.21

# generating folds 
set.seed(123)
folds <- rsample::vfold_cv(present_data, v=5, strata=patch)
set.seed(NULL)

# generating a workflow 
calanus_wkf <- generate_workflow(analysis(folds$splits[[1]]))

# generating a model for each fold and assessing its accuracy 
folds <- test_folds(folds, calanus_wkf)

#using "best" model
best <- which.max(folds$auc)
auc <- round(folds$auc[[best]], 4)
workflow <- folds$model[[best]]
test_results <- folds$augmented[[best]]

save_testing_results(test_results, version, auc)

### save a model to the file system
saveRDS(workflow, 
        file = file.path(path, "model_fit.csv.gz"))




