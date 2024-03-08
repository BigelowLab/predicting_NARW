source("/mnt/ecocast/projects/students/ojohnson/brickman/model/model_help.R")
library(pROC)
path <- v_path(v="v1.10", "model")

#reading in data 
present_data <- retrieve_data("d1.10")

# generating folds 
set.seed(123)
folds <- rsample::vfold_cv(present_data, v=5, strata=patch)
set.seed(NULL)

# generating a workflow 
calanus_wkf <- generate_workflow(analysis(folds$splits[[1]]))

# generating a model for each fold and assessing its accuracy 
folds <- test_folds(folds, calanus_wkf)

ggplot()

curves <- folds |>
  rowwise() |>
  transmute(curve = list(roc_curve(augmented, truth=patch, .pred_0)), 
            auc)

autoplot(curves$curve[[1]])

### save a model to the file system
saveRDS(folds$model, 
        file = file.path(path, "model_fit.csv.gz"))














