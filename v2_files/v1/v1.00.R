source("/mnt/ecocast/projects/students/ojohnson/brickman/model/model_help.R")
#building and saving the model
root <- "/mnt/ecocast/projectdata/students/ojohnson/brickman"

#reading in data 
present_data <- retrieve_data("d1.00")

### data split
set.seed(409)
data_split <- initial_split(present_data, prob = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)
set.seed(NULL)

### recipe
# assigning id roles to longitude and latitude, manipulating data 
calanus_rec <-
  recipe(patch ~ ., data=train_data) |>
  update_role(lon, lat, new_role="ID") |>
  step_corr(all_numeric_predictors()) |> 
  step_center(all_numeric_predictors()) |>
  step_scale(all_numeric_predictors()) |>
  prep()

### model
calanus_rf <- rand_forest(trees=500, mode="classification") |>
  set_engine("ranger") 

### bundling model and recipe together into a workflow, fitting to data
calanus_fit <-
  workflows::workflow() |>
  add_model(calanus_rf) |>
  add_recipe(calanus_rec) |>
  fit(data = train_data)

### testing
calanus_testing <- augment(calanus_fit, test_data)
# saving testing results
pdf(file.path(root, "versions/v1/v1.00/model/heat_map.pdf")) 
  calanusthreshold::heat_map(conf_mat(calanus_testing, patch, .pred_class), 
                             title="C. Finmarchicus prediction accuracy")
dev.off()

saveRDS(calanus_fit, 
        file = file.path(root, "versions/v1/v1.00/model/model_fit.csv.gz"))




