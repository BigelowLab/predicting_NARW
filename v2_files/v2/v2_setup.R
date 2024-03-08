source("datasets_code/training_data.R")

data <- get_ae_vars(b_vars = c("Bathy_depth", "MLD", "Sbtm", "SSS", 
                               "SST", "Tbtm", "U", "V"),
                    interval = "mon",
                    date_start = "1990-01-01",
                    date_end = "2015-12-31",
                    threshold = NULL,
                    source = "cfin",
                    add = NULL, 
                    downsample = FALSE)

set.seed(405)
# original data split
data_split <- initial_split(data, prop = 3/4, strata = patch)
training <- training(data_split)
testing <- testing(data_split)

# resampling
folds <- vfold_cv(training, v = 5, repeats = 1, strata = patch)
set.seed(NULL)

recipe <-
  recipe(patch ~ ., data = training) |>
  update_role(lon, lat, new_role = "ID") |>
  step_normalize(all_numeric_predictors())

recipe <- recipe(patch ~ ., data = training) |>
  update_role(lat, lon, U, V, new_role = "ID") |>
  step_mutate(vel = sqrt(U^2 + V^2), role = "predictor") |>
  step_normalize(all_numeric_predictors())

