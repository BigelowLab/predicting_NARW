library(xgboost)

### MODEL DEFINITION 

model_spec <- boost_tree() |>
  set_mode("classification") |>
  set_engine("xgboost") |>
  set_args(trees = 500,
           learn_rate = .1,
           tree_depth = 4,
           mtry = 5,
           min_n = 10)