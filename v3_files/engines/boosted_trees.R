library(xgboost)

### MODEL DEFINITION 

model_spec <- boost_tree() |>
  set_mode("classification") |>
  set_engine("xgboost") |>
  set_args(trees = 1000,
           tree_depth = 6,
           learn_rate = 0.0745,
           min_n = 26,
           mtry = 5,
           sample_size = .597,
           loss_reduction = 0.00568)

# model_spec <- boost_tree() |>
#   set_mode("classification") |>
#   set_engine("xgboost") |>
#   set_args(trees = 25.0,
#            tree_depth = 7.0,
#            learn_rate = 0.3,
#            min_n = 1.0,
#            loss_reduction = 0.04)