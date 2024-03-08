library(xgboost)

### MODEL DEFINITION 

# model_spec <- boost_tree() |>
#   set_mode("classification") |>
#   set_engine("xgboost") |>
#   set_args(trees: 1000.0,
#            tree_depth: 7.0,
#            learn_rate: 0.0177,
#            min_n: 15.0,
#            mtry: 4.0,
#            sample_size: 0.367,
#            loss_reduction: 1.77e-08)#v4.04.01

# model_spec <- boost_tree() |>
#   set_mode("classification") |>
#   set_engine("xgboost") |>
#   set_args(trees = 1000,
#            tree_depth = 15,
#            learn_rate = .000132,
#            min_n = 13,
#            mtry = 4,
#            sample_size = .667,
#            loss_reduction = .0000725) #v4.02.04

model_spec <- boost_tree() |>
  set_mode("classification") |>
  set_engine("xgboost") |>
  set_args(trees = 1000,
           tree_depth = 12,
           learn_rate = .00684,
           min_n = 9,
           mtry = 6,
           sample_size = .754,
           loss_reduction = .00000000613) #v4.02.03

# model_spec <- boost_tree() |>
#   set_mode("classification") |>
#   set_engine("xgboost") |>
#   set_args(trees = 25.0,
#            tree_depth = 7.0,
#            learn_rate = 0.3,
#            min_n = 1.0,
#            loss_reduction = 0.04)