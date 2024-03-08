
#model definition
# model_spec <- rand_forest(trees = 500,
#                           engine = "ranger", 
#                           mode = "classification")

model_spec <- rand_forest(mtry = 5, min_n = 33, trees = 500) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")