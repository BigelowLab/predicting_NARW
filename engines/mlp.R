
model_spec <- mlp(mode = "classification", 
                  hidden_units = 5, 
                  penalty = .01, 
                  #dropout = .1,
                  epochs = 20,
                  activation = "relu") |>
  set_engine("keras", verbose = FALSE)


