
data <- 
  data_from_config(read_config("v5.10")$training_data,
                   threshold_method = flat_tm(30000*195))

count(data, patch) |> mutate(prop = n/sum(n))

# calculate optimal threshold
if (FALSE) {
  library(PresenceAbsence)
  
  data <- get_testing("v5.10") |>
    transmute(PlotID = "test", observed = (patch |> as.numeric()) - 1, 
              pred = .pred_1)
  
  optimal.thresholds(DATA = data,
                     threshold = seq(0, .5, by = .01),
                     na.rm = TRUE,
                     opt.methods = c(1:3))
  
  #'         Method pred
  #' 1      Default 0.50
  #' 2    Sens=Spec 0.14
  #' 3 MaxSens+Spec 0.10
}

set.seed(132)
splits <- initial_split(data, strata = patch, prop = 3/4)
test <- testing(splits)
train <- training(splits)
folds <- vfold_cv(train, v = 5, repeats = 1, strata = patch)

# tuning boosted regression tree
if (FALSE) {
  
  #tune <- readr::read_csv(v_path("v4.02.03", "model", "tune_results.csv"))
  #tune
  
  xgb_spec <- boost_tree(
    trees = tune(), ## model complexity
    tree_depth = tune(), 
    min_n = 10, 
    #loss_reduction = tune(),              
    #sample_size = tune(),  ## randomness
    mtry = 5, 
    learn_rate = tune(),  ## step size
  ) |>
    set_engine("xgboost") |> 
    set_mode("classification")
  
  # complex models don't need normalization
  recipe <- recipe(patch ~ ., data = train) |>
    update_role(lat, lon, U, V, new_role = "ID") |>
    step_log(Bathy_depth, offset = 1, base = 10) |>
    step_mutate(Vel = sqrt(U^2 + V^2), role = "predictor") |>
    step_normalize(all_numeric_predictors()) |> # retain for vip()
    step_dummy(all_nominal_predictors())
  
  workflow <- workflow(preprocessor = recipe, 
                       spec = xgb_spec)
  
  xgb_grid <- grid_regular(
    trees(c(500, 1000)),
    tree_depth(c(2, 6)),
    #loss_reduction(c(0,)),
    #sample_size = sample_prop(c(.7, .9)),
    learn_rate(c(-3, -1)),
    levels = 3
  )
  
  res <- tune_grid(
    workflow,
    resamples = folds,
    grid = xgb_grid,
    metrics = metric_set(roc_auc, accuracy)
  )
  
  best_res <- res |>
    show_best(10, metric = "roc_auc")
  autoplot(res, metric = "roc_auc")
  
  readr::write_csv(collect_metrics(res),
                   v_path("v4.04.01", "model", "tune_results.csv"))
  
  final_res <- select_best(res, metric = "roc_auc")
  
  # assumes train, workflow
  get_splits <- function(params) {
    strings <- workflow |>
      finalize_workflow(params) |>
      fit(train) |>
      extract_fit_engine() |>
      xgb.dump(dump_format = "text")
    total_splits = 0
    for(tree_string in strings) {
      leaves = stringr::str_count(tree_string, "leaf")
      total_splits <- total_splits + 1 - leaves
    }
    total_splits
  }
  
  get_splits(dplyr::slice(best_res, 3) |>
               select(-(.metric:std_err)))
}

# approximate overall auc 
fit_resamples(workflow, folds) |>
  collect_metrics()

######## TUNE THE MODEL ###########
if (FALSE) {
  keras_spec <- mlp(
    hidden_units = 5,
    penalty = tune(),
    epochs = 20,
    activation = "relu"
  ) |>
    set_engine("keras") |>
    set_mode("classification")
  
  recipe <- recipe(patch ~ ., data = train) |>
    update_role(lat, lon, U, V, new_role = "ID") |>
    step_log(Bathy_depth, offset = 1, base = 10) |>
    step_mutate(Vel = sqrt(U^2 + V^2), role = "predictor") |>
    step_normalize(all_numeric_predictors()) |> # retain for vip()
    step_dummy(all_nominal_predictors())
  
  workflow <- workflow(preprocessor = recipe, 
                       spec = keras_spec)
  
  
  keras_grid <- grid_regular(
    penalty(c(.01, .4), trans = NULL),
    levels = 4
  )
  
  res <- tune_grid(
    workflow,
    resamples = folds,
    grid = keras_grid,
    metrics = metric_set(roc_auc, accuracy)
  )
  
  best_res <- res |>
    show_best(10, metric = "roc_auc")
  autoplot(res, metric = "roc_auc")
} # neural network

# use with collect_metrics(), autoplot()
compare_workflows <- function(recipe_list, model_list) {
  test_workflows <- workflow_set(preproc = recipe_list, 
                                 models = model_list, 
                                 cross = FALSE)
  
  results <- test_workflows |>
    workflow_map("fit_resamples", verbose = TRUE, seed = 120, 
                 resamples = folds)
  
  results
}

### PATCH THRESHOLD ON PERFORMANCE
if (FALSE) {
rawdata <- get_ae_vars(interval = "mon", threshold = NULL)
set.seed(405)
folds <- rawdata |>
  initial_split(prop = 3/4) |>
  training() |>
  vfold_cv(v = 10, repeats = 1)
set.seed(NULL)

make_patch_recipe <- function(threshold) {
  recipe(patch ~ ., data = head(rawdata)) |>
  update_role(lon, lat, new_role = "ID") |>
  step_mutate(patch = (patch > threshold) |> as.factor()) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())
}

values <- seq(from = 10000, to = 100000, by = 15000)

recipe_list <- values |>
  lapply(make_patch_recipe)
model_list <- list("MLP" = mlp_spec)

r <- compare_workflows(recipe_list, model_list)

pdf(v_path("v2.03", "model", "patchthreshold_vs_auc.pdf"))
r |> 
  collect_metrics() |>
  bind_cols(threshold = rep(values, each = 2)) |>
  ggplot(aes(x = threshold, y = mean)) +
  theme_bw() +
  facet_wrap(~.metric, nrow = 1) +
  geom_errorbar(aes(ymin=mean-2*std_err, ymax=mean+2*std_err), 
                width = .7, col = "orange") +
  geom_point() +
  ylab("metric") +
  ggtitle("Patch threshold versus predictive capacity (MLP)")
dev.off()
}

# tuning hyperboreus model 
if (FALSE) {
  # 7.6% patch 
  data <- get_data(vars = c("Bathy_depth", "MLD", "Sbtm", "SSS", 
                            "SST", "Tbtm", "U", "V"), 
                   interval = "mon",
                   date_start = "1990-01-01",
                   date_end = "2015-12-31",
                   threshold = 3333,
                   species = "C. Hyperboreus", 
                   vertical_correction = FALSE,
                   add_source = NULL)
  
  set.seed(132)
  splits <- initial_split(data, strata = patch, prop = 3/4)
  test <- testing(splits)
  train <- training(splits)
  folds <- vfold_cv(train, v = 5, repeats = 1, strata = patch)
  
  
  xgb_spec <- boost_tree(
    trees = 1000, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),              ## first three: model complexity
    sample_size = tune(), mtry = tune(),  ## randomness
    learn_rate = tune(),                  ## step size
  ) |>
    set_engine("xgboost") |> 
    set_mode("classification")
  
  # complex models don't need normalization
  recipe <- recipe(patch ~ ., data = train) |>
    update_role(lat, lon, U, V, new_role = "ID") |>
    step_log(Bathy_depth, offset = 1, base = 10) |>
    step_mutate(Vel = sqrt(U^2 + V^2), role = "predictor") |>
    step_normalize(all_numeric_predictors()) |> # retain for vip()
    step_dummy(all_nominal_predictors())
  
  workflow <- workflow(preprocessor = recipe, 
                       spec = xgb_spec)
  
  xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    mtry = mtry(c(5, 8)),
    learn_rate(),
    size = 20
  )
  
  res <- tune_grid(
    workflow,
    resamples = folds,
    grid = xgb_grid,
    metrics = metric_set(roc_auc, accuracy)
  )
  
  best_res <- res |>
    show_best(10, metric = "roc_auc")
  
  readr::write_csv(collect_metrics(res),
                   v_path("v3.04", "model", "tune_results.csv"))
  
  final_res <- select_best(res, metric = "roc_auc")
  
  autoplot(res, metric = "roc_auc")
  
  # assumes train, workflow
  get_splits <- function(params) {
    strings <- workflow |>
      finalize_workflow(params) |>
      fit(train) |>
      extract_fit_engine() |>
      xgb.dump(dump_format = "text")
    total_splits = 0
    for(tree_string in strings) {
      leaves = stringr::str_count(tree_string, "leaf")
      total_splits <- total_splits + 1 - leaves
    }
    total_splits
  }
  
  get_splits(dplyr::slice(best_res, 3) |>
               select(-(.metric:std_err)))
}

# optimal thresholds
if (FALSE) {
  config <- read_config("v5.10")
  
  # THRESHOLD METHOD DEFINITION - CHANGE THIS!!!!
  tm <- flat_tm(30000*195)
  post <- stephane_final(state_val = "rest")
  
  data <- data_from_config(config$training_data, 
                           threshold_method = tm)
  
  optimal.thresholds(DATA = NULL, 
                     threshold = 101, 
                     which.model = 1:(ncol(DATA)-2), 
                     model.names = NULL, na.rm = FALSE, 
                     opt.methods = NULL, 
                     req.sens, 
                     req.spec, 
                     obs.prev = NULL, 
                     smoothing = 1, 
                     FPC, 
                     FNC)
}




