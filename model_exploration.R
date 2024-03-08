source("setup.R")
source("datasets/training_data.R")

# 30.1% patch
data <- get_data(vars = c("Bathy_depth", "MLD", "Sbtm", "SSS", 
                          "SST", "Tbtm", "U", "V"), 
                 interval = "mon",
                 date_start = "1990-01-01",
                 date_end = "2015-12-31",
                 threshold = 10000,
                 species = "C. Finmarchicus", 
                 vertical_correction = TRUE,
                 add_source = "pelagic")

set.seed(132)
splits <- initial_split(data, strata = patch, prop = 3/4)
test <- testing(splits)
train <- training(splits)
folds <- vfold_cv(train, v = 5, repeats = 1, strata = patch)

# building an ensemble model
if (FALSE) {
  library(stacks)
  
  # defining recipe, models
  if (TRUE) {
    recipe_spec <- recipe(patch ~ ., data = train) |>
      update_role(lat, lon, U, V, new_role = "ID") |>
      step_log(Bathy_depth, offset = 1, base = 10) |>
      step_mutate(Vel = sqrt(U^2 + V^2), role = "predictor") |>
      step_normalize(all_numeric_predictors())
    
    brt_spec <- boost_tree() |>
      set_mode("classification") |>
      set_engine("xgboost") |>
      set_args(trees = 1000, tree_depth = 7,
               learn_rate = 0.00801, min_n = 8, mtry = 7,
               sample_size = .67, loss_reduction = 0.000210)
    
    lg_spec <- logistic_reg(penalty = .0002, mixture = 1) |>
      set_engine("glmnet")
    
    rf_spec <- rand_forest(mtry = 8, min_n = 38, trees = 500) |>
      set_engine("ranger", importance = "impurity") |>
      set_mode("classification")
  }
  
  get_resample_wkf <- function(recipe, model) {
    workflow(preprocessor = recipe, 
             spec = model) |>
      fit_resamples(folds,
                    control = control_resamples(save_pred = TRUE, 
                                                save_workflow = TRUE))
  }
  
  brt_wkf <- get_resample_wkf(recipe_spec |> 
                                step_dummy(all_nominal_predictors()),
                              brt_spec)
  rf_wkf <- get_resample_wkf(recipe_spec, rf_spec)
  lg_wkf <- get_resample_wkf(recipe_spec |>
                               step_dummy(all_nominal_predictors()),
                             lg_spec)
  
  stacked <- stacks() |>
    add_candidates(brt_wkf) |>
    add_candidates(rf_wkf) |>
    add_candidates(lg_wkf)
  
  as_tibble(stacked)
  
  stack_model <- blend_predictions(stacked)
  autoplot(stack_model) |> save_analysis(v, "stackmodel_plot")
  autoplot(stack_model, type = "members") |> 
    save_analysis(v, "stackmodel_plot_members")
  
  autoplot(stack_model, type = "weights") |>
    save_analysis(v, "stackmodel_weights")
  
  fitted_stack <- stack_model |>
    fit_members()
  
  # evaluation
  
  predictions <- fitted_stack |>
    predict(test, type = "prob") |>
    bind_cols(test)
  
  roc_auc(predictions, truth = patch, .pred_0)
  predictions |>
    readr::write_csv(v_path(v, "model", "testing_results.csv.gz"))
  
  member_predictions <- test |>
    select(patch) |>
    bind_cols(predict(fitted_stack, test, type = "prob", members = TRUE))
  
  member_predictions |>
    readr::write_csv(v_path(v, "model", "testing_results_members.csv.gz"))
  
  roc_auc_mp <- member_predictions |>
    select(patch, starts_with(".pred_0"))
  
  map_dfr(roc_auc_mp, roc_auc, truth = patch, .pred_0:.pred_0_lg_wkf_1_1)
  
  map_dfr(member_predictions, roc_auc, truth = patch, startsWith(.pred_0), data = member_predictions) |>
    mutate(member = colnames(member_predictions))
}

# building a linear regression model 
#https://www.tidymodels.org/start/case-study/
if (FALSE) {
  model_spec <- logistic_reg(penalty = tune(), mixture = 1) |>
    set_engine("glmnet")
  
  recipe <- recipe(patch ~ ., data = train) |>
    update_role(lat, lon, U, V, new_role = "ID") |>
    step_log(Bathy_depth, offset = 1, base = 10) |>
    step_mutate(Vel = sqrt(U^2 + V^2), role = "predictor") |>
    step_normalize(all_numeric_predictors()) |>
    step_dummy(all_nominal_predictors()) 
  
  wkf <- workflow(preprocessor = recipe, 
                  spec = model_spec)
  
  lr_reg_grid = tibble(penalty = 10^seq(-4, -1, length.out = 30))
  
  lr_res <- wkf |>
    tune_grid(folds, 
              grid = lr_reg_grid, 
              metrics = metric_set(roc_auc))
  
  lr_res |> 
    collect_metrics() |>
    ggplot(aes(x = penalty, y = mean)) +
    geom_point() +
    geom_line() +
    scale_x_log10(labels = scales::label_number())
  
  lr_res |>
    show_best("roc_auc", n = 5) %>% 
    arrange(penalty)
}

# tuning random forest ranger 
if (FALSE) {
  # tuning random forest is expensive -- use parallel processing to speed up
  cores <- parallel::detectCores()
  
  # the tune package can do pp for you ~ but not if only using validation set
  # ranger engine allows individual trees to be computed in parallel
  # rf_mod <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) |>
  #   set_engine("ranger", num.threads = cores) |>
  #   set_mode("classification")
  
  rf_mod <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) |>
    set_engine("ranger") |>
    set_mode("classification")
  
  # complex models don't need normalization
  recipe <- recipe(patch ~ ., data = train) |>
    update_role(lat, lon, U, V, new_role = "ID") |>
    step_log(Bathy_depth, offset = 1, base = 10) |>
    step_mutate(Vel = sqrt(U^2 + V^2), role = "predictor")
    #step_normalize(all_numeric_predictors()) 
  
  workflow <- workflow() |>
    add_model(rf_mod) |>
    add_recipe(recipe)
  
  extract_parameter_set_dials(rf_mod)
  
  # upper bound for mtry determined by tune_grid
  # this takes years to run
  rf_res <- workflow |>
    tune_grid(folds, 
              grid = 10, 
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(roc_auc))
  
  best_res <- rf_res |>
    show_best(10, metric = "roc_auc")
  
  readr::write_csv(best_res,
                   v_path("v3.03", "model", "tune_results.csv"))
  autoplot(rf_res)
  

  # rebuilding, now with variable importance
  final_rf_mod <- rand_forest(mtry = 8, min_n = 38, trees = 500) |>
    set_engine("ranger", importance = "impurity") |>
    set_mode("classification")
  
  final_wkf <- workflow |>
    update_model(final_rf_mod)
  
  last_rf_fit <- final_wkf |>
    last_fit(splits)
  
  last_rf_fit |>
    collect_metrics()
  
  library(vip)
  # must extract fit from workflow, and then use vip package to visualize scores
  last_rf_fit |> 
    extract_fit_parsnip() |>
    vi()
}

# tuning boosted regression tree
if (FALSE) {
  
  tune <- readr::read_csv(v_path("v3.04", "model", "tune_results.csv"))
  
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

# approximate overall auc 
fit_resamples(workflow, folds) |>
  collect_metrics()

######## TUNE THE MODEL ###########
if (FALSE) {
  param <- workflow |>
    extract_parameter_set_dials() |>
    update(epochs = epochs(range = c(10, 25)),
           hidden_units = hidden_units(c(200, 300)),
           dropout = dropout(range = c(.75, .9))) 
  
  roc_res <- metric_set(roc_auc)
  set.seed(22)
  tuned <- workflow |>
    tune_grid(folds, 
              grid = param |> grid_regular(levels = 2),
              metrics = roc_res)
  set.seed(NULL)
  
  autoplot(tuned) +
    theme(legend.position = "top")
  
  show_best(tuned) |> select(-.estimator)
  
  best <- select_best(tuned, metric = "roc_auc")
  model_spec <- finalize_model(model_spec, best)
}

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






