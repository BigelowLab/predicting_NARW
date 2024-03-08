setwd("/mnt/ecocast/projects/students/ojohnson/brickman")
source("create_version.R")
source("v2/v2_setup.R")

wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz"))

# Tune the model
# Compare models with resampling
# Create and plot monthly models 

workflow <- workflow() |>
  add_recipe(recipe) |>
  add_model(model_spec)

# approximate overall auc 
fit_resamples(workflow, folds) |>
  collect_metrics()

######## TUNE THE MODEL ###########
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

######### COMPARE MULTIPLE WORKFLOWS ####

control <- recipe(patch ~ ., data = training) |>
  update_role(lon, lat, new_role = "ID") |>
  step_dummy(all_nominal_predictors())

rMLD <- control |>
  step_log(MLD, base = 10)

rbathy <- rMLD |>
  step_log(Bathy_depth, base = 10)

recipe_list <- list(bathy = rbathy, mld = rMLD, control = control) |>
  map(~step_normalize(.x, all_numeric_predictors()))
model_list <- list(brt = brt_spec)

compare_workflows <- function(recipe_list, model_list) {
  test_workflows <- workflow_set(preproc = recipe_list, 
                                 models = model_list, 
                                 cross = FALSE)
  
  results <- test_workflows |>
    workflow_map("fit_resamples", verbose = TRUE, seed = 120, 
                 resamples = folds)
  
  results
}

r <- compare_workflows(recipe_list, model_list)

r |>
  collect_metrics()

autoplot(r)

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

# lets implement the evaluation strip method 
response_curves <- function(v = "v2.02",
                            method = median) {
  
  wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz"))

  # generates response curves for a given month 
  month_response <- function(mon) {
    # static prediction info needed for model 
    static <- as_tibble(list(month = mon |> as.factor(), 
                             lat = NA, 
                             lon = NA))
    
    # selecting monthly data 
    mon_data <- data |>
      filter(month == mon) |>
      select(Bathy_depth:V)
    
    # acquiring the "middle values" for each variable
    med_df <- mon_data |>
      apply(2, method) |>
      as_tibble_row()
    
    # acquiring the step values for each variable (based on min and max)
    steps <- mon_data |>
      apply(2, function(.x) seq(from = min(.x), 
                                to = max(.x), 
                                length.out = 30)) |>
      as.data.frame()
    
    # helper that generates evaluation strip and runs predictions
    var_response <- function(var) {
      in_df <- bind_cols(static, 
                         steps |> select(all_of(var)), 
                         med_df |> select(-all_of(var)))
      
      augment(wkf, in_df) |>
        select(all_of(var), .pred_1) |>
        rename(value = var) |>
        mutate(variable = var)
    }
    
    # creating plottable object
    eval_strip <- colnames(mon_data) |>
      lapply(var_response) |>
      bind_rows()
    
    # plotting 
    ggplot(data = eval_strip, mapping = aes(x = value, y = .pred_1)) +
      facet_wrap(~ variable, scales = "free") +
      geom_line() +
      theme_bw() +
      ylim(0, 1) +
      labs(x = "Variable value", y = "Patch probability") +
      ggtitle(paste(v, "Response Curves (Training Range)", as_month(mon)))
  }
  
  pdf(v_path(v, "model", "response_curves.pdf"))
  for(mon in 1:12) {
    print(month_response(mon))
  }
  dev.off()
}


# lets implement the evaluation strip method 
response_curves_ann <- function(v = "v2.02",
                                method = median) {
  
  wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz"))
  
  ann_data <- data |>
    select(Bathy_depth:V)
  
  med_df <- ann_data |>
    apply(2, method) |>
    as_tibble_row()
  
  steps <- ann_data |>
    apply(2, function(.x) seq(from = min(.x), 
                              to = max(.x), 
                              length.out = 30)) |>
    as.data.frame()
  
  # generates response curves for a given month 
  month_response <- function(mon) {
    # static prediction info needed for model 
    static <- as_tibble(list(month = mon |> as.factor(), 
                             lat = NA, 
                             lon = NA))
    
    # helper that generates evaluation strip and runs predictions
    var_response <- function(var) {
      in_df <- bind_cols(static, 
                         steps |> select(all_of(var)), 
                         med_df |> select(-all_of(var)))
      
      augment(wkf, in_df) |>
        select(all_of(var), .pred_1) |>
        rename(value = var) |>
        mutate(variable = var)
    }
    
    # creating plottable object
    eval_strip <- colnames(ann_data) |>
      lapply(var_response) |>
      bind_rows()
    
    # plotting 
    ggplot(data = eval_strip, mapping = aes(x = value, y = .pred_1)) +
      facet_wrap(~ variable, scales = "free") +
      geom_line() +
      theme_bw() +
      ylim(0, 1) +
      labs(x = "Variable value", y = "Patch probability") +
      ggtitle(paste(v, "Response Curves (Training Range)", as_month(mon)))
  }
  
  pdf(v_path(v, "model", "response_curves_ann.pdf"))
  for(month in 1:12) {
    print(month_response(month))
  }
  dev.off()
}

### PRESENT PREDICTIONS

bathy <- brickman::read_brickman(scenario="PRESENT", 
                                 vars = "Bathy_depth", 
                                 interval = "ann", 
                                 form = "stars")
add <- brickman::read_brickman(scenario = "PRESENT", 
                               vars = c("SST", "Sbtm", "Tbtm", 
                                        "MLD", "SSS", "U", "V"),
                               interval = "mon", 
                               form = "stars")

# lets implement the evaluation strip method 
response_curves_pred <- function(v = "v2.02",
                                 method = median) {
  
  wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz"))
  
  # generates response curves for a given month 
  month_response <- function(mon) {
    # static prediction info needed for model 
    static <- as_tibble(list(month = mon |> as.factor(), 
                             lat = NA, 
                             lon = NA))
    
    pred_data <- c(add |> dplyr::slice(month, mon),
                   bathy) |>
      as_tibble() |>
      na.omit() |>
      select(SST:Bathy_depth)
    
    # acquiring the "middle values" for each variable
    med_df <- pred_data |>
      apply(2, method) |>
      as_tibble_row()
    
    # acquiring the step values for each variable (based on min and max)
    steps <- pred_data |>
      apply(2, function(.x) seq(from = min(.x), 
                                to = max(.x), 
                                length.out = 50)) |>
      as.data.frame()
    
    # helper that generates evaluation strip and runs predictions
    var_response <- function(var) {
      in_df <- bind_cols(static, 
                         steps |> select(all_of(var)), 
                         med_df |> select(-all_of(var)))
      
      augment(wkf, in_df) |>
        select(all_of(var), .pred_1) |>
        rename(value = var) |>
        mutate(variable = var)
    }
    
    # creating plottable object
    eval_strip <- colnames(pred_data) |>
      lapply(var_response) |>
      bind_rows()
    
    # plotting 
    ggplot(data = eval_strip, mapping = aes(x = value, y = .pred_1)) +
      facet_wrap(~ variable, scales = "free") +
      geom_line() +
      theme_bw() +
      ylim(0, 1) +
      labs(x = "Variable value", y = "Patch probability") +
      ggtitle(paste(v, "Response Curves (Prediction Range)", as_month(mon)))
  }
  
  pdf(v_path(v, "model", "response_curves_pred.pdf"))
  for(mon in 1:12) {
    print(month_response(mon))
  }
  dev.off()
}
response_curves_pred(v)

# lets implement the evaluation strip method 
response_curves_diapause <- function(v = "v2.11.01",
                                     method = median) {
  
  wkfs <- readRDS(v_path(v, "model", "model_fit.csv.gz"))
  
  # generates response curves for a given month 
  month_response <- function(mon) {
    wkf <- (if(mon > 6) {wkfs[[2]]}
            else {wkfs[[1]]})
    
    # static prediction info needed for model 
    static <- as_tibble(list(month = mon |> as.factor(), 
                             lat = NA, 
                             lon = NA))
    
    pred_data <- c(add |> dplyr::slice(month, mon),
                   bathy) |>
      as_tibble() |>
      na.omit() |>
      select(all_of(SST:Bathy_depth))
    
    # acquiring the "middle values" for each variable
    med_df <- pred_data |>
      apply(2, method) |>
      as_tibble_row()
    
    # acquiring the step values for each variable (based on min and max)
    steps <- pred_data |>
      apply(2, function(.x) seq(from = min(.x), 
                                to = max(.x), 
                                length.out = 50)) |>
      as.data.frame()
    
    # helper that generates evaluation strip and runs predictions
    var_response <- function(var) {
      in_df <- bind_cols(static, 
                         steps |> select(all_of(var)), 
                         med_df |> select(-all_of(var)))
      
      augment(wkf, in_df) |>
        select(all_of(var), .pred_1) |>
        rename(value = var) |>
        mutate(variable = var)
    }
    
    # creating plottable object
    eval_strip <- colnames(pred_data) |>
      lapply(var_response) |>
      bind_rows()
    
    # plotting 
    ggplot(data = eval_strip, mapping = aes(x = value, y = .pred_1)) +
      facet_wrap(~ variable, scales = "free") +
      geom_line() +
      theme_bw() +
      ylim(0, 1) +
      labs(x = "Variable value", y = "Patch probability") +
      ggtitle(paste(v, "Response Curves (Prediction Range)", as_month(mon)))
  }
  
  pdf(v_path(v, "model", "response_curves_pred.pdf"))
  for(mon in 1:12) {
    print(month_response(mon))
  }
  dev.off()
}

# code to compare predictive power of azmp vs. ecomon
if (FALSE) {
src_data <- get_ae_vars(b_vars = c("Bathy_depth", "MLD", "Sbtm", "SSS", 
                                   "SST", "Tbtm", "U", "V", "src"),
                        interval = "mon") 

dsplit <- src_data |>
  split(src_data$src)

#1: AZMP, 2: ECOMON

recipe <- recipe(patch ~ ., data = src_data) |>
  update_role(lat, lon, U, V, src, new_role = "ID") |>
  step_mutate(vel = sqrt(U^2 + V^2), role = "predictor") |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

ecomon_preds <- workflow |>
  fit(dsplit[[1]]) |>
  augment(dsplit[[2]])

azmp_preds <- workflow |>
  fit(dsplit[[2]]) |>
  augment(dsplit[[1]])

make_auc_monthly <- function(aug) {
  count(aug, month) |>
    bind_cols(
      auc_mon = split(aug, aug$month) |> 
        lapply(function(x) roc_auc_vec(x$patch,
                                       x$.pred_1,
                                       event_level = "second")) |>
        unlist(),
      src = aug$src |> head(12)) |>
    mutate(month = as.numeric(month))
}

auc_monthly <- list(ecomon_preds, azmp_preds) |>
  lapply(make_auc_monthly) |>
  bind_rows()

aucs <- list(azmp_preds, ecomon_preds) |>
  map(~roc_auc(.x, truth = patch, .pred_1, event_level = "second")) |>
  map(~.x[[3]][[1]]) |>
  map(~round(.x, 4)) |>
  paste(collapse = " ")

pdf("/mnt/ecocast/projectdata/calanusclimate/plots/ecomon_vs_azmp_outofsamplepredictions.pdf")                    
ggplot(data = auc_monthly, 
       mapping = aes(x = month, y = auc_mon)) +
  geom_line(aes(color = src)) +
  geom_point(aes(size = n)) +
  scale_x_continuous(name = "Month", 
                     breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", 
                                "May", "Jun", "Jul", "Aug",
                                "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(name = "AUC", limits = c(.4, 1)) +
  ggtitle(paste0("Ecomon v AZMP - AUC by Month (Overall: ", aucs, ")")) +
  theme_classic() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        text = element_text(size=15)) +
  labs(color = "Testing")
dev.off()

}




