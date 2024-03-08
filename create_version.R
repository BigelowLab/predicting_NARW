source("setup.R")
source("predictions.R")
source("plots.R")

#' Generates and saves the final workflow for a version
#' 
#' @param v chr, the version to be saved
#' @param data_split rsplit, the train/test split
#' @param unfitted_workflow wkf, the final unfitted workflow to be saved
#' @param overwrite boolean, can we overwrite an existing version?
#' @return fitted workflow 
create_version <- function(v,
                           data_split, 
                           unfitted_workflow,
                           overwrite = FALSE) {
  original_dir <- getwd()
  path <- v_path(v, "model")
  
  if (dir.exists(path) && !overwrite) {stop("version already exists")}
  dir.create(path, recursive = TRUE)
  setwd(path)
  
  # retrieving fitted workflow and predictions
  fit <- fit(unfitted_workflow, training(data_split))
  
  # saving fitted workflow object to file 
  saveRDS(fit,
          file.path(path, "model_fit.csv.gz"))
  
  aug <- augment(fit, testing(data_split))
  
  # saving analysis and returning monthly auc results
  analyse(v, aug)
  setwd(original_dir)
  
  fit
}


#' Generates and saves the final workflow list for a version
#' 
#' @param v chr, the version to be saved
#' @param data df, the data to be used
#' @param unfitted_workflow wkf, an unfitted workflow
#' @param overwrite boolean, can we overwrite an existing version?
#' @param save boolean, are we saving these results to file?
#' @return table of auc by month, either saves plot results to file or returns
#'   to screen
create_separate_models <- function(v, 
                                   data, 
                                   unfitted_workflow,
                                   overwrite = FALSE) {
  path <- v_path(v, "model")
  
  if (dir.exists(path) & !overwrite) {stop("version already exists")}
  dir.create(path, recursive = TRUE)
  setwd(path)
  
  # generating fitted workflows and results
  set.seed(405)
  months <- split(data, data$month) |>
    lapply(initial_split, strata = patch) |>
    lapply(last_fit, object = unfitted_workflow)
  set.seed(NULL)
  
  # pruning and transforming results 
  results_table <- months |> 
    lapply(collect_metrics) |>
    bind_rows() |>
    filter(.metric == "roc_auc") |>
    bind_cols(months |> bind_rows(), month = 1:12) |>
    select(month, .estimate) |>
    rename(auc_mon = .estimate)
  
  # saving workflow list
  months |>
    lapply(extract_workflow) |>
    saveRDS("model_fit.csv.gz")
  
  # creating overall aug table
  aug <- months |>
    lapply(collect_predictions) |>
    bind_rows()
  
  # saving final analysis
  analyse(v, aug, results_table)
}

diapause_split <- function(v, 
                           data, 
                           unfitted_workflow,
                           overwrite = FALSE) {
  path <- v_path(v, "model")
  
  if (dir.exists(path) & !overwrite) {stop("version already exists")}
  dir.create(path, recursive = TRUE)
  setwd(path)
  
  # generating fitted workflows and results
  set.seed(405)
  dsplit <- split(data, data$month |> as.numeric() > 6) |>
    lapply(initial_split, strata = patch) |>
    lapply(last_fit, object = unfitted_workflow)
  set.seed(NULL)
  
  # saving workflow list
  dsplit |>
    lapply(extract_workflow) |>
    saveRDS("model_fit.csv.gz")
  
  # creating overall aug table
  months <- dsplit |>
    map(~.x$splits[[1]] |> 
          testing() |>
          select(month))
  aug <- dsplit |>
    lapply(collect_predictions) |>
    map2(months, ~bind_cols(.x, .y)) |>
    bind_rows()
  
  # saving final analysis
  analyse(v, aug)
}

### PLOTS PREDICTIONS

#' Generates predictions and plots for the desired model version
#' 
#' @param v chr, the model version to run
#' @param save_months numeric vector of months
#' @return saves predictions and plots to file system and returns TRUE 
#'   if code ran without erroring
plots_predictions <- function(v = "v2.00", 
                              downsample = c(0, 1)[1],
                              save_months = 1:12,
                              scenarios = 1:5,
                              verbose = TRUE) {
  #from predictions.R
  get_predictions(v, 
                  verbose = verbose, 
                  save_months = save_months,
                  save_scenarios = scenarios,
                  downsample = downsample)
  #from plots.R
  get_plots(v,
            plot_scenarios = scenarios,
            downsample = downsample)
}

### ANALYSIS METHODS
plot_version_var_contribution <- function(v = "v2.01") {
  wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz")) 
  model_list <- wkf |>
    extract_spec_parsnip() |>
    list()
  
  recipe_list <- list(control = recipe, 
                      bathymetry = recipe |>
                        update_role(Bathy_depth, new_role = "ID"), 
                      MLD = recipe |>
                        update_role(MLD, new_role = "ID"), 
                      SSS = recipe |>
                        update_role(SSS, new_role = "ID"),
                      SST = recipe |>
                        update_role(SST, new_role = "ID"),
                      Tbtm = recipe |>
                        update_role(Tbtm, new_role = "ID"), 
                      Sbtm = recipe |>
                        update_role(Sbtm, new_role = "ID"), 
                      U = recipe |>
                        update_role(U, new_role = "ID"), 
                      V = recipe |>
                        update_role(V, new_role = "ID"))
  
  var_wkfs <- workflow_set(preproc = recipe_list, 
                           models = model_list, 
                           cross = FALSE)
  
  results <- var_wkfs |>
    workflow_map("fit_resamples", 
                 verbose = TRUE, 
                 seed = 120, 
                 resamples = folds,
                 metrics = metric_set(roc_auc)) |>
    collect_metrics()
  
  control <- results$mean[[1]]
  
  results_new <- results |>
    dplyr::slice(-1) |>
    rowwise() |>
    transmute(var = strsplit(wflow_id, '_', fixed=TRUE)[[1]][[1]],
              auc_dif = mean - control,
              negative = auc_dif < 0) |>
    ungroup()
  
  pdf(v_path(v, "model", "var_on_auc.pdf"))
  (ggplot(results_new, aes(x = var, y = auc_dif)) +
      geom_col(aes(fill = negative)) + 
      theme(legend.position = "none") +
      coord_flip() +
      labs(x = "Variable removed", y = "Change in AUC") +
      ggtitle(paste0(v, " effect of removing variables on AUC (AUC: ", 
                     round(control, 4), ")"))) |>
    print()
  dev.off()
}
plot_version_var_contribution_monthly <- function(v = "v2.02") {
  wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz")) 
  model_list <- wkf |>
    extract_spec_parsnip() |>
    list()
  
  control <- control_resamples(save_pred = TRUE)
  
  recipe_list <- list(original = recipe, 
                      bathymetry = recipe |>
                        update_role(Bathy_depth, new_role = "ID"), 
                      MLD = recipe |>
                        update_role(MLD, new_role = "ID"), 
                      SSS = recipe |>
                        update_role(SSS, new_role = "ID"),
                      SST = recipe |>
                        update_role(SST, new_role = "ID"),
                      Tbtm = recipe |>
                        update_role(Tbtm, new_role = "ID"), 
                      Sbtm = recipe |>
                        update_role(Sbtm, new_role = "ID"), 
                      U = recipe |>
                        update_role(U, new_role = "ID"), 
                      V = recipe |>
                        update_role(V, new_role = "ID"))
  
  var_wkfs <- workflow_set(preproc = recipe_list, 
                           models = model_list, 
                           cross = FALSE)
  
  # getting raw results from resampling
  results <- var_wkfs |>
    workflow_map("fit_resamples", 
                 verbose = TRUE, 
                 seed = 120, 
                 control = control, 
                 resamples = folds,
                 metrics = NULL)
  
  # extracting predictions and binding to month information
  new_results <- results$result |>
    map(~transmute(.x |> rowwise(), 
                   fold_preds = bind_cols(.predictions, 
                                          splits |>
                                            testing() |>
                                            select(month)) |>
                     list()))
  
  # acquiring monthly AUCS per fold, averaging together, and assembling 
  # into graphable output
  aucs <- new_results |>
    lapply(function(z) {z[["fold_preds"]] |>
        lapply(function(y) {group_split(y, month) |>
            map(~roc_auc(.x, truth = patch, 
                         .pred_1, event_level = "second")) |>
            bind_rows() |>
            select(.estimate)}) |>
        bind_cols(.name_repair = ~ vctrs::vec_as_names(..., quiet = TRUE)) |>
        rowMeans()}) |>
    as.data.frame(col.names = sub("_.*", "", results$wflow_id)) |>
    transmute(across(!c("original"), ~.x - original), 
              month = mon_names) |>
    pivot_longer(cols = !month, names_to = "variable", values_to = "AUC") |>
    mutate(negative = AUC < 0)
  
  # graphing output
  pdf(v_path(v, "model", "var_on_auc_mon.pdf"))
  (ggplot(aucs, aes(x = variable, y = AUC)) +
      facet_wrap(~ factor(month, level= mon_names), ncol = 3, nrow = 4) +
      geom_col(aes(fill = negative)) + 
      theme(legend.position = "none") +
      coord_flip() +
      labs(x = "Variable removed", y = "Change in AUC") +
      ggtitle(paste0(v, " effect of removing variables on AUC"))) |>
    print()
  dev.off()
  
  # in progress code
  if(FALSE) {
    testing <- new_results |>
      map(~transmute(.x, 
                     aucs = fold_preds |>
                       group_split(month) |>
                       map_dfr(~roc_auc(.x, truth = patch, 
                                        .pred_1, event_level = "second") |>
                                 select(.estimate)) |>
                       list())) 
  }
}

vel_varcont <- function(v = "v2.11") {
  wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz")) 
  model_list <- wkf |>
    extract_spec_parsnip() |>
    list()
  
  recipe_list <- list(original = recipe, 
                      bathymetry = recipe |>
                        update_role(Bathy_depth, new_role = "ID"), 
                      MLD = recipe |>
                        update_role(MLD, new_role = "ID"), 
                      SSS = recipe |>
                        update_role(SSS, new_role = "ID"),
                      SST = recipe |>
                        update_role(SST, new_role = "ID"),
                      Tbtm = recipe |>
                        update_role(Tbtm, new_role = "ID"), 
                      Sbtm = recipe |>
                        update_role(Sbtm, new_role = "ID"), 
                      vel = recipe(patch ~ ., data = training) |>
                        update_role(lat, lon, U, V, new_role = "ID") |>
                        step_normalize(all_numeric_predictors()) |>
                        step_dummy(all_nominal_predictors()))
  
  var_wkfs <- workflow_set(preproc = recipe_list, 
                           models = model_list, 
                           cross = FALSE)
  
  results <- var_wkfs |>
    workflow_map("fit_resamples", 
                 verbose = TRUE, 
                 seed = 120, 
                 resamples = folds,
                 metrics = metric_set(roc_auc)) |>
    collect_metrics()
  
  control <- results$mean[[1]]
  
  results_new <- results |>
    dplyr::slice(-1) |>
    rowwise() |>
    transmute(var = strsplit(wflow_id, '_', fixed=TRUE)[[1]][[1]],
              auc_dif = mean - control,
              negative = auc_dif < 0) |>
    ungroup()
  
  pdf(v_path(v, "model", "var_on_auc.pdf"))
  (ggplot(results_new, aes(x = var, y = auc_dif)) +
      geom_col(aes(fill = negative)) + 
      theme(legend.position = "none") +
      coord_flip() +
      labs(x = "Variable removed", y = "Change in AUC") +
      ggtitle(paste0(v, " effect of removing variables on AUC (AUC: ", 
                     round(control, 4), ")"))) |>
    print()
  dev.off()
}
var_contribution_vel_mon <- function(v = "v2.11") {
  wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz")) 
  model_list <- wkf |>
    extract_spec_parsnip() |>
    list()
  
  control <- control_resamples(save_pred = TRUE)
  
  recipe_list <- list(original = recipe, 
                      bathymetry = recipe |>
                        update_role(Bathy_depth, new_role = "ID"), 
                      MLD = recipe |>
                        update_role(MLD, new_role = "ID"), 
                      SSS = recipe |>
                        update_role(SSS, new_role = "ID"),
                      SST = recipe |>
                        update_role(SST, new_role = "ID"),
                      Tbtm = recipe |>
                        update_role(Tbtm, new_role = "ID"), 
                      Sbtm = recipe |>
                        update_role(Sbtm, new_role = "ID"), 
                      vel = recipe(patch ~ ., data = training) |>
                        update_role(lat, lon, U, V, new_role = "ID") |>
                        step_normalize(all_numeric_predictors()) |>
                        step_dummy(all_nominal_predictors()))
  
  var_wkfs <- workflow_set(preproc = recipe_list, 
                           models = model_list, 
                           cross = FALSE)
  
  # getting raw results from resampling
  results <- var_wkfs |>
    workflow_map("fit_resamples", 
                 verbose = TRUE, 
                 seed = 120, 
                 control = control, 
                 resamples = folds,
                 metrics = NULL)
  
  # extracting predictions and binding to month information
  new_results <- results$result |>
    map(~transmute(.x |> rowwise(), 
                   fold_preds = bind_cols(.predictions, 
                                          splits |>
                                            testing() |>
                                            select(month)) |>
                     list()))
  
  # acquiring monthly AUCS per fold, averaging together, and assembling 
  # into graphable output
  aucs <- new_results |>
    lapply(function(z) {z[["fold_preds"]] |>
        lapply(function(y) {group_split(y, month) |>
            map(~roc_auc(.x, truth = patch, 
                         .pred_1, event_level = "second")) |>
            bind_rows() |>
            select(.estimate)}) |>
        bind_cols(.name_repair = ~ vctrs::vec_as_names(..., quiet = TRUE)) |>
        rowMeans()}) |>
    as.data.frame(col.names = sub("_.*", "", results$wflow_id)) |>
    transmute(across(!c("original"), ~.x - original), 
              month = mon_names) |>
    pivot_longer(cols = !month, names_to = "variable", values_to = "AUC") |>
    mutate(negative = AUC < 0)
  
  # graphing output
  pdf(v_path(v, "model", "var_on_auc_mon.pdf"))
  (ggplot(aucs, aes(x = variable, y = AUC)) +
      facet_wrap(~ factor(month, level= mon_names), ncol = 4, nrow = 3) +
      geom_col(aes(fill = negative)) + 
      theme(legend.position = "none") +
      coord_flip() +
      labs(x = "Variable removed", y = "Change in AUC") +
      ggtitle(paste0(v, " effect of removing variables on AUC"))) |>
    print()
  dev.off()
}

# finds variable contribution for diapause splits - could fairly easily
# be adapted to by-month models
sep_models_vel_mon <- function(v = "v2.11.01") {
  
  control <- control_resamples(save_pred = TRUE)
  
  recipe_list <- list(original = recipe, 
                      bathymetry = recipe |>
                        update_role(Bathy_depth, new_role = "ID"), 
                      MLD = recipe |>
                        update_role(MLD, new_role = "ID"), 
                      SSS = recipe |>
                        update_role(SSS, new_role = "ID"),
                      SST = recipe |>
                        update_role(SST, new_role = "ID"),
                      Tbtm = recipe |>
                        update_role(Tbtm, new_role = "ID"), 
                      Sbtm = recipe |>
                        update_role(Sbtm, new_role = "ID"), 
                      vel = recipe(patch ~ ., data = training) |>
                        update_role(lat, lon, U, V, new_role = "ID") |>
                        step_normalize(all_numeric_predictors()) |>
                        step_dummy(all_nominal_predictors()))
  
  wkfs <- readRDS(v_path(v, "model", "model_fit.csv.gz"))
  # datasets <- data |>
  #   split(data$month |> as.numeric() > 6)
  # month_vecs <- list(1:6, 7:12)
  datasets <- data |>
    split(data$month)
  month_vecs <- 1:12
  
  # helper for each submodel
  run_submodel <- function(wkf, dataset, month_vec) {
    model_list <- wkf |>
      extract_spec_parsnip() |>
      list()
    
    var_wkfs <- workflow_set(preproc = recipe_list, 
                             models = model_list, 
                             cross = FALSE)
    
    set.seed(405)
    folds <- dataset |>
      initial_split(strata = patch) |>
      training() |>
      vfold_cv(v = 10, strata = patch, repeats = 1)
    set.seed(NULL)
  
    # getting raw results from resampling
    results <- var_wkfs |>
      workflow_map("fit_resamples", 
                   verbose = TRUE, 
                   seed = 120, 
                   control = control, 
                   resamples = folds,
                   metrics = NULL)
  
    # extracting predictions and binding to month information
    new_results <- results$result |>
      map(~transmute(.x |> rowwise(), 
                     fold_preds = bind_cols(.predictions, 
                                            splits |>
                                              testing() |>
                                              select(month)) |>
                       list()))
  
    # acquiring monthly AUCS per fold, averaging together, and assembling 
    # into graphable output
    new_results |>
      lapply(function(z) {z[["fold_preds"]] |>
          lapply(function(y) {group_split(y, month) |>
              map(~roc_auc(.x, truth = patch, 
                           .pred_1, event_level = "second")) |>
              bind_rows() |>
              select(.estimate)}) |>
          bind_cols(.name_repair = ~ vctrs::vec_as_names(..., quiet = TRUE)) |>
          rowMeans()}) |>
      as.data.frame(col.names = sub("_.*", "", results$wflow_id)) |>
      transmute(across(!c("original"), ~.x - original),
                month = mon_names[month_vec]) |>
      pivot_longer(cols = !month, names_to = "variable", values_to = "AUC") |>
      mutate(negative = AUC < 0)
  }
  
  aucs <- pmap(list(wkfs, datasets, month_vecs), run_submodel) |>
    bind_rows()
  
  # graphing output
  pdf(v_path(v, "model", "var_on_auc_mon.pdf"))
  (ggplot(aucs, aes(x = variable, y = AUC)) +
      facet_wrap(~ factor(month, level= mon_names), ncol = 3, nrow = 4) +
      geom_col(aes(fill = negative)) + 
      theme(legend.position = "none") +
      coord_flip() +
      labs(x = "Variable removed", y = "Change in AUC") +
      ggtitle(paste0(v, " effect of removing variables on AUC"))) |>
    print()
  dev.off()
}

#' creates heat map, roc curve, and auc by month.
#'
#' @param v chr, the version being saved
#' @param aug df, augmented data to have accuracy measured
#' @param auc_monthly df, optional table of months and auc values
#' @return saves three pdf files to system with heat map, overall roc curve,
#'   and auc by month
analyse <- function(v, aug, auc_monthly = NULL) {
  
  # boolean for whether we are processing monthly data 
  monthly <- "month" %in% colnames(aug) || !is.null(auc_monthly)
  
  # retrieving auc 
  auc <- roc_auc_vec(aug$patch, aug$.pred_1, event_level = "second")
  
  pdf("roc_curve.pdf")
  (yardstick::roc_curve(aug, patch, .pred_1, event_level = "second") |>
      autoplot() + 
      ggtitle(paste0(v, " ROC Curve (AUC = ", round(auc, 4), ")"))) |>
    print()
  dev.off()
  
  pdf("heatmap.pdf")
  calanusthreshold::heat_map(conf_mat(aug, patch, .pred_class), 
                             title= paste0(v, " prediction accuracy")) |>
    print()
  dev.off()
  
  if (monthly) {
    #defining auc monthly
    if (is.null(auc_monthly)) {
      auc_monthly <- count(aug, month) |>
        bind_cols(
          auc_mon = split(aug, aug$month) |> 
            lapply(function(x) roc_auc_vec(x$patch,
                                           x$.pred_1,
                                           event_level = "second")) |>
            unlist()) |>
        mutate(month = as.numeric(month))
    }
    
    with_total <- auc_monthly |> 
      bind_rows(c(month = NULL, auc_mon = auc))
    
    # creating plot object
    monthly_plot <- ggplot(data = auc_monthly, 
                           mapping = aes(x = month, y = auc_mon)) +
      geom_line(color = "yellowgreen") +
      geom_point() +
      scale_x_continuous(name = "Month", 
                         breaks = 1:12, 
                         labels = c("Jan", "Feb", "Mar", "Apr", 
                                    "May", "Jun", "Jul", "Aug",
                                    "Sep", "Oct", "Nov", "Dec")) +
      scale_y_continuous(name = "AUC", limits = c(.7, 1)) +
      ggtitle(paste(v, "AUC by Month (Overall:", round(auc, 4), ")")) +
      theme_classic() + 
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(),
            panel.grid.minor.y = element_line(),
            text = element_text(size=15))
    
    pdf("mon_aucs.pdf")
    print(monthly_plot)
    plot.new()
    grid.table(auc_monthly, theme = ttheme_minimal())
    dev.off()
  }
  
  # return auc 
  if(monthly){with_total}else{auc}
}

