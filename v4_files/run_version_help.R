source("setup.R")
source("predictions.R")
source("plots.R")
library(ggridges)

### MODEL INITIATION 

#' Creates a fitted workflow
#' 
#' @param training_data df, the data used for the model 
#' @param model_name, chr, name of desired model
get_workflow <- function(training_data, 
                         model_name) {
  # recipe
  recipe_spec <- recipe(patch ~ ., data = training_data) |>
    update_role(lat, lon, U, V, new_role = "ID") |>
    step_log(Bathy_depth, offset = 1, base = 10) |>
    step_mutate(Vel = sqrt(U^2 + V^2), role = "predictor") |>
    step_normalize(all_numeric_predictors())

  if (model_name %in% c("Boosted Regression Tree", 
                        "GAM", "Logistic Regression")) {
    recipe_spec <- recipe_spec |>
      step_dummy(all_nominal_predictors())
  }
  
  # model spec (+ any recipe changes)
  model_file <- list("Random Forest" = "random_forest.R",
       "GAM" = "gam.R", 
       "Boosted Regression Tree" = "boosted_trees.R", 
       "MLP Neural Network" = "mlp.R",
       "Logistic Regression" = "logistic_reg.R")[[model_name]]
  source(file.path("engines", model_file))
  
  workflow <- workflow() |>
    add_recipe(recipe_spec) |>
    add_model(model_spec) |>
    fit(training_data)
}

#' Generates general analyses: heat map, roc curve, and auc by month.
#'
#' @param v chr, the version - NULL if none
#' @param aug df, augmented data to have accuracy measured
#' @return list of three named ggplot objects: roc curve, heatmap, auc by month
initial_analyse <- function(v, aug) {
  
  if (is.null(v)) {v <- "Workflow"}
  
  # retrieving auc 
  auc <- roc_auc_vec(aug$patch, aug$.pred_1, event_level = "second")
  
  # roc curve
  roc_curve <- yardstick::roc_curve(aug, patch, .pred_1, 
                                    event_level = "second") |>
    autoplot() + 
    ggtitle(paste0(v, " ROC Curve (AUC = ", round(auc, 4), ")"))
  
  # heatmap
  heatmap <- calanusthreshold::heat_map(conf_mat(aug, patch, .pred_class), 
                                        title= paste(v, "prediction accuracy"))
  
  # auc by month
  auc_monthly <- count(aug, month) |>
    bind_cols(
      auc_mon = split(aug, aug$month) |> 
        lapply(function(x) roc_auc_vec(x$patch,
                                       x$.pred_1,
                                       event_level = "second")) |>
        unlist()) |>
    mutate(month = as.numeric(month))
  
  lb = .5
  mon_aucs <- ggplot(data = auc_monthly, 
                     mapping = aes(x = month, y = lb, height = auc_mon - lb)) +
    geom_ridgeline(color = "yellowgreen", fill = "yellowgreen", alpha = .5) +
    geom_point(aes(y = auc_mon)) +
    scale_x_continuous(name = "Month", 
                       breaks = 1:12, 
                       labels = c("Jan", "Feb", "Mar", "Apr", 
                                  "May", "Jun", "Jul", "Aug",
                                  "Sep", "Oct", "Nov", "Dec")) +
    scale_y_continuous(name = "AUC", limits = c(lb, 1)) +
    ggtitle(paste(v, "AUC by Month (Overall:", round(auc, 4), ")")) +
    theme_classic() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_line(),
          text = element_text(size=15))
  
  list(roc_curve = roc_curve, 
       heatmap = heatmap, 
       mon_aucs = mon_aucs, 
       patch_plot = plot_ae(aug, "patch", 
                            paste(v, "patch values")))
}

#' Creates a workflow version 
#' 
#' @param data_split rsplit, the data split object
#' @param model_types list, configuration for model
#' @param v chr, version -- NULL if none 
#' @param overwrite boolean, overwriting existing version? NULL if no version
#' @return a fitted workflow. If version exists, version, analysis plots, and 
#'   testing results are saved to file. 
wkf_version <- function(data_split, 
                        model_types, 
                        v = NULL, 
                        overwrite = FALSE) {
  
  #creating workflow
  fit_wkf <- get_workflow(training(data_split), 
                          model_types[[1]]$name)
  
  # creating analysis plots
  aug <- augment(fit_wkf, testing(data_split))
  
  plot_list <- initial_analyse(v, aug)
  
  # are we saving this model to file?
  if (!is.null(v)) {
    # initializing path
    path <- v_path(v, "model")
    if (dir.exists(path)) {
      if (!overwrite) {stop("Version already exists")}
    } else {
      dir.create(path, recursive = TRUE)
    }
    wd <- getwd()
    setwd(path)
    
    # saving objects
    fit_wkf |>
      saveRDS("model_fit.csv.gz")
    aug |>
      readr::write_csv("testing_results.csv.gz")
    
    augment_plot_helper <- function(plot, name) {
      pdf(paste0(name, ".pdf"))
      print(plot)
      dev.off()
    }
    plot_list |>
      purrr::imap(augment_plot_helper)
    
    setwd(wd)
  } else {
    plot_list
  }
  
  fit_wkf
}

### PLOTS PREDICTIONS

#' Generates predictions and plots for the desired model version
#' 
#' @param v chr, the model version to run
#' @param save_months numeric vector of months
#' @return saves predictions and plots to file system and returns TRUE 
#'   if code ran without erroring
plots_predictions <- function(v = "v3.00", 
                              downsample = c(0, 1, 2, 3)[1],
                              save_months = 1:12,
                              scenarios = 1:5,
                              verbose = TRUE, 
                              comparison_scenario = 5) {
  #from predictions.R
  get_predictions(v, 
                  verbose = verbose, 
                  save_months = save_months,
                  save_scenarios = scenarios,
                  downsample = downsample)
  #from plots.R
  get_plots(v,
            plot_scenarios = scenarios,
            comparison_scenario = comparison_scenario,
            downsample = downsample)
}

