source("/mnt/ecocast/projects/students/ojohnson/brickman/setup.R")

#' Generates an unfitted workflow with recipe and model
#' 
#' @param train_data the template data to use when building recipe
#' @return unfitted workflow 
generate_workflow <- function(train_data) {
  
  ### recipe
  # assigning id roles to longitude and latitude, manipulating data 
  calanus_rec <-
    recipe(patch ~ ., 
           data = train_data) |>
    update_role(lon, lat, new_role = "ID") |>
    step_corr(all_numeric_predictors()) |> 
    step_normalize(all_numeric_predictors()) |>
    prep()
  
  rf <- rand_forest(trees = 500, mode="classification", engine = "ranger")
  
  calanus_fit <-
    workflows::workflow() |>
    add_model(rf) |>
    add_recipe(calanus_rec)
  
  calanus_fit
}

##### FOLDS HELPERS ########

#' fits and tests a workflow to supplied folds 
#' 
#' @param folds the folds to be tested
#' @param wkf the workflow to be used 
#' @return a table with folds, fitted model, test predictions, and auc
augment_folds <- function(folds, calanus_wkf) {
  folds |>
    rowwise() |>
    mutate(model = list(fit(calanus_wkf, data=analysis(splits))),
           augmented = list(augment(model, assessment(splits))),
           auc = roc_auc_vec(augmented$patch, augmented$.pred_0)) |>
    ungroup()
}

#' Selects the model/list of models from a list of folds 
#'   to be the final model based on the provided merge method
#' 
#' @param fold_table tbl of folds, models, testing results and auc
#' @param method chr, method of merging folds together
#' @return fitted workflow/list of workflows to be saved
save_results <- function(fold_table, 
                         method = c("max", "mean", "median")[1]) {f
  
  # helper - which value is the median? assuming odd vector length
  which.med <- function(v) {
    which(v == median(v))[1]
  }
  
  # determining which fold(s) will be kept 
  index <- switch(method, 
                  "mean" = TRUE, 
                  "max" = which.max(fold_table$auc),
                  "median" = which.med(fold_table$auc))
  
  # saving selected model(s)
  save_model(fold_table$model[[index]], 
             v)
  # generating analysis for selected model(s)
  analyse(fold_table$augmented[[index]], 
          v, 
          fold_table$auc[[index]])
}

######### SAVING AND RESULTS HELPERS ##############

#' Saves a fitted model to file
#' 
#' @param model wkf or list of wkfs to be saved
#' @param v chr, version 
save_model <- function (model, v) {
  # saving to file
  saveRDS(model, v_path(v, "model", "model_fit.csv.gz"))
  
  model
}

#' Saves heat map and roc curve to file system.
#'
#' @param aug df, augmented data to have accuracy measured
#' @param v chr, the version being saved
#' @param auc dbl, computed overall auc or calculated if NULL
#' @return saves two pdf files to system with heat map and roc curve
analyse <- function(aug, v, auc = NULL) {
  
  # folded data can't be processed
  if (!is.data.frame(aug)) {
    message("cannot save roc curve or heat map at this time")
    return(as.data.frame(auc))
  }
  
  # saving heat map
  pdf(v_path(v, "model", "heat_map.pdf")) 
  print(calanusthreshold::heat_map(conf_mat(aug, patch, .pred_class), 
                                   title= paste0(v, " prediction accuracy")))
  dev.off()
  
  # saving overall roc curve
  if(is.null(auc)) {
    auc <- roc_auc_vec(aug$patch, aug$.pred_0)
  }
  pdf(v_path(v, "model", "roc_curve.pdf")) 
  print(yardstick::roc_curve(aug, patch, .pred_0) |>
          autoplot() + ggtitle(paste0(v, 
                                      " ROC Curve (AUC = ", round(auc, 4), ")")))
  dev.off()
  
  # if monthly, then save a plot of auc by month 
  if (!("month" %in% colnames(aug))) {
    return (auc)
  } else {
    split_aug <- split(aug, aug$month)
    
    # retrieving a table with a count of each month, number of instances 
    #  per month, and auc for each month
    auc_monthly <- count(aug, month) |>
      rowwise() |>
      mutate(month = as.numeric(month), 
             auc_mon = roc_auc_vec(split_aug[[month]]$patch, 
                                   split_aug[[month]]$.pred_0))
    
    pdf(v_path(v, "model", "auc_by_month.pdf"))
    
    # generating by-month auc plot 
    ggplot(data = auc_monthly, 
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
            text = element_text(size=15))
    
    dev.off()
    
    return(auc_monthly)
  }
}


# old code
if (FALSE) {
  #' Retrieves desired training data
  #' 
  #' @param d the version of the training data to be read
  #' @param root the root folder holding observations
  #' @return desired climate data 
  retrieve_data <- function(d = "d1.00", 
                            root = "/mnt/ecocast/projectdata/calanusclimate/obs") {
    readr::read_csv(file.path(root, d, paste0(d, ".csv.gz"))) |>
      mutate(patch=as.factor(patch))
  }
  
  #' Saves folds results to system
  #' 
  #' @param folds tbl of folds, models, testing results and auc
  #' @param v chr, version
  #' @param method chr, method of merging folds together
  #' @return fitted workflow/list of workflows saved 
  save_folds_results <- function(folds, 
                                 v, 
                                 method = c("max", "mean", "median")[1]) {
    
    # which value is the median? assuming odd vector length
    which.med <- function(v) {
      which(v == median(v))[1]
    }
    
    # if merge method for folds is mean
    if (method == "mean") {
      message("cannot currently save results for mean method")
      save_obj <- folds$model
    } else { # max and median methods both process 1 model
      index <- ifelse(method == "max", which.max, which.med)(folds$auc)
      save_obj <- folds$model[[index]]
      # save testing results
      save_testing_results(folds$augmented[[index]], 
                           v, 
                           folds$auc[[index]])
    }
    
    saveRDS(save_obj, file = v_path(v, "model", "model_fit.csv.gz"))
    
    return(save_obj)
  }
  
  #' Saves heat map and roc curve to file system.
  #'
  #' @param augmented_data df, augmented data to have accuracy measured
  #' @param v chr, the version being saved
  #' @param auc dbl, computed auc or calculated if NULL
  #' @return saves two pdf files to system with heat map and roc curve
  save_testing_results <- function(augmented_data, 
                                   v, 
                                   auc = NULL) {
    # saving heat map
    pdf(v_path(v, "model", "heat_map.pdf")) 
    print(calanusthreshold::heat_map(conf_mat(augmented_data, patch, .pred_class), 
                                     title= paste0(v, " prediction accuracy")))
    dev.off()
    
    auc <- roc_auc_vec(augmented_data$patch, augmented_data$.pred_0)
    # saving roc curve
    pdf(v_path(v, "model", "roc_curve.pdf")) 
    print(yardstick::roc_curve(augmented_data, patch, .pred_0) |>
            autoplot() + ggtitle(paste0(v, 
                                        " ROC Curve (AUC = ", round(auc, 4), ")")))
    dev.off()
  }
}


