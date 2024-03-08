library(RColorBrewer)
library(viridis)
library(vip)

# Saves a desired model analysis to file
save_analysis <- function(plot, v, name) {
  savepath <- v_path(v, "model", paste0(name, ".pdf"))
  
  pdf(savepath)
  print(plot)
  dev.off()
}

# creates a breakdown of how removing a variable affects AUC and saves to file. 
var_cont <- function(v, 
                     training_data,
                     wkf = NULL,
                     monthly = c(TRUE, FALSE),
                     num_folds = 10, 
                     formula = patch ~ Bathy_depth + MLD + SST + SSS + Tbtm + Sbtm + Vel) {
  # extracting needed elements
  wkf <- get_v_wkf(v, wkf)
  model_list <- wkf |>
    extract_spec_parsnip() |>
    list()
  
  baked_data <- wkf |>
    extract_recipe(estimated = TRUE) |>
    bake(training_data)
  
  formulas <- leave_var_out_formulas(
    formula, 
    data = baked_data
  )
  
  # building workflow set and running models
  var_wkfs <- workflow_set(preproc = formulas, 
                           models = model_list, 
                           cross = FALSE) |>
    mutate(wflow_id = sub("\\_.*", "", wflow_id))
  
  results <- var_wkfs |>
    workflow_map("fit_resamples", 
                 verbose = TRUE, 
                 seed = 120, 
                 control = control_resamples(save_pred = TRUE),
                 resamples = vfold_cv(baked_data, v = num_folds, strata = patch),
                 metrics = metric_set(roc_auc))
  
  # iterating through each desired value for monthly 
  for (m in monthly) {
    
    if (m) {
      preds <- results |>
        collect_predictions() |>
        group_split(wflow_id) |>
        map(~ bind_cols(.x, training_data |> select(month)) |>
              group_by(month) |>
              summarize(wflow_id = wflow_id[[1]], 
                        auc = roc_auc_vec(truth = patch, .pred_0)))
      
      control <- preds[[2]] |> pull(auc)
      
      plottable_aucs <- preds |>
        map(~ mutate(.x, auc = auc - control)) |>
        bind_rows() |>
        mutate(negative = auc < 0) |>
        filter(wflow_id != "everything")
      
      save_name = "var_on_aucs_mon"
      
    } else {
      metrics <- results |>
        collect_metrics(summarize = FALSE) |>
        group_by(wflow_id) |>
        summarize(max = max(.estimate), 
                  min = min(.estimate), 
                  auc = median(.estimate))
      
      control = pull(metrics, auc, wflow_id)[["everything"]]
      
      plottable_aucs <- metrics |>
        filter(wflow_id != "everything") |>
        mutate(across(c(max, auc, min), ~.x - control), 
               negative = auc < 0)
      
      save_name = "var_on_aucs"
    }
    
    plot_obj <- ggplot(plottable_aucs, 
                       aes(x = wflow_id, y = auc, fill = negative)) +
      geom_col() + 
      theme_bw() +
      theme(legend.position = "none") +
      coord_flip() +
      labs(x = "Variable removed", y = "Change in AUC") +
      ggtitle(paste0(v, " effect of removing variables on AUC"))
    
    if (m) {
      plot_obj <- plot_obj +
        facet_wrap(~month, nrow = 4, ncol = 3)
    } else {
      plot_obj <- plot_obj +
        geom_pointrange(aes(ymin = min, ymax = max), size = .35)
    }
    
    save_analysis(plot_obj, v, save_name)
  }
}

# plot false positives/negatives
heatmap_geography <- function(v) {
  results <- get_testing(v) |>
    mutate(heatmap_val = paste0(patch, .pred_class))
  
  # color schema
  status <- list("00" = "True Negative",
                 "01" = "False Positive", 
                 "10" = "False Negative", 
                 "11" = "True Positive")
  # palette for colors 
  pal <- c("00" = "gray90",
           "01" = "red3", 
           "10" = "dodgerblue2", 
           "11" = "goldenrod1")
  
  plot <- ggplot(results, aes(x = lon, y = lat)) +
    geom_point(aes(col = heatmap_val), cex = .3) +
    scale_color_manual(labels = status,
                       values = pal) + 
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group)) +
    coord_quickmap(xlim = c(-77.0, -42.5),
                   ylim = c(36.5,  56.7),
                   expand = TRUE) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom") +
    guides(colour = guide_legend(override.aes = list(size=2))) +
    labs(color = "Status") +
    ggtitle(paste(v, "location of Accurate/Inaccurate Predictions"))
  
  save_analysis(plot, v, "geographic_heatmap")
}

# plot model accuracy vs variable values 
accuracy_vs_variables <- function(v,
                                  position = c("fill", "stack")[1]) {
  results <- get_testing(v) |>
    mutate(heatmap_val = paste0(patch, .pred_class) |>
             factor(levels = c("00", "11", "10", "01")))
  
  # color schema
  status <- list("00" = "True Negative",
                 "11" = "True Positive",
                 "10" = "False Negative",
                 "01" = "False Positive")
  pal <- c("00" = "gray90", 
           "11" = "goldenrod1", 
           "10" = "dodgerblue2",
           "01" = "red3")
  
  #helper
  plot_var <- function(var, title = NULL) {
    if (is.null(title)) {title <- var}
    
    plot <- ggplot(results, aes_string(x = var, 
                               fill = "heatmap_val")) +
      geom_histogram(position = position,
                     bins = 30) +
      geom_vline(aes_string(xintercept = paste0("median(", var, ")"))) +
      theme_bw() +
      scale_fill_manual(labels = status,
                        values = pal) +
      labs(x = title, fill = "Prediction") +
      ggtitle(paste(v, title))
    
    if (position == "fill") {
      plot <- plot +
        scale_y_continuous(name = "Percent of Bin Count", 
                           labels = scales::percent)
    }
    
    plot
  }
  
  names <- colnames(results)
  vars <- names[names %in% mon_vars()]
 
  lapply(vars, plot_var) |>
    append(plot_var("log10(Bathy_depth + 1)", 
                    "Bathymetry (log10(x + 1))") |>
             list()) |>
    save_analysis(v, paste0("vars_heatmaps_", position))
}

# create plots of abundance vs. predicted probability
pred_v_abundance <- function(v) {
  # retrieving data -- predictions merged with abundance 
  ref <- retrieve_vars(v, "abundance")
  
  # performing correlation test
  cortest <- cor.test(ref$abundance, ref$.pred_1, 
                      method = "spearman", exact = FALSE)
  
  # creating plottable objects
  base <- ggplot(ref, aes(x = log10(abundance + 1), y = .pred_1)) +
    theme_bw() +
    labs(y = "Patch Probability", x = "Abundance (log10(x + 1))") +
    ggtitle("Patch probability vs. Abundance") +
    coord_cartesian(expand = TRUE)
  
  density <- base +
    theme(legend.position = "bottom") +
    geom_hex(bins = 45) +
    #stat_density_2d(aes(fill = ..level..), geom = "polygon") +
    #scale_fill_distiller(palette = "Purples", direction = 1) +
    scale_fill_viridis(direction = -1) +
    geom_hline(yintercept = .5) +
    geom_vline(xintercept = 4)
  
  points <- base +
    geom_point(alpha = .3) +
    geom_hline(yintercept = .5, color = "red") +
    geom_vline(xintercept = 4, color = "red")
  
  annotate_help <- function(plot) {
    plot +
      annotate("text", x = 1, y = c(.9, .85, .8), 
               label = c(paste("rho =", round(cortest$estimate, 4)), 
                         paste("p =", cortest$p.value), 
                         paste("n =", nrow(ref))))
  }
  
  list(density, points) |>
    lapply(annotate_help) |>
    save_analysis(v, "predictions_vs_abundance")
}

#' generate response curves for a desired version
#' 
#' @param v the version
#' @param folds, the data folds used to train model variations. If NULL, uses
#'   original workflow.
#' @param num_pts the resolution of the lines 
#' @return response curves separated by month and variable 
response_curves <- function(v,
                            folds = NULL, 
                            num_pts = 50) {

  # retrieving brickman data
  bathy <- brickman::read_brickman(scenario="PRESENT",
                                   vars = "Bathy_depth",
                                   interval = "ann",
                                   form = "stars")
  add <- brickman::read_brickman(scenario = "PRESENT",
                                 vars = c("SST", "Sbtm", "Tbtm",
                                          "MLD", "SSS", "U", "V"),
                                 interval = "mon",
                                 form = "stars")

  # retrieving workflow
  wkf <- get_v_wkf(v)
  if (!is.null(folds)) {
    wkf <- folds |>
      rowwise() |>
      transmute(wkf = fit(wkf, 
                          training(splits)) |>
                  list(),
                fold = id)
  }

  # generates response curves for a given month
  month_response <- function(mon) {
    # static prediction info needed for model
    static <- as_tibble(list(month = mon |> as.factor(),
                             lat = NA,
                             lon = NA))

    # monthly prediction data
    pred_data <- c(dplyr::slice(add, month, mon), bathy) |>
      as_tibble() |>
      na.omit() |>
      select(SST:Bathy_depth)

    # acquiring quantile ranges - max, min, median, 90% range
    quantiles <- pred_data |>
      apply(2, function(x) quantile(x, probs = c(0, .05, .5, .95, 1)))

    range_steps <- map2_dfc(quantiles['0%',], quantiles['100%',], 
                            ~seq(.x, .y, length.out = num_pts))
    
    medians <- quantiles['50%',] |>
      as_tibble_row()
    
    quant_table <- quantiles |>
      t() |>
      as_tibble(rownames = "variable") |>
      select(-all_of(c("0%", "100%")))

    # helper that generates evaluation strip and runs predictions
    var_response <- function(var) {

      in_df <- bind_cols(static,
                         select(range_steps, all_of(var)),
                         select(medians, -all_of(var)))
      
      if (!is.null(folds)) {
        model_preds <- map2(wkf$wkf, wkf$fold,
                            ~ augment(.x, in_df) |>
                              mutate(fold = .y)) |>
          bind_rows()
      } else {
        model_preds <- augment(wkf, in_df) |>
          mutate(fold = "Fold1")
      }
      
      # augment(wkf, in_df)
      model_preds |>
        select(all_of(var), fold, .pred_1) |>
        rename(value = var) |>
        mutate(variable = var)
    }

    # creating plottable object
    eval_strip <- colnames(pred_data) |>
      lapply(var_response) |>
      bind_rows()

    # plotting
    ggplot(quant_table) +
      geom_rect(aes(xmin = `5%`, xmax = `95%`, ymin = -Inf, ymax = Inf),
                fill = "red", alpha = .08) +
      geom_line(data = eval_strip, 
                mapping = aes(x = value, y = .pred_1, col = fold)) +
      scale_color_brewer(palette="Dark2") +
      geom_vline(aes(xintercept = `50%`), color = "red") +
      facet_wrap(~ variable, scales = "free") +
      theme_bw() +
      ylim(0, 1) +
      theme(legend.position = "none") +
      labs(x = "Variable value", y = "Patch probability") +
      ggtitle(paste(v, "Response Curves (Prediction Range)", as_month(mon)))
  }

  plot_list <- 1:12 |>
    lapply(month_response)
  
  filename <- ifelse(is.null(folds), 
                     "response_curves", "response_curves_folded")
  
  save_analysis(plot_list, v, filename)
}

#' generate response curves for a desired version
#' 
#' @param v the version
#' @param folds, the data folds used to train model variations. If NULL, uses
#'   original workflow.
#' @param num_pts the resolution of the lines 
#' @return response curves separated by month and variable 
annual_response_curves <- function(v,
                                   wkf = NULL,
                                   folds = NULL, 
                                   num_pts = 50,
                                   mid_mon = 6,
                                   var_imp = NULL) {
  
  # retrieving brickman data
  bdata <- brickman::read_brickman(scenario = "PRESENT",
                                   vars = c("Bathy_depth", "SST", "Sbtm", "Tbtm",
                                            "MLD", "SSS", "U", "V"),
                                   interval = "ann",
                                   form = "stars") |>
    as_tibble() |>
    na.omit() |>
    mutate(Vel = sqrt(U^2 + V^2)) |>
    select(Bathy_depth:SSS, Vel)
  
  # retrieving workflow
  wkf <- get_v_wkf(v, wkf)
  if (!is.null(folds)) {
    wkf <- folds |>
      rowwise() |>
      transmute(wkf = fit(wkf, 
                          training(splits)) |>
                  list(),
                fold = id)
  }
  
  # acquiring quantile ranges - max, min, median, 90% range
  quantiles <- bdata |>
    apply(2, function(x) quantile(x, probs = c(0, .05, .5, .95, 1)))
  
  range_steps <- map2_dfc(quantiles['0%',], quantiles['100%',], 
                          ~seq(.x, .y, length.out = num_pts))
  
  medians <- quantiles['50%',] |>
    as_tibble_row() |>
    mutate(month = as.factor(mid_mon))
  
  quant_table <- quantiles |>
    t() |>
    as_tibble(rownames = "variable") |>
    select(-all_of(c("0%", "100%"))) |>
    bind_rows(list(variable = "month", `5%` = mid_mon, 
                   `50%` = mid_mon, `95%` = mid_mon))
  
  # helper that generates evaluation strip and runs predictions
  var_response <- function(var) {
    
    base <- bind_cols(lat = NA, 
                      lon = NA, 
                      select(medians, -all_of(var)))
    
    in_df <- (if(var == "month"){
      bind_cols(base, month = 1:12 |> as.factor())
    } else {
      bind_cols(base, select(range_steps, all_of(var)))
    }) |>
      rename(V = Vel) |>
      mutate(U = 0)
    
    model_preds <- if(!is.null(folds)) {
      map2(wkf$wkf, wkf$fold, 
           ~ augment(.x, in_df) |>
             mutate(fold = .y)) |>
        bind_rows()
    } else {
      augment(wkf, in_df) |>
        mutate(fold = "Fold1")
    }
    
    # augment(wkf, in_df)
    model_preds |>
      rename(Vel = V) |>
      select(all_of(var), fold, .pred_1) |>
      rename(value = var) |>
      mutate(value = as.numeric(value),
             variable = var)
  }
  
  # creating plottable object
  eval_strip <- c(colnames(bdata), "month") |>
    lapply(var_response) |>
    bind_rows()
  
  if (!is_null(var_imp)) {
    var_imp <- gsub("month[^ ]*", "month", var_imp) |>
      unique()
    
    quant_table <- quant_table |>
      mutate(across(variable, factor, levels = var_imp))
    
    eval_strip <- eval_strip |>
      mutate(across(variable, factor, levels = var_imp))
  }
  
  # plotting
  plot <- ggplot(quant_table) +
    geom_rect(aes(xmin = `5%`, xmax = `95%`, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = .08) +
    geom_line(data = eval_strip, 
              mapping = aes(x = value, y = .pred_1, col = fold)) +
    scale_color_brewer(palette="Dark2") +
    geom_vline(aes(xintercept = `50%`), color = "red") +
    facet_wrap(~ variable, scales = "free") +
    theme_bw() +
    ylim(0, 1) +
    theme(legend.position = "none") +
    labs(x = "Variable value", y = "Patch probability") +
    ggtitle(paste(v, "Response Curves (Prediction Range)"))
  
  filename <- ifelse(is.null(folds), 
                     "response_curve_ann", "response_curve_ann_folded")
  save_analysis(plot, v, filename)
}

#' Returns variable importance ranking for a version and saves bar chart to file
#' 
#' @param v the version
#' @param wkf optional workflow 
#' @return list of variables from most to least important, NULL if error 
var_imp <- function(v, wkf = NULL) {
  model <- get_v_wkf(v, wkf) |>
    extract_fit_parsnip()
  
  tryCatch({
    vip(model) |>
      save_analysis(v, "var_importance")
    vi(model)$Variable
  }, error = function(e) {
    NULL
  })
}

