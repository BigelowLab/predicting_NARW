# /mnt/ecocast/projects/students/ojohnson/brickman/analyse.R
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

# Calculates AUC by month 
roc_curves_w_ci <- function(v, save = TRUE) {
  
  test_res <- get_testing(v)
  
  overall <- test_res |>
    group_by(wkf_id) |>
    summarize(auc = roc_auc_vec(patch, .pred_1, event_level = "second"), 
              .groups = 'keep') |>
    ungroup()
  
  # by standard deviation
  # overall_stats <- lm(auc ~ 1, overall) |>
  #   confint()
  # overall_stats
  #
  # res <- wkfsd |>
  #   group_by(month, id) |>
  #   summarize(auc = roc_auc_vec(patch, .pred_1, event_level = "second"), 
  #             .groups = 'keep') |>
  #   na.omit() |>
  #   group_by(month) |>
  #   summarize(confint = lm(auc~1, list(auc)) |> confint()) |>
  #   transmute(month, lower = confint[,1], upper = confint[,2])
  
  # by quantile
  res <- test_res |>
    group_by(month, wkf_id) |>
    summarize(auc = roc_auc_vec(patch, .pred_1, event_level = "second"), 
              .groups = 'keep') |>
    na.omit() |>
    group_by(month) |>
    summarize(lower = quantile(auc, .025), 
              mean = quantile(auc, .5), 
              upper = quantile(auc, .975),
              .groups = 'keep')
  
  res <- left_join(data.frame(month = 1:12 |> factor()), 
                   res,
                   join_by(month)) |>
    as_tibble()
  
  title <- paste(v, "AUC (2.5%, 50%, 97.5%):", 
                 overall$auc |> quantile(c(.025, .5, .975)) |> 
                   round(3) |> paste(collapse = ", "))
  
  plot <- ggplot(res, aes(x = month)) + 
    geom_ribbon(aes(ymax = upper, ymin = lower), 
                fill = "yellowgreen", alpha = .5, group = 1) + 
    geom_line(aes(y = mean), group = 1) +
    geom_point(aes(y = mean), size = .75) +
    coord_cartesian(ylim = c(ifelse(save, 0, .5), 1)) +
    theme_bw() +
    labs(y = "AUC", x = "Month") + 
    ggtitle(title)
  
  if (save) {
    save_analysis(plot, v, "auc_by_month")
  } else {
    plot
  }
}

# Calculates TSS by month 
tss_by_month <- function(v) {
  
  test_res <- get_testing(v)
  
  overall <- test_res |>
    group_by(wkf_id) |>
    summarize(tss = sens_vec(patch, .pred_class, event_level = "second") +
                    spec_vec(patch, .pred_class, event_level = "second") - 1, 
              .groups = 'keep') |>
    ungroup()
  
  # by quantile
  res <- test_res |>
    group_by(month, wkf_id) |>
    summarize(tss = sens_vec(patch, .pred_class, event_level = "second") +
                spec_vec(patch, .pred_class, event_level = "second") - 1, 
              .groups = 'keep') |>
    na.omit() |>
    group_by(month) |>
    summarize(lower = quantile(tss, .025), 
              mean = quantile(tss, .5), 
              upper = quantile(tss, .975),
              .groups = 'keep')
  
  title <- paste(v, "TSS (2.5%, 50%, 97.5%):", 
                 overall$tss |> quantile(c(.025, .5, .975)) |> 
                   round(3) |> paste(collapse = ", "))
  
  plot <- ggplot() + 
    geom_ribbon(data = res, 
                aes(x = month, ymax = upper, ymin = lower), 
                fill = "lightblue", alpha = .5, group = 1) + 
    geom_line(data = res, aes(x = month, y = mean), group = 1) +
    coord_cartesian(ylim = c(.0, 1)) +
    theme_bw() +
    labs(y = "True Skill Statistic", x = "Month") + 
    ggtitle(title)
  
  save_analysis(plot, v, "tss_by_month")
}

#' Returns variable importance ranking for a version and saves bar chart to file
#' 
#' @param v the version
#' @return list of variables from most to least important, NULL if error 
var_imp <- function(v, plot = FALSE) {
  vimps <- get_v_wkfs(v) |>
    map(extract_fit_parsnip) |>
    map(vi_model)
  
  vimp <- vimps |> 
    bind_rows() |>
    group_by(Variable) |>
    summarize(Importance = mean(Importance), .groups = "keep") |>
    ungroup()
  
  month <- vimp |>
    filter(startsWith(Variable, "month")) |>
    summarize(Variable = 'month', 
              Importance = sum(Importance), .groups = 'keep')
  
  agg_vimp <- vimp |>
    filter(!startsWith(Variable, "month")) |>
    arrange(desc(Importance))
  agg_vimp <- bind_rows(agg_vimp, month) |>
    mutate(across(Variable, ~var_abb()[Variable] |> unlist())) |>
    mutate(across(Variable, ~factor(.x, levels = Variable)))
  
  if (plot) {
    p <- ggplot(agg_vimp, aes(x = Importance, y = Variable)) +
      theme(axis.title.y = element_blank()) +
      theme_bw() +
      theme(axis.title.y = element_blank()) +
      geom_bar(stat = 'identity', fill = "dodgerblue4")
    
    save_analysis(p, v, "var_importance")
  }
  
  agg_vimp
  
  # tryCatch({
  #   vip(model) |>
  #     save_analysis(v, "var_importance")
  #   vi(model)$Variable
  # }, error = function(e) {
  #   NULL
  # })
}

#' generate response curves for a desired version
#' 
#' @param v the version
#' @param data the data used to determine ranges for model
#' @param vars the vars included
#' @param num_pts the resolution of the lines 
#' @param mid_mon the middle month
#' @param log_bathy convert bathymetry to log scale?
#' @param vimp the list of most important variables
#' @param post post-prediction correction function, if applicable
#' @return response curves separated by month and variable 
response_curves_data <- function(v,
                                 data,
                                 vars = c("Bathy_depth", "SST", #"SSS", 
                                          "Tbtm", "MLD", "Sbtm", "Vel"),
                                 num_pts = 100,
                                 mid_mon = 8,
                                 log_bathy = TRUE,
                                 show_no_post = TRUE,
                                 same_y = FALSE,
                                 save_plot = TRUE,
                                 patch_only_medians = FALSE,
                                 bottom_latitude = NULL,
                                 vimp = NULL,
                                 post = NULL) {
  
  vdata <- data |>
    mutate(Vel = sqrt(U^2 + V^2))
  
  # retrieving workflow
  wkfs <- get_v_wkfs(v)
  
  # acquiring quantile ranges of covariates - max, min, median, 95% range
  quantiles <- vdata |>
    select(all_of(vars)) |>
    apply(2, function(x) quantile(x, probs = c(0, .025, .5, .975, 1)))
  
  # COMMENT OUT IF NOT REPLACING TBTM
  # quantiles[3, 3] <- 4.5
  
  # generating steps for each covariate between max and min
  range_steps <- map2_dfc(quantiles['0%',], quantiles['100%',], 
                          ~seq(.x, .y, length.out = num_pts))
  
  # Replacing medians as needed
  if (patch_only_medians) {
    quantiles['50%',] <- vdata |> 
      filter(patch == "1") |>
      select(all_of(vars)) |>
      apply(2, function(x) quantile(x, probs = c(.5)))
  } else if (!is_null(bottom_latitude)) {
    quantiles['50%',] <- vdata |>
      filter(lat > 42) |>
      select(all_of(vars)) |>
      apply(2, function(x) quantile(x, probs = c(.5)))
  }
  
  medians <- quantiles['50%',] |>
    as_tibble_row() |>
    mutate(month = as.factor(mid_mon))
  
  # Table summarizing median, 95% range for all covariates
  if (log_bathy) {
    quantiles[,"Bathy_depth"] <- log10(quantiles[,"Bathy_depth"] + 1)
  }
  quant_table <- quantiles |>
    base::t() |>
    as_tibble(rownames = "variable") |>
    select(-all_of(c("0%", "100%"))) |>
    bind_rows(list(variable = "month", `2.5%` = mid_mon, 
                   `50%` = mid_mon, `97.5%` = mid_mon))
  
  # helper that generates evaluation strip and runs predictions
  var_response <- function(var, post_func = post) {
    
    base <- bind_cols(lat = NA, 
                      lon = NA, 
                      select(medians, -all_of(var)))
    
    # input dataframe for workflows
    in_df <- (if(var == "month"){
      bind_cols(base, month = 1:12 |> as.factor())
    } else {
      bind_cols(base, select(range_steps, all_of(var)))
    }) |>
      rename(V = Vel) |>
      mutate(U = 0)
    
    # Predicting, binding value
    model_preds <- apply_quantile_preds(wkfs, in_df, c(.025, .5, .975)) |>
      bind_cols(value = pull(in_df, ifelse(var == "Vel", "V", var)) |>
                  as.numeric()) |>
      mutate(variable = var)
    
    if(log_bathy && var == "Bathy_depth") {
      model_preds <- model_preds |>
        mutate(value = log10(value + 1))
    }
    if(show_no_post && !is.null(post)) { #&& var == "Bathy_depth"
      model_preds <- model_preds |>
        mutate(no_post = `50%`)
    }
    
    if (!is.null(post_func)) {
      bathymetry_col <- (if(var == "Bathy_depth") {
        range_steps$Bathy_depth
      } else {
        medians[["Bathy_depth"]]
      })
      
      model_preds <- model_preds |>
        mutate(bathymetry_col = bathymetry_col) |>
        mutate(across(ends_with("%"), 
                      function(x) {x * post_func(bathymetry_col)})) |>
        select(-bathymetry_col)
    }
    
    model_preds
  }
  
  # creating plottable object
  eval_strip <- c(colnames(quantiles), "month") |>
    lapply(var_response) |>
    bind_rows()
  
  if (!is_null(vimp)) {
    vimp <- gsub("month[^ ]*", "month", vimp) |>
      unique()
    vimp <- var_abb()[vimp] |> unlist()
    if (log_bathy) {
      vimp["Bathy_depth"] <- "Bathymetry (log 10)"
    }
    
    quant_table <- quant_table |>
      mutate(across(variable, ~vimp[.x] |> unlist())) |>
      mutate(across(variable, ~factor(.x, levels = vimp)))
    
    eval_strip <- eval_strip |>
      mutate(across(variable, ~vimp[.x] |> unlist())) |>
      mutate(across(variable, ~factor(.x, levels = vimp)))
  }
  
  # Percentiles
  # eval_strip |>
  #   filter(`50%` > .01) |>
  #   mutate(diff = `97.5%`-`2.5%`) |>
  #   group_by(variable) |>
  #   summarize(median_range = median(diff),
  #             median_mean = median(`50%`),
  #             .groups = "keep") |>
  #   mutate(ratio = median_range/median_mean)
  
  # plotting
  plot <- ggplot(quant_table) +
    geom_rect(aes(xmin = `2.5%`, xmax = `97.5%`, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = .12) +
    geom_ribbon(data = eval_strip, 
                aes(x = value, ymax = `97.5%`, ymin = `2.5%`),
                fill = "grey50", alpha = .8) +
    geom_line(data = eval_strip, 
              mapping = aes(x = value, y = `50%`)) +
    facet_wrap(~ variable, scales = ifelse(same_y, "free_x", "free")) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Variable value", y = "Patch probability") +
    ggtitle(paste(v, 
                  ifelse(is_null(post), "tau-b", "tau-h"), 
                  "Response Curves"))
  
  if (show_no_post && !is.null(post)) {
    plot <- plot + 
      geom_line(data = eval_strip, aes(x = value, y = no_post), 
                linetype = 6, color = "blue")
  }

  plot <- plot +
    geom_vline(aes(xintercept = `50%`), color = "red")
  
  if (save_plot) {
    savename <- paste0("response_curves_mon", mid_mon, 
                       ifelse(!is_null(post), "_corrected", ""),
                       ifelse(show_no_post, "WNoPost", ""),
                       ifelse(log_bathy, "_logbathy", ""),
                       ifelse(same_y, "_fixedY", ""),
                       ifelse(!is_null(bottom_latitude), paste0("_lat", bottom_latitude), ""))
    save_analysis(plot, v, savename)
  } else {
    plot
  }
}

# creates a 2D response curve plot
response_curve_2var <- function(v,
                                data,
                                all_vars = c("Bathy_depth", "SST", "SSS", 
                                             "Tbtm", "MLD", "Sbtm", "Vel"),
                                var1 = "Bathy_depth",
                                var2 = "SST",
                                mid_mon = 8,
                                patch_only_medians = FALSE,
                                post_func = NULL) {
  
  vdata <- data |>
    mutate(Vel = sqrt(U^2 + V^2))
  
  # retrieving workflow
  wkfs <- get_v_wkfs(v)
  
  # acquiring quantile ranges of choice variables - max, min, median, 95% range
  quantiles <- vdata |>
    select(all_of(c(var1, var2))) |>
    apply(2, function(x) quantile(x, probs = c(0, .025, .5, .975, 1)))
  
  # generating steps for choice variables between max and min
  range_steps <- map2_dfc(quantiles['0%',], quantiles['100%',], 
                          ~seq(.x, .y, length.out = 50))
  
  medians <- (if(patch_only_medians) {
                vdata |>
                  filter(patch == "1")
              } else {
                vdata
              }) |>
    select(all_of(all_vars)) |>
    apply(2, function(x) quantile(x, probs = c(.5))) |>
    as_tibble_row() |>
    mutate(month = as.factor(mid_mon))
  
  quant_table <- quantiles |>
    base::t() |>
    as_tibble(rownames = "variable") |>
    select(-all_of(c("0%", "100%")))
  
  base <- bind_cols(lat = NA, 
                    lon = NA,
                    select(medians, -all_of(c(var1, var2))))
  
  in_df <- bind_cols(base,
                     expand.grid(var1 = range_steps[[1]],
                                 var2 = range_steps[[2]])) |>
    rename(V = Vel, !!var1 := var1, !!var2 := var2) |>
    mutate(U = 0)
  
  model_preds <- apply_quantile_preds(wkfs, in_df, .5) |>
    bind_cols(var1_value = pull(in_df, ifelse(var1 == "Vel", "V", var1)) |> as.numeric(),
              var2_value = pull(in_df, ifelse(var2 == "Vel", "V", var2)) |> as.numeric())
    
  # applying post correction, if applicable
  if (!is.null(post_func)) {
    bathymetry_col <- (if(var1 == "Bathy_depth" || var2 == "Bathy_depth") {
      in_df$Bathy_depth
    } else {
      medians[["Bathy_depth"]]
    })
    
    model_preds <- model_preds |>
      mutate(bathymetry_col = bathymetry_col) |>
      mutate(`50%` = `50%` * post_func(bathymetry_col)) |>
      select(-bathymetry_col)
  }
  
  # plotting
  ggplot(model_preds) +
    geom_raster(aes(x = var1_value, y = var2_value, fill = `50%`)) +
    theme_bw() +
    labs(x = var1, y = var2, fill = "prob") +
    ggtitle(paste(v, var1, var2,
                  ifelse(is_null(post), "tau-b", "tau-h"), 
                  "Response Raster")) +
    scale_fill_viridis(option = "viridis") +
    geom_vline(aes(xintercept = medians[[var1]]), color = "white") +
    geom_hline(aes(yintercept = medians[[var2]]), color = "white")
}

# create plots of abundance vs. dry weight
pred_v_dryweight <- function(v, threshold_method, model_preds, save = TRUE) {
  
  ref <- data_from_config(read_config(v)$training_data, 
                          vars_override = "dry_weight",
                          threshold_method = threshold_method) |>
    select(patch, dry_weight) |>
    bind_cols(.pred_1 = model_preds$`50%`)
  
  # performing correlation test
  cortest <- cor.test(ref$dry_weight, ref$.pred_1, 
                      method = "spearman", exact = FALSE)
  
  cortest
  
  # creating plottable objects
  base <- ggplot(ref, aes(x = log10(dry_weight + 1), y = .pred_1)) +
    theme_bw() +
    labs(y = expression(τ[b] * "-patch probability"), 
         x = "Dry weight (µg)(log10(x + 1))") +
    ggtitle(expression(τ[b]*"-patch probability vs. dry weight")) +
    coord_cartesian(expand = TRUE)
  
  density <- base +
    theme(legend.position = "bottom") +
    geom_hex(bins = 45) +
    #stat_density_2d(aes(fill = ..level..), geom = "polygon") +
    #scale_fill_distiller(palette = "Purples", direction = 1) +
    scale_fill_viridis(direction = -1) +
    geom_hline(yintercept = .5)
  
  points <- base +
    geom_point(alpha = .1, size = ifelse(save, 1, .8)) +
    geom_hline(yintercept = .5, color = "red") + 
    geom_vline(xintercept = log10(30000*195 + 1), color = "red")
  
  points_color <- base +
    geom_point(aes(col = patch), alpha = .2) +
    scale_color_manual(values = c("orange", "blue")) + 
    geom_hline(yintercept = .5, color = "black", linewidth = .5) + 
    labs(col = "Patch")
  
  points_smooth <- base + 
    geom_point(aes(col = patch), alpha = .2) +
    scale_color_manual(values = c("orange", "blue")) + 
    labs(col = "Patch") + 
    geom_smooth(col = 'black', linewidth = .75)
  
  annotate_help <- function(plot) {
    if(save) {
      x_val = 1
      y_val = c(.9, .85, .8)
    } else {
      x_val = 1.8
      y_val = c(.93, .83, .73)
    }
    
    plot +
      annotate("text", x = x_val, y = y_val,
               label = c(paste("rho =", round(cortest$estimate, 4)), 
                         "p < 2.2e-16", 
                         paste("n =", nrow(ref))))
  }
  
  if (save) {
    list(density, points, points_color, points_smooth) |>
      lapply(annotate_help) |>
      save_analysis(v, "predictions_vs_dryweight")
  } else {
    annotate_help(points)
  }
}

# predicitions vs. dryweight grams units
pred_v_dryweight_g <- function(v, threshold_method, model_preds, save = TRUE) {
  
  ref <- data_from_config(read_config(v)$training_data, 
                          vars_override = "dry_weight",
                          threshold_method = threshold_method) |>
    select(patch, dry_weight) |>
    mutate(dry_weight_g = dry_weight/(10^6)) |>
    bind_cols(.pred_1 = model_preds$`50%`)
  
  # performing correlation test
  cortest <- cor.test(ref$dry_weight_g, ref$.pred_1, 
                      method = "spearman", exact = FALSE)
  
  cortest
  
  # creating plottable objects
  base <- ggplot(ref, aes(x = log10(dry_weight_g + 1), y = .pred_1)) +
    theme_bw() +
    labs(y = expression(τ[b] * "-patch probability"), 
         x = expression("Water column biomass ("*g*"·"*m^{-2}*") ("*log[10]*"(x + 1))")) +
    ggtitle(expression(τ[b]*"-patch probability vs. water column biomass")) +
    coord_cartesian(expand = TRUE)
  
  density <- base +
    theme(legend.position = "bottom") +
    geom_hex(bins = 45) +
    #stat_density_2d(aes(fill = ..level..), geom = "polygon") +
    #scale_fill_distiller(palette = "Purples", direction = 1) +
    scale_fill_viridis(direction = -1) +
    geom_hline(yintercept = .5)
  
  points <- base +
    geom_point(alpha = .1, size = ifelse(save, 1, .8)) +
    geom_hline(yintercept = .5, color = "red") + 
    geom_vline(xintercept = log10(5.85 + 1), color = "red")
  
  points_color <- base +
    geom_point(aes(col = patch), alpha = .2) +
    scale_color_manual(values = c("orange", "blue")) + 
    geom_hline(yintercept = .5, color = "black", linewidth = .5) + 
    labs(col = "Patch")
  
  points_smooth <- base + 
    geom_point(aes(col = patch), alpha = .2) +
    scale_color_manual(values = c("orange", "blue")) + 
    labs(col = "Patch") + 
    geom_smooth(col = 'black', linewidth = .75)
  
  annotate_help <- function(plot) {
    if(save) {
      x_val = 1
      y_val = c(.9, .85, .8)
    } else {
      x_val = 1.9
      y_val = c(.28, .18, .08)
    }
    
    plot +
      annotate("text", x = x_val, y = y_val,
               label = c(paste("rho =", round(cortest$estimate, 4)), 
                         "p < 2.2e-16", 
                         paste("n =", nrow(ref))))
  }
  
  if (save) {
    list(density, points, points_color, points_smooth) |>
      lapply(annotate_help) |>
      save_analysis(v, "predictions_vs_dryweight")
  } else {
    annotate_help(points)
  }
}

# plot false positives/negatives
heatmap_geography <- function(v, data, model_preds) {
  
  results <- data |>
    transmute(lat, lon,
              heatmap_val = paste0(patch,
                                   as.numeric(model_preds$`50%` >= .5)))
  
  # color schema
  status <- list("00" = "True Negative",
                 "01" = "False Positive", 
                 "10" = "False Negative", 
                 "11" = "True Positive")
  # palette for colors 
  pal <- c("00" = "#EAEAEA",
           "01" = "#54BF21", 
           "10" = "#CD0000",  
           "11" = "#FFD82E")
  
  plot <- ggplot(results, aes(x = lon, y = lat)) +
    geom_point(aes(col = heatmap_val), cex = .3, alpha = .6) +
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
    ggtitle(paste(v, "location of Accurate/Inaccurate Raw Patch"))
  
  save_analysis(list(plot, plot + facet_wrap(~heatmap_val)),
                v, "geographic_heatmap")
}

