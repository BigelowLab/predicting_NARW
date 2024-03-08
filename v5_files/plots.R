source("setup.R")
library(RColorBrewer)
library(viridis)
library(patchwork)
library(ggpubr)

##### FILE/DATA HELPERS ##########

#' Retrieves prediction data from file
#' 
#' @param v chr, the version being retrieved
#' @param year int, the year being retrieved
#' @param scenario chr, the scenario being retrieved
#' @param save_months array of numerical months to retrieve
#' @return list, prediction data named by month
read_preds <- function(v, year, scenario, save_months = 1:12, 
                       quantile = FALSE) {
  
  folder <- pred_path(v, year, scenario)
  # if directory does not exist, return false
  if(!dir.exists(folder)) {
    stop(paste("Predictions for", year, scenario, 
               "don't exist. Skipping plots."))
  }
  
  wd <- getwd()
  setwd(folder)
  
  file_prefix <- ifelse(quantile, "quant_preds", "predictions")
  
  # helper method that retrieves data for a month
  read_month <- function(mon) {
    filename <- paste0(file_prefix, mon, ".csv.gz")
    
    if (file.exists(filename)) {
      readr::read_csv(filename, col_types = readr::cols())
    } else {
      NULL
    }
  }
  
  # constructing list of prediction data named by month
  preds <- setNames(1:12, mon_names())[save_months] |>
    lapply(read_month)
  
  setwd(wd)
  preds
}

#' Saves a plot object to file
#' 
#' @param plot_obj object/object list to save to file
#' @param v chr, version
#' @param year int, year
#' @param scenario chr, scenario
#' @param filename chr, filename
#' @param verbose bool, print destination filename?
#' @return TRUE if executes successfully
save_plots <- function(plot_obj, v, year, scenario, filename) {
  
  ppath <- pred_path(v, year, scenario, filename)
  
  pdf(ppath)
  print(plot_obj)
  dev.off()
  
  TRUE
}

########### DATA PREP HELPERS ###############

#' Returns raw data
get_raw_data <- identity

#' Takes two prediction sets and returns a new prediction set expressing the 
#'   difference between the two datasets. Intended to feed to difference plots.
#'   Assumes both datasets have same downsample value and month set
#'   
#' @param base_preds the "current" dataset
#' @param comparison_preds the "prior" dataset
#' @return list of difference data
get_difference_data <- function(base_preds, comparison_preds) {
  purrr::map2(base_preds, 
              comparison_preds, 
              ~mutate(.x, .pred_1 = .pred_1 - .y$.pred_1))
}

#' Takes a Cfin and Chyp prediction set and returns combined predictions 
#'   computed as the probability of either patch. 
get_combined_data <- function(cfin_preds, chyp_preds) {
  purrr::map2(cfin_preds, chyp_preds,
              ~mutate(.x, .pred_1 = 1 - (.x$.pred_0 * .y$.pred_0)))
}

#' Computes threshold status -- how patch probability shifted relative to a 
#'   critical threshold for two prediction sets. Results are in a 
#'   REL_PRESENCE column: FALSE_FALSE, FALSE_TRUE, TRUE_FALSE, or TRUE_TRUE
get_threshold_data <- function(base_preds, comparison_preds, 
                               threshold = .3) {
  purrr::map2(base_preds, comparison_preds, 
              ~mutate(.x, 
                      REL_PRESENCE = paste0(.y$.pred_1 > threshold, 
                                            "_", 
                                            .x$.pred_1 > threshold) |>
                        factor(levels = c("FALSE_FALSE", "FALSE_TRUE", 
                                          "TRUE_FALSE", "TRUE_TRUE"))))
}

#' Computes threshold status by quantile - how top X% of preds shifted
get_threshold_percent_data <- function(base_preds, comparison_preds, 
                               threshold_percent = .1,
                               filter_bathy = FALSE) {
  
  process_month <- function(base_pred, comparison_pred) {
    base_vec <- base_pred$.pred_1
    comp_vec <- comparison_pred$.pred_1
    
    if (filter_bathy) {
      base_vec <- base_vec[base_pred$Bathy_depth < 180]
      comp_vec <- comp_vec[comparison_pred$Bathy_depth < 180]
    }
    
    base_threshold <- quantile(base_vec, 1 - threshold_percent)[[1]]
    comp_threshold <- quantile(comp_vec, 1 - threshold_percent)[[1]]
    
    mutate(base_pred, 
           REL_PRESENCE = paste0(comparison_pred$.pred_1 > comp_threshold, 
                                 "_", 
                                 base_pred$.pred_1 > base_threshold) |>
             factor(levels = c("FALSE_FALSE", "FALSE_TRUE", 
                               "TRUE_FALSE", "TRUE_TRUE")))
  }
  
  purrr::map2(base_preds, comparison_preds, 
              process_month)
}

get_threshold_table <- function(data_list,
                                threshold = .1) {
  data_list |>
    purrr::imap(~count(.x, 'Patch' = .pred_1 > threshold) |>
                  mutate('Month' = .y, 'Percent Patch' = n/sum(n))) |>
    bind_rows() |>
    filter(Patch) |>
    select(Month, `Percent Patch`)
}

########## PLOT FORMATTING HELPERS ############

#' Takes a list of four plots and formats into a 2x2 printable grid object
#'   Could be customized in future to accept other grid dims
grid_plots <- function(plotlist, title) {
  if (length(plotlist) != 4) {stop("Plotlist must be length 4")}
  months <- names(plotlist)
  
  top_left <- plotlist[[1]] + 
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    ggtitle(paste("a)", months[1]))
  
  top_right <- plotlist[[2]] + 
    theme(axis.ticks = element_blank(), 
          axis.text = element_blank()) +
    ggtitle(paste("b)", months[2]))
  
  bottom_left <- plotlist[[3]] +
    ggtitle(paste("c)", months[3]))
  
  bottom_right <- plotlist[[4]] +
    theme(axis.ticks.y = element_blank(), 
          axis.text.y = element_blank()) +
    ggtitle(paste("d)", months[4]))
  
  grid <- (top_left + top_right + bottom_left + bottom_right +
             plot_layout(nrow = 2, ncol = 2, guides = "collect") +
             plot_annotation(title)) &
    theme(legend.position = "bottom", 
          axis.title = element_blank())
  
  grid
}

######## PLOT GENERATION HELPERS ############

#' base plotting function - returns a base plot common to all plot functions
plot_base <- function(mon_data, downsample, plotCol = ".pred_1",
                      cropped = TRUE, gridded = TRUE, title = NULL,
                      cex_override = NULL) {
  if (cropped) {
    xlim <- c(-77.0, -42.5) 
    ylim <- c(36.5,  56.7)
    cex <- c(.5, .8, 1.7, 2)[downsample + 1]
  } else {
    xlim <- ylim <- NULL 
    cex <- c(.23, .17, .4, .4)[downsample + 1]
  }
  
  if (gridded) {
    cex <- c(.35, .45, .6, .7)[downsample + 1]
  }
  
  if (!is.null(cex_override)) {
    cex <- cex_override
  }
  
  ggplot(mon_data, aes(x = lon, y = lat)) +
    geom_point(aes(col = get(plotCol)), cex = cex, pch = 15) +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "bottom") +
    labs(x = "Latitude", y = "Longitude") +
    coord_quickmap(xlim = xlim, 
                   ylim = ylim) +
    ggtitle(title)
}

#' Functions that each tack on additional needed attributes for plot type
plot_raw <- function(plot_base, top_limit = 1) {
  plot_base + 
    scale_color_viridis(option = "inferno", limits = c(0, top_limit)) +
    labs(color = "Patch Probability")
}

plot_difference <- function(plot_base, limit = .9) {
  plot_base + 
    scale_color_gradientn(limits = c(-limit, limit),
                          colors = c("midnightblue",
                                     "midnightblue",
                                     "dodgerblue3",
                                     "deepskyblue1",
                                     "gray94", 
                                     "goldenrod1",
                                     "darkorange1",
                                     "orangered3",
                                     "orangered3"),
                          na.value = "white") +
    labs(color = "Change in probability")
}

plot_threshold <- function(plot_base) {
  # naming factor levels 
  feedstatus <- list(FALSE_FALSE = "No Patch",
                     FALSE_TRUE = "New Patch", 
                     TRUE_FALSE = "Lost Patch", 
                     TRUE_TRUE = "Patch")
  
  # palette for colors 
  pal <- c(FALSE_FALSE = "#EAEAEA",
           FALSE_TRUE = "#54BF21", 
           TRUE_FALSE = "#CD0000", 
           TRUE_TRUE = "#FFD82E")
  
  plot_base +
    scale_color_manual(labels = feedstatus,
                       values = pal) + 
    guides(colour = guide_legend(override.aes = list(size=2))) +
    labs(color = "Patch Status")
}

#' Processes a data list and siphons to correct plotting function
plot_data_list <- function(data_list, downsample, plot_func, plot_col,
                           cropped = TRUE, gridded = TRUE, title = NULL) {
  
  plots_list <- data_list |>
    purrr::imap(~plot_base(.x, downsample, plot_col, cropped, gridded, 
                          title = paste(title, "-", .y)) |>
                 plot_func())
  
  if (gridded) {
    plots_list <- plots_list |>
      grid_plots(title)
  }
  
  plots_list
}

### MAIN FUNCTIONS ##########

# HELPER
run_scenarios <- function(v, downsample, plot_scenarios, save_months, 
                          cropped, gridded, plot_func, plot_col,
                          process_data, get_title, filename_base, 
                          quantile = FALSE) {
  
  # processes climate situations
  run_scenario <- function(year, scenario) {
    
    title <- get_title(v, year, scenario)
    
    preds_list <- read_preds(v, year, scenario, save_months = save_months, 
                             quantile = quantile) |>
      process_data()
    
    for (crop in cropped) {
      plots_list <- plot_data_list(preds_list, downsample, plot_func, plot_col, 
                                   cropped, gridded = gridded, title = title)
      
      filename <- paste0(filename_base,
                         ifelse(cropped, "_cropped", ""),
                         ifelse(gridded, "_gridded", ""),
                         ".pdf")
      
      plots_list |>
        save_plots(v, year, scenario, filename)
    }
    
    TRUE
  }
  
  # plotting predictions
  climate_table(plot_scenarios) |>
    mutate(success = run_scenario(year, scenario))
}

# BASIC
get_raw_plots <- function(v,
                          downsample,
                          plot_scenarios = 1:5,
                          save_months = 1:12, 
                          cropped = c(TRUE, FALSE), gridded = FALSE,
                          top_limit = 1) {
  # defining pass-in variables
  process_data <- identity
  get_title <- function(v, year, scenario) {
    paste(v, 
          ifelse(scenario != "PRESENT", 
                 paste(scenario, year), scenario),
          "Calanus Presence Probability")
  }
  filename_base <- "plot"
  
  plot_method <- function(x) {plot_raw(x, top_limit = top_limit)}
  
  # call to base function
  run_scenarios(v, downsample, plot_scenarios, save_months, 
                cropped, gridded, plot_method, ".pred_1", 
                process_data, get_title, filename_base)
}

get_difference_plots <- function(v, 
                                 downsample, 
                                 plot_scenarios = 1:5, 
                                 comparison_scenario = 5, 
                                 save_months = 1:12, 
                                 cropped = c(TRUE, FALSE), gridded = FALSE) {
  # defining pass-in variables
  compare_spec <- climate_table(comparison_scenario)
  comparison_preds <- read_preds(v, 
                                 year = compare_spec$year,
                                 scenario = compare_spec$scenario,
                                 save_months = save_months)
  process_data <- function(base_preds) {
    get_difference_data(base_preds, comparison_preds)
  }
  
  get_title <- function(v, year, scenario) {
    paste(v, 
          ifelse(scenario != "PRESENT", 
                 paste(scenario, year), scenario),
          "Change in Calanus Presence Probability")
  }
  filename_base <- "plot_diff"
  
  # call to base function
  run_scenarios(v, downsample, plot_scenarios, save_months, 
                cropped, gridded, plot_difference, ".pred_1", 
                process_data, get_title, filename_base)
}

get_threshold_plots <- function(v, 
                                downsample, 
                                plot_scenarios = 1:5, 
                                threshold = .3, 
                                save_months = 1:12, 
                                cropped = c(TRUE, FALSE), gridded = FALSE) {
  # defining pass-in variables
  comparison_preds <- read_preds(v, NA, "PRESENT", save_months)
  process_data <- function(base_preds) {
    get_threshold_data(base_preds, comparison_preds, threshold)
  }
  
  get_title <- function(v, year, scenario) {
    paste(v, scenario, year, 
          "Patch Change (Threshold: ", threshold, ")")
  }
  filename_base <- paste0("feedplot", threshold)
  
  # call to base function
  run_scenarios(v, downsample, plot_scenarios, save_months, 
                cropped, gridded, plot_threshold, "REL_PRESENCE", 
                process_data, get_title, filename_base)
}

get_threshold_percent_plots <- function(v, 
                                        downsample, 
                                        plot_scenarios = 1:5, 
                                        threshold_perc = .1, 
                                        filter_bathy = FALSE,
                                        save_months = 1:12, 
                                        cropped = c(TRUE, FALSE), 
                                        gridded = FALSE) {
  
  # defining pass-in variables
  comparison_preds <- read_preds(v, NA, "PRESENT", save_months)
  process_data <- function(base_preds) {
    get_threshold_percent_data(base_preds, comparison_preds, threshold_perc,
                               filter_bathy)
  }
  
  get_title <- function(v, year, scenario) {
    paste(v, scenario, year, 
          "Patch Change (Threshold Quant: ", threshold_perc, ")")
  }
  filename_base <- paste0("feedplot_perc", threshold_perc)
  
  # call to base function
  run_scenarios(v, downsample, plot_scenarios, save_months, 
                cropped, gridded, plot_threshold, "REL_PRESENCE", 
                process_data, get_title, filename_base)
}

# COMPARE DIFFERENCE
compare_versions <- function(old_v,
                             new_v, 
                             downsample,
                             year = NA, 
                             scenario = "PRESENT",
                             save_months = 1:12,
                             cropped = FALSE, gridded = FALSE) {
  
  old_v_preds <- read_preds(old_v, year, scenario, save_months)
  new_v_preds <- read_preds(new_v, year, scenario, save_months)
  
  data_list <- get_difference_data(new_v_preds, old_v_preds)
  
  title <- paste(old_v, "vs.", new_v,
                 ifelse(scenario != "PRESENT", 
                        paste(year, scenario), scenario))
  
  plotlist <- plot_data_list(data_list, downsample, plot_difference, ".pred_1",
                             cropped, gridded, title = title)
  
  filename <- paste0(new_v, "_vs_", old_v, 
                     ifelse(cropped, "_cropped", ""),
                     ifelse(gridded, "_gridded", ""),
                     ".pdf")
  
  save_plots(plotlist, new_v, year, scenario, filename, verbose)
}

# COMBINED
get_combined_plots <- function(vcfin,
                               vchyp, 
                               downsample,
                               year = NA, 
                               scenario = "PRESENT",
                               save_months = 1:12,
                               cropped = FALSE, gridded = FALSE,
                               top_limit = 1) {
  
  vcfin_preds <- read_preds(vcfin, year, scenario, save_months)
  vchyp_preds <- read_preds(vchyp, year, scenario, save_months)
  
  data_list <- get_combined_data(vcfin_preds, vchyp_preds)
  
  title <- paste(vcfin, "and", vchyp, "combined", 
                 ifelse(scenario != "PRESENT", 
                        paste(year, scenario), scenario))
  
  plot_method <- function(x) {plot_raw(x, top_limit = top_limit)}
  
  plotlist <- plot_data_list(data_list, downsample, plot_method, ".pred_1",
                             cropped, gridded, title = title)
  
  filename <- paste0(vcfin, "_", vchyp, "_COMBINED",
                     ifelse(cropped, "_cropped", ""),
                     ifelse(gridded, "_gridded", ""),
                     ".pdf")
  
  save_plots(plotlist, vcfin, year, scenario, filename)
}

get_combined_difference_plots <- function(vcfin,
                                          vchyp, 
                                          downsample,
                                          year = NA, 
                                          scenario = "PRESENT",
                                          save_months = 1:12,
                                          cropped = FALSE, gridded = FALSE,
                                          limit = .5) {
  
  comparison_preds <- get_combined_data(
    read_preds(vcfin, 2075, "RCP85", save_months), 
    read_preds(vchyp, 2075, "RCP85", save_months)
  )
  
  base_preds <- get_combined_data(
    read_preds(vcfin, year, scenario, save_months),
    read_preds(vchyp, year, scenario, save_months)
  )
  
  data_list <- get_difference_data(base_preds, comparison_preds)
  
  title <- paste(vcfin, "and", vchyp, scenario, year, "Patch Difference")
  
  plotlist <- plot_data_list(data_list, downsample, 
                             function(x) plot_difference(x, limit = .5), ".pred_1",
                             cropped, gridded, title = title)
  
  filename <- paste0(vcfin, "_", vchyp, "_COMBINED_diff",
                     ifelse(cropped, "_cropped", ""),
                     ifelse(gridded, "_gridded", ""),
                     ".pdf")
  
  save_plots(plotlist, vcfin, year, scenario, filename)
}

get_combined_threshold_plots <- function(vcfin,
                                         vchyp, 
                                         downsample,
                                         year = NA, 
                                         scenario = "PRESENT",
                                         threshold = .3,
                                         save_months = 1:12,
                                         cropped = FALSE, gridded = FALSE) {
  
  comparison_preds <- get_combined_data(
    read_preds(vcfin, NA, "PRESENT", save_months), 
    read_preds(vchyp, NA, "PRESENT", save_months)
  )
  
  base_preds <- get_combined_data(
    read_preds(vcfin, year, scenario, save_months),
    read_preds(vchyp, year, scenario, save_months)
  )
  
  data_list <- get_threshold_data(base_preds, comparison_preds, threshold)
  
  title <- paste(vcfin, "and", vchyp, scenario, year, 
                 "Patch Change (Threshold: ", threshold, ")")
  
  plotlist <- plot_data_list(data_list, downsample, plot_threshold, "REL_PRESENCE",
                             cropped, gridded, title = title)
  
  filename <- paste0(vcfin, "_", vchyp, "_COMBINED_feedplot", threshold,
                     ifelse(cropped, "_cropped", ""),
                     ifelse(gridded, "_gridded", ""),
                     ".pdf")
  
  save_plots(plotlist, vcfin, year, scenario, filename)
  
  # percent table
  table_file <- paste0(vcfin, "_", vchyp, "COMBINED_perctable", threshold, ".csv")
  
  get_threshold_table(data_list, threshold) |>
    readr::write_csv(pred_path(vcfin, year, scenario, table_file))
}

get_combined_threshold_percent_plots <- function(vcfin,
                                                 vchyp, 
                                                 downsample,
                                                 year = NA, 
                                                 scenario = "PRESENT",
                                                 threshold_perc = .3,
                                                 filter_bathy = FALSE,
                                                 save_months = 1:12,
                                                 cropped = FALSE, gridded = FALSE) {
  
  comparison_preds <- get_combined_data(
    read_preds(vcfin, NA, "PRESENT", save_months), 
    read_preds(vchyp, NA, "PRESENT", save_months)
  )
  
  base_preds <- get_combined_data(
    read_preds(vcfin, year, scenario, save_months),
    read_preds(vchyp, year, scenario, save_months)
  )
  
  data_list <- get_threshold_percent_data(base_preds, comparison_preds, threshold_perc,
                                          filter_bathy)
  
  title <- paste(vcfin, "and", vchyp, scenario, year, 
                 "Patch Change (Threshold Quant: ", threshold_perc, ")")
  
  plotlist <- plot_data_list(data_list, downsample, plot_threshold, "REL_PRESENCE",
                             cropped, gridded, title = title)
  
  filename <- paste0(vcfin, "_", vchyp, "_COMBINED_feedplot_perc", threshold_perc,
                     ifelse(cropped, "_cropped", ""),
                     ifelse(gridded, "_gridded", ""),
                     ".pdf")
  
  save_plots(plotlist, vcfin, year, scenario, filename)
}

# QUANTILE
get_quant_perc_plots <- function(v, downsample, year, scenario, 
                                 save_months, cropped = TRUE, gridded = FALSE,
                                 quant_col = .50,
                                 top_limit = 1) {
  
  quant_name <- paste0(quant_col*100, "%")
  
  quantile_data <- read_preds(v, year, scenario, save_months,
                              quantile = TRUE)
  
  plot_quantile_perc <- function(plot_base) {
    plot_base + 
      scale_color_viridis(option = "inferno", limits = c(0, top_limit)) +
      labs(color = paste(quant_name, "Quantile Patch Probability"))
  }
  
  title <- paste(v, quant_name, "Quantile Patch Probability")
  
  quant_plots_list <- plot_data_list(quantile_data, downsample,
                                     plot_quantile_perc, quant_name, 
                                     cropped, gridded, title)
  
  filename <- paste0("QUANT_", quant_col*100,
                     ifelse(cropped, "_cropped", ""),
                     ifelse(gridded, "_gridded", ""),
                     ".pdf")
  
  save_plots(quant_plots_list, v, year, scenario, filename)
}

get_quant_range_plots <- function(v, downsample, year, scenario, 
                                  save_months, cropped = TRUE, gridded = FALSE,
                                  ci = .95,
                                  top_limit = .6) {
  
  upper_ci <- paste0((.5 + ci/2.0)*100, "%")
  lower_ci <- paste0((.5 - ci/2.0)*100, "%")
  ci_title <- paste0(ci*100, "%")
  
  quantile_data <- read_preds(v, year, scenario, save_months,
                              quantile = TRUE) |>
    map(~mutate(.x, CI = get(upper_ci) - get(lower_ci)))
  
  plot_quantile_range <- function(plot_base) {
    plot_base +
      scale_color_viridis(limits = c(0, top_limit)) +
      labs(col = paste(ci_title, "Confidence Interval Range"))
  }
  
  title <- paste(v, ci_title, "Confidence Interval")
  
  quant_plots_list <- plot_data_list(quantile_data, downsample,
                                     plot_quantile_range, "CI", 
                                     cropped, gridded, title)
  
  filename <- paste0("QUANTRANGE_", ci*100,
                     ifelse(cropped, "_cropped", ""),
                     ifelse(gridded, "_gridded", ""),
                     ".pdf")
  
  save_plots(quant_plots_list, v, year, scenario, filename)
}

