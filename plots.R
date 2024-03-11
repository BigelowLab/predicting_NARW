#source("setup.R")
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
#' @param quantile bool, quantile data?
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
get_difference_data <- function(base_preds, comparison_preds, quantile = NULL) {
  
  var <- ifelse(is.null(quantile), ".pred_1", quantile)
  
  purrr::map2(base_preds, 
              comparison_preds, 
              ~mutate(.x, !!var := pull(.x, var) - pull(.y, var)))
}

#' Takes a Cfin and Chyp prediction set and returns combined predictions 
#'   computed as the probability of either patch. 
get_combined_data <- function(cfin_preds, chyp_preds, quantile = NULL) {
  
  if (is.null(quantile)) { # raw data 
    purrr::map2(cfin_preds, chyp_preds,
                ~mutate(.x, .pred_1 = 1 - (.x$.pred_0 * .y$.pred_0)))
    
  } else { # combined data
    purrr::map2(
      cfin_preds, chyp_preds,
      ~mutate(.x, 
              !!quantile := pull(.x, quantile) + pull(.y, quantile) -
                pull(.x, quantile) * pull(.y, quantile)))
  }
}

#' Computes threshold status -- how patch probability shifted relative to a 
#'   critical threshold for two prediction sets. Results are in a 
#'   REL_PRESENCE column: FALSE_FALSE, FALSE_TRUE, TRUE_FALSE, or TRUE_TRUE
#'   @param quantile dbl, quantile column. if null, use .pred_1
get_threshold_data <- function(base_preds, comparison_preds, 
                               threshold = .3, quantile = NULL) {
  
  col <- ifelse(is.null(quantile), ".pred_1", paste0(quantile*100, "%"))
  
  purrr::map2(base_preds, comparison_preds, 
              ~mutate(.x, 
                      REL_PRESENCE = paste0(pull(.y, col) > threshold, 
                                            "_", 
                                            pull(.x, col) > threshold) |>
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

#gridded alternative
grid_12_plots <- function(plotlist, title) {
  if (length(plotlist) != 12) {stop("Plotlist must be length 12")}
  #titles <- map2(letters[1:12], mon_names(), ~paste0(.x, ") ", .y))
  titles <- mon_names()
  
  new_plotlist <- map2(plotlist, titles, 
                       ~.x +
                         theme(axis.ticks = element_blank(), 
                               axis.text = element_blank(), 
                               axis.title = element_blank(),
                               plot.title = element_text(size = 10,
                                                         margin = margin(t = 10, b = -15),
                                                         hjust = .06)
                               ) +
                         ggtitle(.y))
  
  ggstatsplot::combine_plots(
    plotlist = new_plotlist, 
    plotgrid.args = list(nrow = 4, ncol = 3),
    annotation.args = list(
      theme = theme(legend.position = "bottom")))
}

grid_6_plots <- function(plotlist, title) {
  if (length(plotlist) != 6) {stop("Plotlist must be length 6")}
  #titles <- map2(letters[1:12], mon_names(), ~paste0(.x, ") ", .y))
  titles <- names(plotlist)
  
  new_plotlist <- map2(plotlist, titles, 
                       ~.x +
                         theme(axis.ticks = element_blank(), 
                               axis.text = element_blank(), 
                               axis.title = element_blank(),
                               plot.title = 
                                 element_text(size = 12, 
                                              margin= margin(t=10, b=-17),
                                              colour = "black",
                                              hjust = .96)) +
                         ggtitle(.y))
  
  ggstatsplot::combine_plots(
    plotlist = new_plotlist, 
    plotgrid.args = list(nrow = 3, ncol = 2),
    annotation.args = list(
      theme = theme(legend.position = "bottom")))
}

######## PLOT GENERATION HELPERS ############

#' base plotting function - returns a base plot common to all plot functions
plot_base <- function(mon_data, downsample, plotCol = ".pred_1",
                      cropped = TRUE, title = NULL,
                      cex_override = NULL) {
  if (cropped) {
    xlim <- c(-77.0, -42.5) 
    ylim <- c(36.5, 56.7) #c(35.2,  57.6)
    cex <- c(.5, .8, 1.7, 2)[downsample + 1]
  } else {
    xlim <- ylim <- NULL 
    cex <- c(.23, .17, .4, .4)[downsample + 1]
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
                   ylim = ylim, 
                   expand = FALSE) +
    ggtitle(title)
  
  # p <- ggplot(mon_data, aes(x = lon, y = lat)) +
  #   geom_point(aes(col = get(plotCol)), cex = cex, pch = 15) +
  #   theme_void() + 
  #   theme(panel.grid.minor = element_blank(),
  #         panel.grid.major = element_blank(),
  #         legend.position = "none",
  #         panel.background = element_rect(fill='transparent'),
  #         plot.background = element_rect(fill='transparent', color=NA)) +
  #   labs(x = "Latitude", y = "Longitude") +
  #   coord_quickmap(xlim = xlim, 
  #                  ylim = ylim) +
  #   ggtitle(title) +
  #   # REMOVE
  #   scale_color_viridis(option = "inferno", limits = c(0, .4))
  # 
  # ggsave("transparentplot.png", p, bg='transparent')
}

#' Functions that each tack on additional needed attributes for plot type
plot_raw <- function(plot_base, top_limit = 1) {
  plot_base + 
    scale_color_viridis(option = "turbo", limits = c(0, top_limit)) +
    labs(color = "Patch Probability") + 
    # new!
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "white")
}

plot_difference <- function(plot_base, limit = .9) {
  plot_base + 
    scale_color_gradientn(limits = c(-limit, limit),
                          colors = c("darkorchid4",
                                     "darkslateblue",
                                     "royalblue3", 
                                     "deepskyblue2", 
                                     "white", 
                                     "goldenrod2",
                                     "darkorange1",
                                     "orangered3", 
                                     "red4"),
                          na.value = "white",
                          breaks = c(-.3, -.15, 0, .15, .3)) +
    labs(color = "Relative patch probability") + 
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "black")
}

plot_threshold <- function(plot_base) {
  # naming factor levels 
  feedstatus <- list(FALSE_FALSE = "No Habitat",
                     FALSE_TRUE = "New Habitat", 
                     TRUE_FALSE = "Lost Habitat", 
                     TRUE_TRUE = "Retained Habitat")
  
  # palette for colors 
  pal <- c(FALSE_FALSE = "white",
           FALSE_TRUE = "#0295d9", 
           TRUE_FALSE = "#CD0000", 
           TRUE_TRUE = "#FFD82E")
  
  plot_base +
    scale_color_manual(labels = feedstatus,
                       values = pal,
                       drop = FALSE) + 
    guides(colour = guide_legend(
      override.aes = list(size=3,
                          shape = 22,
                          color = c("black", pal[2:4]),
                          fill = pal))) +
    labs(color = NULL) + #"Patch Status") + 
    geom_polygon(data = ggplot2::map_data("world"), 
                 aes(long, lat, group = group),
                 fill = "black")
}

#' Processes a data list and siphons to correct plotting function
plot_data_list <- function(data_list, downsample, plot_func, plot_col,
                           cropped = TRUE, gridded = TRUE, title = NULL) {
  
  cex_override <- NULL
  if (gridded) {
    if (length(data_list) == 4) {
      cex_override <- c(.35, .45, .6, .7)[downsample + 1]
    } else {
      cex_override <- c(.005, .1, .2, .2) [downsample + 1]
    }
  }
  
  plots_list <- data_list |>
    purrr::imap(~plot_base(.x, downsample, plot_col, cropped, 
                          title = paste(title, "-", .y),
                          cex_override = cex_override) |>
                 plot_func())
  
  if (gridded) {
    grid_func <- ifelse(length(plots_list) == 4, grid_plots, 
                        ifelse(length(plots_list) == 6, grid_6_plots, 
                               grid_12_plots))
    
    plots_list <- plots_list |>
      grid_func(title)
  }
  
  plots_list
}

### MAIN FUNCTIONS ##########

# HELPER
run_scenarios <- function(v, downsample, plot_scenarios, save_months, 
                          cropped, gridded, plot_func, plot_col,
                          process_data, get_title, filename_base, 
                          quantile = NULL,
                          combining_v = NULL) {
  
  is_quantile <- !is.null(quantile)
  
  # processes climate situations
  run_scenario <- function(year, scenario) {
    
    title <- get_title(v, year, scenario, combining_v)
    
    preds_list <- read_preds(v, year, scenario, save_months, is_quantile)
    if(!is.null(combining_v)) {
      preds_list <- 
        read_preds(combining_v, year, scenario, save_months, is_quantile) |>
        get_combined_data(preds_list, quantile)
    }
    preds_list <- preds_list |> process_data()
    
    for (crop in cropped) {
      plots_list <- plot_data_list(preds_list, downsample, plot_func, plot_col, 
                                   crop, gridded = gridded, title = title)
      
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

# PLOT METHODS
get_quant_raw_plots <- function(v,
                                downsample,
                                plot_scenarios = 1:5,
                                save_months = 1:12, 
                                cropped = TRUE, gridded = FALSE,
                                quant_col = .5,
                                top_limit = 1,
                                combining_v = NULL) {
  
  quant_name <- paste0(quant_col*100, "%")
  combined <- !is.null(combining_v)
  
  # defining pass-in variables
  process_data <- identity
  get_title <- function(v, year, scenario, combining_v) {
    paste(ifelse(combined, 
                 paste(v, "and", combining_v), v),
          ifelse(scenario != "PRESENT", 
                 paste(scenario, year), scenario),
          quant_name, "Quant Patch Probability")
  }
  
  filename_base <- paste0("plot_", quant_col*100)
  if (combined) {
    filename_base <- paste0(v, "_", combining_v, filename_base)
  }
  
  plot_method <- function(x) {plot_raw(x, top_limit = top_limit)}
  
  # call to base function
  run_scenarios(v, downsample, plot_scenarios, save_months, 
                cropped, gridded, plot_method, quant_name, 
                process_data, get_title, filename_base,
                quantile = quant_name, combining_v = combining_v)
}

get_quant_threshold_plots <- function(v, 
                                      downsample, 
                                      plot_scenarios = 1:5, 
                                      threshold = .3, 
                                      save_months = 1:12, 
                                      cropped = TRUE, gridded = FALSE,
                                      quant_col = .5,
                                      combining_v = NULL) {
  quant_name <- paste0(quant_col*100, "%")
  combined <- !is.null(combining_v)
  
  # defining pass-in variables
  comparison_preds <- read_preds(v, NA, "PRESENT", save_months,
                                 quantile = TRUE)
  if (combined) {
    comparison_preds <- 
      read_preds(combining_v, NA, "PRESENT", save_months, TRUE) |>
      get_combined_data(comparison_preds, quant_name)
  }
  
  process_data <- function(base_preds) {
    get_threshold_data(base_preds, comparison_preds, threshold, 
                       quantile = quant_col)
  }
  
  get_title <- function(v, year, scenario, combining_v = NULL) {
    paste(ifelse(combined, 
                 paste(v, "and", combining_v), v), 
          scenario, year,
          "Patch Change (Threshold: ", threshold, ")")
  }
  filename_base <- paste0("feedplot", threshold, "_quant", quant_col*100)
  if (combined) {
    filename_base <- paste0(v, "_", combining_v, filename_base)
  }
  
  # call to base function
  run_scenarios(v, downsample, plot_scenarios, save_months, 
                cropped, gridded, plot_threshold, "REL_PRESENCE", 
                process_data, get_title, filename_base,
                quantile = quant_name, combining_v)
}

get_quant_diff_plots <- function(v, 
                                 downsample, 
                                 plot_scenarios = 1:5, 
                                 comparison_scenario = 5, 
                                 save_months = 1:12, 
                                 cropped = TRUE, gridded = FALSE,
                                 quant_col = .5,
                                 combining_v = NULL,
                                 limit = .3) {
  
  quant_name <- paste0(quant_col*100, "%")
  combined <- !is.null(combining_v)
  
  # defining pass-in variables
  comp_s <- climate_table(comparison_scenario)
  comparison_preds <- read_preds(v, 
                                 year = comp_s$year,
                                 scenario = comp_s$scenario,
                                 save_months = save_months,
                                 quantile = TRUE)
  if (combined) {
    comparison_preds <- 
      read_preds(combining_v, comp_s$year, comp_s$scenario, save_months, TRUE) |>
      get_combined_data(comparison_preds, quant_name)
  }
  
  process_data <- function(base_preds) {
    get_difference_data(base_preds, comparison_preds, quantile = quant_name)
  }
  
  get_title <- function(v, year, scenario, combining_v) {
    paste(ifelse(combined, paste(v, "and", combining_v), v), 
          ifelse(scenario != "PRESENT", 
                 paste(scenario, year), scenario),
          "Change in", quant_name, "Patch Probability")
  }

  filename_base <- "plot_diff"
  if (combined) {
    filename_base <- paste0(v, "_", combining_v, filename_base)
  }
  
  plot_function <- function(x) {plot_difference(x, limit = limit)}
  
  # call to base function
  run_scenarios(v, downsample, plot_scenarios, save_months, 
                cropped, gridded, plot_function, quant_name, 
                process_data, get_title, filename_base,
                quantile = quant_name, combining_v = combining_v)
}

# OTHER
get_quant_range_plots <- function(v, downsample, year, scenario, 
                                  save_months, 
                                  cropped = TRUE, gridded = FALSE,
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

