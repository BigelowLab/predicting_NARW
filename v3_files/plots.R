source("setup.R")
library(RColorBrewer)
library(viridis)

##### HELPERS ##########

#' Retrieves prediction data from file for a given climate situation
#' 
#' @param v chr, the version being retrieved
#' @param year int, the year being retrieved
#' @param scenario chr, the scenario being retrieved
#' @return df, a table with a "data" column with prediction data from each
#'   prediction file in climate situation folder and "mon" column with month 
#'   for corresponding climate data - NA if annual data
read_preds <- function(v, year, scenario) {
  
  folder <- pred_path(v, year, scenario)
  # if directory does not exist, return false
  if(!dir.exists(folder)) {
    stop(paste("Predictions for", year, scenario, 
               "don't exist. Skipping plots."))
  }
  
  wd <- getwd()
  setwd(folder)
  
  # helper method that retrieves data for a month
  read_month <- function(mon) {
    filename <- paste0("predictions", mon, ".csv.gz")
    
    if (file.exists(filename)) {
      readr::read_csv(filename, col_types = readr::cols())
    } else {
      NULL
    }
  }
  
  # constructing list of prediction data named by month
  preds <- 1:12 |>
    lapply(read_month) |>
    setNames(mon_names()) |>
    purrr::discard(is.null)
  
  setwd(wd)
  preds
}

#' ALTER THIS METHOD FOR ALTERNATIVE PLOT ARRANGEMENT
#' 
#' @param plots list, plot objects
#' @param path chr, file path to save to 
save_plots <- function(plots, v, year, scenario, filename) {
  pdf(pred_path(v, year, scenario, filename))
  print(plots)
  dev.off()
  
  TRUE
}

# grid 2x2 of plots
# INCOMPLETE: troubleshooting labels
save_plots_gridded <- function(plots, v, year, scenario, filename,
                               months = c(1, 4, 7, 10)) {
  
  winter <- test_plots[[months[[1]]]] + 
    theme(axis.ticks.x = element_blank(), 
          axis.text.x = element_blank())
  
  spring <- test_plots[[months[[2]]]] + 
    theme(axis.ticks = element_blank(), 
          axis.text = element_blank())
  
  summer <- test_plots[[months[[3]]]]
  
  fall <- test_plots[[months[[4]]]] +
    theme(axis.ticks.y = element_blank(), 
          axis.text.y = element_blank())

  winter + spring + summer + fall +
    plot_layout(nrow = 2, ncol = 2, guides = "collect") &
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          plot.title = element_blank())
} 

#' ALTER THIS METHOD TO CHANGE PLOT APPEARANCE
#' 
#' @param preds df, data to plot
#' @param title chr, the name of the plot
#' @param diff boolean, is this plot a difference plot?
#' @param cropped boolean, is this plot cropped?
#' @param downsample int, downsample factor 
#' @return plot of desired month 
plot_month <- function (preds, title, diff, cropped, downsample) {
  
  if (cropped) {
    xlim <- c(-77.0, -42.5) 
    ylim <- c(36.5,  56.7)
    cex <- c(.5, .5, .8, 1)[downsample+1]
  } else {
    xlim <- ylim <- NULL 
    cex <- c(.17, .17, .3, .4)[downsample+1]
  }
  
  # ggplot base
  plot_base <- ggplot(preds, aes(x = lon, y = lat, col = .pred_1)) +
    geom_point(cex = cex, pch = 15) +
    coord_quickmap(xlim = xlim,
                   ylim = ylim) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "bottom") +
    labs(x = "Latitude", y = "Longtitude", color = NULL) +
    ggtitle(title)
  
  # desired color scheme depends on whether this is raw or comparison plot
  if (diff) {
    plot <- plot_base +
      scale_color_gradientn(limits = c(-.9, .9),
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
  } else {
    plot <- plot_base +
      scale_color_viridis(option = "inferno", limits = c(0, 1))
  }
  
  plot
}

### MAIN FUNCTION ##########

# goal: produce 4 plots per desired scenario (2 for present day)
get_plots <- function(v = "v3.00", 
                      plot_scenarios = 1:5,
                      comparison_scenario = 5, 
                      downsample = c(0, 1, 2, 3)[1]) {
  
  # retrieving comparison predictions 
  compare_spec <- climate_table(comparison_scenario)
  comparison_preds <- read_preds(v, 
                                 year = compare_spec$year,
                                 scenario = compare_spec$scenario)
  
  # helper function that processes each climate situation
  run_scenario <- function(year, scenario) {
    
    run_crop_tf <- function(plot_data_list, diff) {
      
      plot_month_wrapper <- function (preds, mon_name, cropped) {
        
        # generating title 
        scenario_string <- ifelse((scenario != "PRESENT"), 
                                  paste(scenario, year), 
                                  scenario)
        desc_string <- ifelse(diff, 
                              "Change in Calanus Prescence Probability",
                              "Calanus Presence Probability")
        title <- paste(v, scenario_string, desc_string, "-", mon_name)
        
        plot_month(preds, 
                   title = title, 
                   diff = diff, 
                   cropped = cropped, 
                   downsample = downsample)
      }
      
      for (cropped in c(TRUE, FALSE)) {
        # generating save name
        filename <- paste0("plot",
                           ifelse(cropped, "_cropped", ""),
                           ifelse(diff, 
                                  paste0("_diff", comparison_scenario), ""), 
                           ".pdf")
        
        # creating a list of monthly plots and saving 
        plot_data_list |>
          purrr::imap(~plot_month_wrapper(.x, .y, cropped)) |>
          save_plots(v, year, scenario, filename)
      }
    }
    
    # plotting comparison scenario
    if ((compare_spec$year == year | 
         (is.na(compare_spec$year) & is.na(year))) && 
        compare_spec$scenario == scenario) {
      
      preds_list <- comparison_preds
      comparison_list <- NULL
    } else {
      
      preds_list <- read_preds(v, year, scenario)
      comparison_list <- comparison_preds
    }
    
    # running for original 
    preds_list |>
      run_crop_tf(diff = FALSE)
    
    # for comparison plots, subtract comparison data from original
    if (!is.null(comparison_list)) {
      purrr::map2(preds_list, 
                  comparison_list, 
                  ~mutate(.x, .pred_1 = .pred_1 - .y$.pred_1)) |>
        run_crop_tf(diff = TRUE)
    }
    
    TRUE
  }
  
  # plotting predictions
  climate_table(plot_scenarios) |>
    mutate(success = run_scenario(year, scenario))
}

get_threshold_plots <- function(v = "v3.00", 
                                plot_scenarios = 1:4,
                                threshold = .5,
                                comparison_scenario = 5,
                                downsample = c(0, 1, 2, 3)[3]) {
  
  # retrieving comparison predictions 
  compare_spec <- climate_table(comparison_scenario)
  comparison_preds <- read_preds(v, 
                                 year = compare_spec$year,
                                 scenario = compare_spec$scenario)
  
  # naming factor levels 
  feedstatus <- list(FALSE_FALSE = "No Feed",
                     FALSE_TRUE = "New Feed", 
                     TRUE_FALSE = "Lost Feed", 
                     TRUE_TRUE = "Feed")
  # palette for colors 
  pal <- c(FALSE_FALSE = "gray90",
           FALSE_TRUE = "red3", 
           TRUE_FALSE = "dodgerblue2", 
           TRUE_TRUE = "goldenrod1")
  
  # helper function that generates data for each scenario and generates plots
  run_scenario <- function(year, scenario) {
    
    # helper to create set of monthly plots and save to file
    run_cropped <- function(cropped) {
      
      # helper to generate plot
      feed_plot <- function (climate_data, mon_name, cropped) {
        if (cropped) {
          xlim <- c(-77.0, -42.5) 
          ylim <- c(36.5,  56.7)
          cex <- c(.5, .5, .8, 1)[downsample+1]
        } else {
          xlim <- ylim <- NULL 
          cex <- c(.17, .17, .3, .4)[downsample+1]
        }
        
        # generating title 
        main <- paste(v, scenario, year, 
                      "Feeding Habitat Change -", mon_name,
                      paste0("(Threshold: ", threshold, ")"))
        
        # generating plot
        ggplot(climate_data, aes(x = lon, y = lat, col = REL_PRESENCE)) +
          geom_point(cex = cex, pch = 15) +
          scale_color_manual(labels = feedstatus,
                             values = pal) + 
          coord_quickmap(xlim = xlim,
                         ylim = ylim) + 
          theme_bw() +
          theme(panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                legend.position = "bottom") +
          guides(colour = guide_legend(override.aes = list(size=2))) +
          labs(x = "Latitude", y = "Longtitude", color = "Feed Status") +
          ggtitle(main)
      }
      
      # naming save file
      filename <- paste0("feedplot", 
                         threshold, 
                         ifelse(cropped, "_cropped", ""), 
                         ".pdf")
      # saving to pdf
      plot_data_list |>
        imap(~feed_plot(.x, .y, cropped)) |>
        save_plots(v, year, scenario, filename)
      
      TRUE
    }
    
    # try and read in current predictions
    preds_list <- read_preds(v, year, scenario)
    
    plot_data_list <-  
      purrr::map2(preds_list, 
                  comparison_preds, 
                  ~mutate(.x, 
                          REL_PRESENCE = paste0(.y$.pred_1 > threshold,
                                                "_",
                                                .x$.pred_1 > threshold)))
    
    c(TRUE, FALSE) |>
      lapply(run_cropped)
    
    TRUE
  }
  
  # plotting predictions
  climate_table(plot_scenarios) |>
    mutate(success = run_scenario(year, scenario))
}

compare_versions <- function(old_v,
                             new_v, 
                             year = NA, 
                             scenario = "PRESENT",
                             downsample = c(0, 1, 2, 3)[3]) {
  
  old_v_preds <- read_preds(old_v, year, scenario)
  new_v_preds <- read_preds(new_v, year, scenario)
  
  plottable_preds <- purrr::map2(new_v_preds, 
                                 old_v_preds, 
                                 ~mutate(.x, .pred_1 = .pred_1 - .y$.pred_1))
  
  plottable_preds |>
    purrr::imap(~plot_month(.x, 
                            title = paste(new_v, "vs.", old_v, "-", .y),
                            diff = TRUE, cropped = FALSE, downsample = 3)) |>
    save_plots(new_v, year, scenario, paste0(new_v, "_vs_", old_v, ".pdf"))
}

# old code
if (FALSE) {
  # non-ggplot implementation of plot_month
  plot_month <- function (mon_pred, title, diff, cropped, downsample) {
    
    # converting to sf
    mon_pred <- mon_pred |>
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    # cropped plot has set bounds and larger point sizes
    if (cropped) {
      xlim <- c(-77.0, -42.5) 
      ylim <- c(36.5,  56.7)
      cex <- c(.5, .5, .8, 1)[downsample+1]
    } else {
      xlim <- ylim <- NULL 
      cex <- c(.17, .17, .3, .4)[downsample+1]
    }
    
    # generating separate color/scale formatting for difference plots
    ncol <- 31
    if (diff) {
      breaks <- seq(-.8, .8, length.out = ncol+1)
      pal <- colorRampPalette(c("midnightblue", #cool end
                                "dodgerblue3",
                                "deepskyblue1",
                                "gray94", 
                                "goldenrod1",
                                "darkorange1", 
                                "orangered3"))(ncol) #warm end
    } else { #raw values
      breaks <- seq(0, 1, length.out = ncol+1)
      pal <- inferno(ncol)
    }
    
    # generating plot
    plot(mon_pred['.pred_1'], 
         xlim = xlim,
         ylim = ylim,
         breaks = breaks,
         pal = pal,
         pch = 15,
         cex = cex,
         axes = TRUE,
         main = title)
  }
}
