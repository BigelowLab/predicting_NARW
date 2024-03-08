source("setup.R")
library(RColorBrewer)
library(viridis)
library(patchwork)
library(ggpubr)

##### HELPERS ##########

#' Retrieves prediction data from file for a given climate situation
#' 
#' @param v chr, the version being retrieved
#' @param year int, the year being retrieved
#' @param scenario chr, the scenario being retrieved
#' @return df, a table with a "data" column with prediction data from each
#'   prediction file in climate situation folder and "mon" column with month 
#'   for corresponding climate data - NA if annual data
read_preds <- function(v, year, scenario, save_months = 1:12) {
  
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
    setNames(mon_names())
  
  setwd(wd)
  preds[save_months]
}

#' ALTER THIS METHOD FOR ALTERNATIVE PLOT ARRANGEMENT
#' 
#' @param plots list, plot objects
#' @param path chr, file path to save to 
save_plots <- function(plots, v, year, scenario, filename, 
                       verbose = FALSE) {
  ppath <- pred_path(v, year, scenario, filename)
  if (verbose) {print(ppath)}
  
  pdf(ppath)
  print(plots)
  dev.off()
  
  TRUE
}

# grid 2x2 of plots
# INCOMPLETE: troubleshooting labels
save_plots_gridded <- function(v, year, scenario,
                               months = c("Jan", "Apr", "Jul", "Oct"),
                               downsample = 2, 
                               cex_override = .6) {
  
  test_plots <- read_preds(v, year, scenario)[months] |>
    #get_combined_preds("v4.02.03", "v4.04.01", year = year, scenario = scenario) |>
    map(~plot_month(.x, "", FALSE, TRUE, downsample,
                    cex_override = cex_override))
  
  winter <- test_plots[[months[1]]] + 
    theme(axis.ticks.x = element_blank(), 
          axis.text.x = element_blank()) +
    ggtitle(paste("a)", months[1]))
  
  spring <- test_plots[[months[2]]] + 
    theme(axis.ticks = element_blank(), 
          axis.text = element_blank()) +
    ggtitle(paste("b)", months[2]))
  
  summer <- test_plots[[months[3]]] +
    ggtitle(paste("c)", months[3]))
  
  fall <- test_plots[[months[4]]] +
    theme(axis.ticks.y = element_blank(), 
          axis.text.y = element_blank()) +
    ggtitle(paste("d)", months[4]))
  
  res <- (winter + spring + summer + fall +
            plot_layout(nrow = 2, ncol = 2, guides = "collect") &
            theme(legend.position = "bottom",
                  axis.title = element_blank())) +
    plot_annotation(paste(v, year, scenario, "gridded results"))
  
  pdf(pred_path(v, year, scenario, "gridded_plot_cropped.pdf"))
  print(res)
  dev.off()
} 

#' ALTER THIS METHOD TO CHANGE PLOT APPEARANCE
#' 
#' @param preds df, data to plot
#' @param title chr, the name of the plot
#' @param diff boolean, is this plot a difference plot?
#' @param cropped boolean, is this plot cropped?
#' @param downsample int, downsample factor 
#' @return plot of desired month 
plot_month <- function (preds, title, diff, cropped, downsample, 
                        cex_override = NULL) {
  
  if (cropped) {
    xlim <- c(-77.0, -42.5) 
    ylim <- c(36.5,  56.7)
    cex <- c(.5, .8, 1.3, 2)[downsample+1]
  } else {
    xlim <- ylim <- NULL 
    cex <- c(.17, .17, .3, .4)[downsample+1]
  }
  
  if (!is.null(cex_override)) {cex <- cex_override}
  
  # ggplot base
  plot_base <- ggplot(preds, aes(x = lon, y = lat, col = .pred_1)) +
    geom_point(cex = cex, pch = 15) +
    coord_quickmap(xlim = xlim,
                   ylim = ylim) + 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "bottom") +
    labs(x = "Latitude", y = "Longtitude", color = "Patch probability") +
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
                      save_months = 1:12,
                      downsample = c(0, 1, 2, 3)[1]) {
  
  # retrieving comparison predictions 
  compare_spec <- climate_table(comparison_scenario)
  comparison_preds <- read_preds(v, 
                                 year = compare_spec$year,
                                 scenario = compare_spec$scenario,
                                 save_months = save_months)
  
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
      
      preds_list <- read_preds(v, year, scenario, save_months = save_months)
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
  
  if (FALSE) {
    comparison_preds <- get_combined_preds(vcfin = "v4.02.03", vchyp = "v4.04.01", 
                                           year = NULL, scenario = "PRESENT")
    preds_list <- get_combined_preds(vcfin = "v4.02.03", vchyp = "v4.04.01", 
                                     year = 2075, scenario = "RCP85")
  }
  
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
          cex <- c(.5, .8, 1.3, 2)[downsample+1]
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
      if (FALSE) {
        filename = "COMBINEDLIST.pdf"
      }
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

gridded_threshold <- function(v = "v4.02.02", year, scenario,
                              comparison_scenario = 5,
                              threshold = .5,
                              downsample = c(0, 1, 2, 3)[3],
                              months = c("Jan", "Apr", "Jul", "Oct"), 
                              cex_override = .6) {
  
  # try and read in current predictions
  preds_list <- read_preds(v, year, scenario)[months]
  # retrieving comparison predictions 
  compare_spec <- climate_table(comparison_scenario)
  comparison_preds <- read_preds(v, 
                                 year = compare_spec$year,
                                 scenario = compare_spec$scenario)[months]
  if (FALSE) {
  comparison_preds <- get_combined_preds(vcfin = "v4.02.03", vchyp = "v4.04.01", 
                                   year = NULL, scenario = "PRESENT")
  preds_list <- get_combined_preds(vcfin = "v4.02.03", vchyp = "v4.04.01", 
                                   year = 2075, scenario = "RCP85")
  }
  plot_data_list <-  
    purrr::map2(preds_list, 
                comparison_preds, 
                ~mutate(.x, 
                        REL_PRESENCE = paste0(.y$.pred_1 > threshold, 
                                              "_", 
                                              .x$.pred_1 > threshold) |>
                          factor(levels = c("FALSE_FALSE", "FALSE_TRUE", 
                                            "TRUE_FALSE", "TRUE_TRUE"))))
  
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
  
  # helper to generate plot
  feed_plot <- function (climate_data, mon_name) {
    xlim <- c(-77.0, -42.5) 
    ylim <- c(36.5,  56.7)
    cex <- cex_override
    
    # generating plot
    ggplot(climate_data, aes(x = lon, y = lat, col = REL_PRESENCE)) +
      geom_point(cex = cex, pch = 15) +
      scale_color_manual(labels = feedstatus,
                         values = pal,
                         drop = FALSE) + 
      coord_quickmap(xlim = xlim,
                     ylim = ylim) + 
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            legend.position = "bottom") +
      guides(colour = guide_legend(override.aes = list(size=2))) +
      labs(x = "Latitude", y = "Longtitude", color = "Feed Status") +
      ggtitle(mon_name)
  }
  
  #####
  
  test_plots <- plot_data_list |>
    imap(~feed_plot(.x, .y))
  
  winter <- test_plots[[months[1]]] + 
    theme(axis.ticks.x = element_blank(), 
          axis.text.x = element_blank()) +
    ggtitle(paste("a)", months[1]))
  
  spring <- test_plots[[months[2]]] + 
    theme(axis.ticks = element_blank(), 
          axis.text = element_blank()) +
    ggtitle(paste("b)", months[2]))
  
  summer <- test_plots[[months[3]]] +
    ggtitle(paste("c)", months[3]))
  
  fall <- test_plots[[months[4]]] +
    theme(axis.ticks.y = element_blank(), 
          axis.text.y = element_blank()) +
    ggtitle(paste("d)", months[4]))
  
  res <- (winter + spring + summer + fall +
            plot_layout(nrow = 2, ncol = 2, guides = "collect") +
            plot_annotation(paste(v, year, scenario, 
                                  "(Threshold: ", threshold, ")"))) &
    theme(legend.position = "bottom",
          axis.title = element_blank())
  
  if (FALSE) {
  res <- (winter + spring + summer + fall +
            plot_layout(nrow = 2, ncol = 2, guides = "collect") +
            plot_annotation("COMBINED")) &
    theme(legend.position = "bottom",
          axis.title = element_blank())
  }
  
  pdf(pred_path(v, year, scenario, #"COMBINEDTHRESHOLD.pdf"))
                paste0("gridThreshold_", threshold, ".pdf")))
  print(res)
  dev.off()
}

compare_versions <- function(old_v,
                             new_v, 
                             year = NA, 
                             scenario = "PRESENT",
                             downsample = c(0, 1, 2, 3)[3],
                             cropped = FALSE) {
  
  old_v_preds <- read_preds(old_v, year, scenario)
  new_v_preds <- read_preds(new_v, year, scenario)
  
  plottable_preds <- purrr::map2(new_v_preds, 
                                 old_v_preds, 
                                 ~mutate(.x, .pred_1 = .pred_1 - .y$.pred_1))
  
  plottable_preds |>
    purrr::imap(~plot_month(.x, 
                            title = paste(new_v, "vs.", old_v, "-", .y),
                            diff = TRUE, cropped = cropped, downsample = 3)) |>
    save_plots(new_v, year, scenario, 
               paste0(new_v, "_vs_", old_v, "_", 
                      ifelse(cropped, "cropped", ""), ".pdf"))
}

combined_preds <- function(vcfin, vchyp,
                           downsample = 2,
                           year = 2075, 
                           scenario = "RCP85"){
  
  vcfin_preds <- read_preds(vcfin, year, scenario)
  vchyp_preds <- read_preds(vchyp, year, scenario)
  
  combined_preds <- 
    purrr::map2(vcfin_preds, vchyp_preds,
                ~mutate(.x, .pred_1 = 1 - (.x$.pred_0 * .y$.pred_0)))
  
  months <- combined_preds |>
    imap(~plot_month(.x, 
                     paste("Combined", vcfin, "and", vchyp, 
                           year, scenario, "-", .y),
                     diff = FALSE, cropped = FALSE, downsample = downsample))
  
  save_plots(months, vcfin, year, scenario, "combinedpreds.pdf",
             verbose = TRUE)
}

get_combined_preds <- function(vcfin, vchyp,
                           year = 2075, 
                           scenario = "RCP85"){
  
  vcfin_preds <- read_preds(vcfin, year, scenario)
  vchyp_preds <- read_preds(vchyp, year, scenario)
  
  purrr::map2(vcfin_preds, vchyp_preds,
              ~mutate(.x, .pred_1 = 1 - (.x$.pred_0 * .y$.pred_0)))
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