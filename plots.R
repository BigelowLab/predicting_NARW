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
    message(paste("Predictions for", year, scenario, 
                  "don't exist. Skipping plots."))
    return(NULL)
  }
  
  # retrieving list of all prediction files in a prediction folder
  pred_files <- list.files(folder)
  pred_files <- pred_files[startsWith(pred_files, "predictions")]
  
  # retrieving data from each file
  data.frame(file = pred_files) |>
    rowwise() |>
    transmute(data = list(readr::read_csv(file.path(folder, file), 
                                          col_types = readr::cols()) |> 
                            st_as_sf(coords = c("lon", "lat"), crs = 4326)),
              # use NA for month if not monthly data
              mon = ifelse("month" %in% colnames(data), 
                           data$month[[1]],
                           NA)) |>
    # sorting in ascending month order 
    arrange(mon) 
}

#' Generates a plot of the desired data 
#' 
#' @param climate_data df, the data to be plotted
#' @param v chr, the version to be plotted
#' @param mon int, the month of the data being plotted - NA if annual data
#' @param year int, the year of the given data
#' @param scenario chr, the scenario for the given data 
#' @param cropped lgl, whether the plot is cropped to the GoM/BoF area
#' @param difference lgl, whether the plot depicts difference from present or
#'   raw probabilities.
#' @param downsample int, the resolution of the plot
#' @return a plot
create_plot <- function (climate_data, 
                         v,
                         mon = NA,
                         year=c(2055, 2075)[1], 
                         scenario=c("RCP45", "RCP85")[1], 
                         cropped = c(TRUE, FALSE)[1], 
                         difference = c(TRUE, FALSE)[2],
                         downsample = c(0, 1, 2, 3)[1]) {
  
  cropped_cex_vec <- c(.5, .5, .8, 1)
  uncropped_cex_vec <- c(.17, .17, .3, .4)
  
  # generating title 
  scenario_string <- ifelse((scenario != "PRESENT"), 
                            paste(scenario, year), 
                            scenario)
  desc_string <- ifelse(difference, 
                        "Change in Calanus Prescence Probability",
                        "Calanus Presence Probability")
  main <- paste(v, scenario_string, desc_string, as_month(mon))
  
  
  # cropped plot has set bounds and larger point sizes
  if (cropped) {
    xlim <- c(-77.0, -42.5) 
    ylim <- c(36.5,  56.7)
    cex <- cropped_cex_vec[downsample+1]
  } else {
    xlim <- ylim <- NULL 
    cex <- uncropped_cex_vec[downsample+1]
  }
  
  # generating separate color/scale formatting for difference plots
  ncol <- 31
  if (difference) {
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
  plot(climate_data['.pred_1'], 
       xlim = xlim,
       ylim = ylim,
       breaks = breaks,
       pal = pal,
       pch=15,
       cex=cex,
       axes=TRUE,
       main = main)
}

#' Generates and saves a cropped and uncropped plot of the desired data
#' 
#' @param df df, climate data to be plotted and saved with 
#'   "data" and "mon" columns
#' @param v chr, the version that generated the data
#' @param scenario chr, the climate scenario being plotted
#' @param difference lgl, whether the data is change from present or raw values
#' @param downsample int, the resolution of the plot
#' @return the original data frame. Also creates uncropped and cropped plots
#'   of the data and saves to file. 
plot_climate <- function(df, v, year, scenario, difference, downsample) {
  
  # helper function
  plot_pred_wrapper <- function(row) {
    create_plot(row$data, v, row$mon, year, 
                scenario, cropped, difference, downsample)
  }
  
  for (cropped in c(TRUE, FALSE)) {
    # generating plot path name
    plot_path <- paste(c(pred_path(v, year, scenario, "plot"),
                         ifelse(cropped, "_cropped", ""),
                         ifelse(difference, "_diff", ""), 
                         ".pdf"), 
                       collapse = "")
    
    #opening a pdf and plotting each month
    pdf(plot_path)
    apply(df, 1, plot_pred_wrapper)
    dev.off()
  }
  
  TRUE
}

### MAIN FUNCTION ##########

#' Generates plots for a version
#' 
#' @param v, chr, the version to be plotted
#' @param plot_scenarios numeric vector, a list of integers btwn 1 and 5 
#'   indicating which climate scenarios should be plotted. If predictions
#'   don't exist for a desired scenario, a message is displayed and those
#'   plots are skipped. 
#' @return tibble of climate scenarios and whether plotting executed 
#'   successfully 
get_plots <- function(v = "v1.00", 
                      plot_scenarios = 1:5,
                      downsample = c(0, 1, 2, 3)[1]) {
  
  # retrieving present prediction
  present <- read_preds(v, scenario = "PRESENT")
  if(is.null(present)) {stop("Have not yet generated present predictions")}
  
  # helper function that processes each climate situation
  get_plots_help <- function(year, scenario) {
    
    if (scenario == "PRESENT") {
      plot_climate(present, v, year, scenario, FALSE, downsample)
      return(TRUE)
    }
    
    # try and read in current predictions
    preds <- read_preds(v, year, scenario)
    if(is.null(preds)) { return(FALSE) }
    
    # plot raw predictions
    preds |>
       plot_climate(v, year, scenario, difference = FALSE, downsample)
    
    # transforming data into "change in probability" and plotting
    preds |>
      bind_cols(select(present, data)) |> 
      transmute(mon,
                data = list(mutate(data...1, # current data 
                                   .pred_1 = .pred_1 - data...3$.pred_1))) |>
      plot_climate(v, year, scenario, difference = TRUE, downsample)
    
    TRUE
  }
  
  # plotting predictions
  climate_table(plot_scenarios) |>
    mutate(success = get_plots_help(year, scenario))
}

feeding_habitats <- function(v = "v1.00", 
                             plot_scenarios = 1:4,
                             threshold = .5,
                             downsample = c(0, 1, 2, 3)[3],
                             verbose = TRUE) {
  
  # retrieving present prediction
  present <- read_preds(v, scenario = "PRESENT")
  if(is.null(present)) {stop("Have not yet generated present predictions")}
  present <- present |>
    transmute(pdata = list(transmute(data, PFEED = .pred_1 >= threshold)))
  
  # naming factor levels 
  feedstatus <- list(FALSEFALSE = "No Feed",
                     FALSETRUE = "New Feed", 
                     TRUEFALSE = "Lost Feed", 
                     TRUETRUE = "Feed")
  
  # helper function that generates data for each scenario and generates plots
  get_plots_help <- function(year, scenario) {
    
    # helper to create set of monthly plots and save to file
    save_plots <- function(cropped) {
      
      # helper to generate plot
      feed_plot <- function (climate_data, 
                             mon) {
        
        cropped_cex_vec <- c(.5, .5, 1, 1.3)
        uncropped_cex_vec <- c(.17, .17, .25, .35)
        pal <- c("No Feed" = "gray90", 
                 "Feed" = "goldenrod1",
                 "New Feed" = "orangered3",
                 "Lost Feed" = "dodgerblue3")
        
        # generating title 
        main <- paste(v, scenario, year, 
                      "Feeding Habitat Change", as_month(mon),
                      paste0("(Threshold: ", threshold, ")"))
        
        # cropped plot has set bounds and larger point sizes
        cex <- ifelse(cropped,
                      cropped_cex_vec[downsample+1],
                      uncropped_cex_vec[downsample + 1])
        
        # generating plot
        tplot <- ggplot(climate_data) +
          theme_bw() +
          geom_sf(aes(col = FEED), cex = cex, pch = 15) +
          ggtitle(main) + 
          scale_color_manual(values = pal) + 
          theme(panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                legend.position = "bottom") +
          guides(colour = guide_legend(override.aes = list(size=2))) +
          labs(col = "Feed Status")
        
        if (cropped) {
          tplot <- tplot +
            xlim(-77.0, -42.5) +
            ylim(36.5,  56.7)
        }
        
        tplot
      }
      
      if (verbose) {message(paste("Plotting", year, scenario, 
                                  "cropped =", cropped))}
      
      # naming save file
      filename <- paste0("feedplot", 
                         threshold, 
                         ifelse(cropped, "_cropped", ""), 
                         ".pdf")
      # opening pdf
      pdf(pred_path(v, year, scenario, filename))
      apply(preds, 1, 
            function(x) {print(feed_plot(x$data, x$mon))})
      dev.off()
      
      TRUE
    }
    
    # try and read in current predictions
    preds <- read_preds(v, year, scenario)
    if(is.null(preds)) { return(FALSE) }
    preds <- preds |>
      mutate(data = list(transmute(data, FFEED = .pred_1 >= threshold))) |>
      bind_cols(present) |>
      rowwise() |>
      transmute(mon, 
                data = data |>
                  bind_cols(PFEED = pdata$PFEED) |>
                  rowwise() |>
                  transmute(FEED = feedstatus[[paste0(PFEED, FFEED)]]) |>
                  list())
    
    c(TRUE, FALSE) |>
      lapply(save_plots)
    
    TRUE
  }
  
  # plotting predictions
  climate_table(plot_scenarios) |>
    mutate(success = get_plots_help(year, scenario))
}

##############################################

# old code that purely processed annual data
if (FALSE) {
  #' Generates and saves a plot of the desired data to file.
  #' 
  #' @param v the version to be plotted
  #' @param climate_data the data to be plotted
  #' @param year the year of the given data
  #' @param scenario the scenario for the given data 
  #' @param cropped whether the plot is cropped to the Gulf of Maine
  #' @param difference whether the plot depicts difference from present or
  #'   raw probabilities.
  #' @return a plot saved as a pdf to the file system.
  plot_pred <- function (v,
                         climate_data, 
                         year=c(2055, 2075)[1], 
                         scenario=c("RCP45", "RCP85")[1], 
                         cropped = c(TRUE, FALSE)[1], 
                         difference = c(TRUE, FALSE)[2]) {
    # generating plot name
    plot_path <- pred_path(v, year, scenario, "plot")
    if (cropped) {plot_path <- paste0(plot_path, "_cropped")}
    if (difference) {plot_path <- paste0(plot_path, "_diff")}
    plot_path <- paste0(plot_path, ".pdf")
    
    # cropped plot has set bounds and larger point sizes
    if (cropped) {
      xlim <- c(-77.0, -42.5) 
      ylim <- c(36.5,  56.7)
      cex <- .5
    } else {
      xlim <- ylim <- NULL 
      cex <- .2
    }
    
    title <- ifelse((scenario != "PRESENT"), paste(scenario, year), scenario)
    
    # generating separate titles and color schemes for difference plots
    ncol <- 31
    if (difference) {
      breaks <- seq(-.5, .5, length.out = ncol+1)
      pal <- colorRampPalette(c("midnightblue", #cool end
                                "dodgerblue3",
                                "deepskyblue1",
                                "gray94", 
                                "goldenrod1",
                                "darkorange1", 
                                "orangered3"))(ncol) #warm end
      main <- paste(v, title, "Change in Calanus Presence Probability")
    } else { #raw values
      breaks <- seq(0, 1, length.out = ncol+1)
      pal <- inferno(ncol)
      main <- paste(v, title, "Calanus Presence Probability")
    }
    
    # generating plot
    pdf(plot_path)
    plot(climate_data['.pred_1'], 
         xlim = xlim,
         ylim = ylim,
         breaks = breaks,
         pal = pal,
         pch=15,
         cex=cex,
         axes=TRUE,
         main = main)
    dev.off()
  }
  
  test2 <- brickman::read_brickman(scenario="PRESENT", 
                                  vars = c("Bathy_depth"), 
                                  interval = "ann", 
                                  form = "sf")
  
  #' Generates four plots for a given climate situation.
  #'
  #' @param version the version being plotted
  #' @param year the climate year
  #' @param scenario the climate scenario 
  #' @param climate_pred the data to be plotted 
  #' @param present_pred present climate data to use for comparison
  #' @return saves four plots (all combinations of cropped and difference) 
  #'   as pdfs and returns TRUE if code ran without error
  plot_climate <- function(version, year, scenario, climate_pred, present_pred) {
    # helper function -- given difference and climate data, calls plot_pred
    iterate_cropped <- function(difference, climate_data) {
      for (cropped in c(TRUE, FALSE)) {
        plot_pred(version, 
                  climate_data, 
                  year, 
                  scenario, 
                  cropped = cropped, 
                  difference = difference)
      }
    }
    
    #iterate_cropped(FALSE, climate_pred)
    if (scenario != "PRESENT") {
      iterate_cropped(TRUE, mutate(climate_pred, 
                                   .pred_1 = .pred_1 - present_pred$.pred_1))
    }
    return(TRUE)
  }
  
  ### MAIN FUNCTION
  
  #' Generates plots for the given model predictions
  #' 
  #' @param version the version to be plotted
  #' @param pred_table a table of predictions to be plotted. If NULL, predictions
  #'   are read directly from file system. 
  #' @return saves plots to the file system and returns TRUE if code ran without
  #'  erroring
  get_plots <- function(version="v1.00", pred_table = NULL) {
    #generating table of prediction data if one isn't provided
    pred_table <- climate_table() |> 
      mutate(climate_pred = list(readr::read_csv(
        pred_path(version, year, scenario, "predictions.csv.gz")) |>
          st_as_sf(coords = c("lon", "lat"), crs = 4326)))

    #retrieving present climate predictions
    present_pred <- tail(pred_table, n = 1)$climate_pred[[1]]
    #plotting predictions
    pred_table |>
      mutate(plotted = plot_climate(version, 
                                    year, 
                                    scenario, 
                                    climate_pred, 
                                    present_pred))
    return(TRUE)
  }
}










