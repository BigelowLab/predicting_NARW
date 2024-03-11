setwd("/mnt/ecocast/projects/students/ojohnson/brickman")
source("v2/v2_setup.R")
library(mgcv)

gam_formula <- "patch ~ s(Bathy_depth, k = 18) + s(SSS, k = 25) + 
s(SST, k = 24) + s(MLD, k = 10) + s(U, k = 23) + s(V, k = 20) + 
s(Sbtm, k = 18) + s(Tbtm, k = 18) + month"

monthly_gam_formula <- "patch ~ s(Bathy_depth, k = 18) + s(SSS, k = 25) + 
s(SST, k = 24) + s(MLD, k = 10) + s(U, k = 23) + s(V, k = 20) + 
s(Sbtm, k = 18) + s(Tbtm, k = 18)"

gam <- gen_additive_mod() |>
  set_mode("classification") |>
  set_engine("mgcv", method = "REML") |>
  set_args(select_features = TRUE, adjust_deg_free = 1)

workflow <- workflow() |>
  add_recipe(recipe) |>
  add_model(gam, 
            formula = monthly_gam_formula |> as.formula())

### ONE MODEL CHECKING
workflow |> 
  fit(training) |>
  extract_fit_engine() |>
  gam.check()

#### SEPARATE MODEL BY MONTH CHECKING
set.seed(490)
months <- split(data, data$month) |>
  lapply(initial_split, strata = patch) |>
  lapply(function(s) { workflow |> 
      fit(training(s)) |>
      extract_fit_engine() |> 
      gam.check() })
set.seed(NULL)

# plot gam residuals 
pdf(v_path(v, "model", "smooths.pdf"))
for (month in 1:12) {
  par(mfrow=c(2,2))
  calanus_wkf[[month]] |> 
    extract_fit_engine() |>
    plot(residuals = TRUE, 
         pch = 1,
         cex = .8,
         shade = TRUE,
         shade.col = "pink",
         main = paste("Variable smooths", as_month(month)))
  }
dev.off()

# plotting "most" contributing variable
plot_varContribution <- function(v) {
  calanus_wkf <- readRDS(v_path(v, "model", "model_fit.csv.gz"))
  
  recipe <-
    recipe(patch ~ ., data = training) |>
    update_role(lon, lat, month, new_role = "ID") |>
    step_normalize(all_numeric_predictors())
  
  vars <- read_config(v)$training_data$brickman_data$vars
  v_mon <- vars %in% mon_vars()
 
  #defining overall variables
  static <- brickman::read_brickman(scenario = "PRESENT", 
                                    vars = vars[!v_mon], 
                                    interval = "ann")
  brickman <- brickman::read_brickman(scenario="PRESENT", 
                                      vars = vars[v_mon], 
                                      interval = "mon")
  
  # METHOD
  plot_month <- function(mon) {
    #defining monthly variables
    mon_gam <- calanus_wkf[[mon]] |> extract_fit_engine()
    mon_pred <- c(brickman |> dplyr::slice(month, mon),
                  static) |>
      st_downsample(n = 5) |>
      as_tibble() |>
      rename(lon = x, lat = y) |>
      na.omit()
    
    # acquiring data to plot
    terms <- predict(mon_gam, 
                     bake(recipe |> prep(), mon_pred), 
                     type="terms",
                     newdata.guaranteed=T) |>
      as.data.frame() |>
      bind_cols(dplyr::select(mon_pred, lon, lat))
    
    # removing s() from colnames
    colnames(terms) <- colnames(terms) |>
      gsub(pattern="s(", replacement="", fixed=T) |>
      gsub(pattern=")", replacement="", fixed=T)
    
    # mutating terms to contain whichmax and whichmin columns
    terms <- terms |>
      mutate(whichmax = colnames(terms)[
               max.col(dplyr::select(terms, Bathy_depth:Tbtm), 
                       ties.method = "first")] |>
               as.factor(),
             whichmin = colnames(terms)[
               max.col(-dplyr::select(terms, Bathy_depth:Tbtm),
                       ties.method="first")] |>
               as.factor())
    
    # plotting
    colors = c("Bathy_depth" = "#440154", "MLD" = "thistle3",
               "SST" = "#1F908B", "SSS" = "#ff59ae",
               "Tbtm" = "#5CC862", "Sbtm" = "brown3",
               "V" = "#FCE724", "U" = "blue3")
    
    baseplot <- ggplot(terms) +
      theme_void() +
      coord_quickmap() +
      guides(col = guide_legend(override.aes = list(size = 3.5))) +
      labs(col = "Predictor") +
      scale_color_manual(values = colors)
    
    
    list(
      baseplot +
        geom_point(size = .08, mapping = aes(x=lon, y=lat, col=whichmax)), 
      baseplot + 
        geom_point(size = .08,  mapping = aes(x=lon, y=lat, col=whichmin)))
  }
  
  library(ggpubr)
  pdf(v_path(v, "model", "var_contribution.pdf"), 
      width = 8, height = 5)
  for (mon in 1:12) {
    plots <- plot_month(mon)
    ggarrange(plots[[1]], plots[[2]], 
              labels = list("Largest Positive Effect",
                            "Largest Negative Effect"),
              nrow=1, ncol = 2,
              common.legend = TRUE,
              legend = "bottom") |>
      annotate_figure(top = paste("Predictor Effects", as_month(mon))) |>
      print()
  }
  dev.off()
}





