setwd("/mnt/ecocast/projects/students/ojohnson/brickman/v2")
source("create_version.R")

model_file <- "boosted_trees.R"
source(file.path("v2/model_types", model_file))

# MAKE SURE YAML FILE IS ACCURATE
source("yaml_maker.R")

workflow <- workflow() |>
  add_recipe(recipe) |>
  add_model(model_spec)

# v and separate_month_models are defined by yaml_maker.R

#  scenario  year
# 1 RCP45     2055
# 2 RCP45     2075
# 3 RCP85     2055
# 4 RCP85     2075
# 5 PRESENT     NA

create_separate_models(v, data, workflow)
fitted <- create_version(v, data_split, workflow, overwrite = TRUE) 

plot_version_var_contribution(v)
if(interval == "mon") {
  plot_version_var_contribution_monthly(v)
  var_contribution_vel_mon(v)
}

plots_predictions(v, 
                  downsample = 2,
                  save_months = 1:12,
                  scenarios = c(4, 5),
                  verbose = TRUE)

feeding_habitats(v, 
                 plot_scenarios = 4, 
                 threshold = .5,
                 downsample = 2,
                 verbose = TRUE)
