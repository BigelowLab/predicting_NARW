setwd("/mnt/ecocast/projects/students/ojohnson/brickman")
source("v2/v2_setup.R")

### MODEL DEFINITION

rf <- rand_forest(trees = 1000,
                  engine = "ranger", 
                  mode = "classification")

