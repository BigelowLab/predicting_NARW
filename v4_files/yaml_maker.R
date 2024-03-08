v <- "v4.02.04"
interval <- "mon"
overwrite <- TRUE

# training data 
strata = "patch"
brickman_data_config <- list(interval = interval,
                             vars = c("Bathy_depth", "MLD", "SST", "Sbtm", "SSS",
                                      "Tbtm", "U", "V"),
                             transform = c("Vel = sqrt(U^2 + V^2)",
                                           "Bathy_depth = log10(Bathy_depth + 1)",
                                           "step_normalize()")) 
species_data_config <- list(source = c("azmp", "ecomon"), 
                            species = "C. finmarchicus", 
                            staged = FALSE,
                            base = 40000*195,
                            slope = 1000*195,
                            date_range = list(start =  "1990-01-01",
                                              end = "2015-12-31"),
                            vertical_correction = FALSE)
# model
seed <- 700
model_split <- FALSE

# engines
gam <- list(name = "GAM",
            engine = "mgcv",
            select_features = TRUE, 
            adjust_deg_free = 1, 
            k = c(18, 24, 25, 18, 18, 23, 20))

lg <- list(name = "Logistic Regression", 
           engine = "glm", 
           family = "binomial(link = logit)")

rf <- list(name = "Random Forest", 
           engine = "ranger",
           trees = 500,
           mtry = 5, 
           min_n = 33, 
           importance = "impurity")

brt <- list(name = "Boosted Regression Tree", 
            engine = "xgboost", 
            trees = 1000,
            tree_depth = 15,
            learn_rate = .000132,
            min_n = 13,
            mtry = 4,
            sample_size = .667,
            loss_reduction = .0000725)

mlp <- list(name = "MLP Neural Network",
            engine = "keras",
            hidden_units = 300,
            dropout = .75, 
            epochs = 25)

model_types <- list(brt)

### ASSEMBLY #############################################

training_data_config <- list(strata = strata, 
                             species_data = species_data_config, 
                             brickman_data = brickman_data_config)

model_config <- list(seed = seed,
                     model_split = model_split,
                     model_list = model_types)

config <- list(version = v, 
               training_data = training_data_config, 
               model = model_config)

### WRITING TO FILE
write_config(config, overwrite = overwrite)

