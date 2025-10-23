v <- "v6.01.02"
overwrite <- TRUE

# training data 
strata = "patch"
brickman_data_config <- list(interval = "mon",
                             vars = c("Bathy_depth", "SST", "Sbtm", "MLD", "SSS",
                                      "Tbtm", "U", "V", "lat"), #####
                             transform = c("Vel = sqrt(U^2 + V^2)",
                                           "Bathy_depth = log10(Bathy_depth + 1)",
                                           "step_normalize()")) 
species_data_config <- list(source = c("azmp", "ecomon"), 
                            species = "C. finmarchicus", #####
                            staged = FALSE,
                            threshold = list(pre = "flat_tm(30000*195)",
                                             post = NULL),#"stephane_final(state_val = 'rest')"), #######
                            date_range = list(start =  "1999-01-01",
                                              end = "2015-12-31"),
                            vertical_correction = TRUE) #####
# model
seed <- 800
model_split <- FALSE

# engines
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
            trees = 500,
            learn_rate = .1,
            tree_depth = 4,
            mtry = 5,
            min_n = 10)

mlp <- list(name = "MLP Neural Network",
            engine = "keras",
            hidden_units = 5,
            penalty = .01, 
            epochs = 20,
            activation = "relu")

model_types <- list(brt) ####

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

