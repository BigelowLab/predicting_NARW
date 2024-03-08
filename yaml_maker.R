source("setup.R")

v <- "v2.20"
interval <- "mon"
overwrite <- FALSE

# training data 
strata = "patch"
brickman_data <- list(interval = interval,
                      vars = c("Bathy_depth", "MLD", "SST", "SSS", 
                               "Tbtm", "Sbtm", "U", "V"),
                      transform = "vel = sqrt(U^2 + V^2)")

species_data <- list(source = c("ecomon", "azmp"),
                     species = "C. Hyperboreus", 
                     staged = FALSE,
                     threshold = 5000,
                     date_range = list(start =  "1990-01-01",
                                       end = "2015-12-31"))
# model
seed <- 412
model_split <- FALSE

# engines
gam <- list(name = "GAM",
            engine = "mgcv",
            select_features = TRUE, 
            adjust_deg_free = 1, 
            k = c(18, 24, 25, 18, 18, 23, 20))

lg <- list(name = "Logistic Regression", 
           engine = "glm")

rf <- list(name = "random forest", 
           trees = 1000, 
           engine = "ranger")

brt <- list(name = "Boosted Regression Tree", 
            engine = "xgboost", 
            trees = 25.0,
            tree_depth = 7.0,
            learn_rate = 0.3,
            min_n = 1.0,
            loss_reduction = 0.04)

mlp <- list(name = "MLP Neural Network",
            engine = "keras",
            hidden_units = 300,
            dropout = .75, 
            epochs = 25)

model_list <- list(brt)

### ASSEMBLY #############################################

training_data <- list(strata = strata, 
                      species_data = species_data, 
                      brickman_data = brickman_data)

model <- list(seed = seed,
              model_split = model_split,
              model_list = model_list)

config <- list(version = v, 
               training_data = training_data, 
               model = model)

### WRITING TO FILE
write_config(config, overwrite = overwrite)

