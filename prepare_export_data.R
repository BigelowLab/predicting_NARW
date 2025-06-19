source("setup.R")
source("plots.R")

cfin <- read_preds("v6.01", NULL, "PRESENT", 1:12, quantile = TRUE)
chyp <- read_preds("v6.03", NULL, "PRESENT", 1:12, quantile = TRUE)

# Create combined predictions across all quantiles
quantiles <- colnames(cfin[[1]])[1:7]
combined <- get_combined_data(cfin, chyp, quantile = quantiles)

# Save to file alongside v6.01 because why not
root <- pred_path("v6.01", NA, "PRESENT")
save_month <- function(mon) {
  filename <- file.path(root, paste0("cfinchyp_quant_preds_", mon, ".csv.gz"))
  readr::write_csv(combined[[mon]], filename) 
}
1:12 |> map(save_month)