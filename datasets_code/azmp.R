suppressPackageStartupMessages({
  library(calanusthreshold)
  library(dplyr)
  library(readr)
  library(ncdf4)
})

#function to save patch data to file system
save_calanus <- function() {
  gsts_filename <- file.path("/mnt/ecocast/projects/calanus/calanus-threshold/data",
                             "GSTS_Calanus_consolidated.csv")
  lump_var <- "Calanus finmarchicus"
  calanus <- readr::read_csv(gsts_filename, show_col_types = FALSE) |>
    #lumping calanus 
    calanusthreshold::prep_dataset(
      lump_var = lump_var,
      drop_var = c(),
      threshold = 10000,
      complete_cases_only = FALSE) |>
    dplyr::select('patch', 'longitude', 'latitude') |>
    readr::write_csv(
      file = "/mnt/ecocast/projectdata/students/ojohnson/brickman/calanus/cfin_patch_10000.csv.gz")
}
