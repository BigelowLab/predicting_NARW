library(tidyverse)
library(mgcv)
library(ggthemes)

# THIS CODE APPLIES THE VERTICAL CORRECTION TO THE C. FINMARCHICUS ECOMON DATASET

# choose CV-CVI or CIV_CVI
load(file = "datasets/vertical_correction/Cfin_CIV_CVIgamms3_4_remove01.RData")
# mod3 is the model with the month as factor effect (model 1 in ppt presentation)
summary(mod3)
# load ecomon data
ecomon <- ecomon::read_staged(species = "calfin", 
                              form = "tibble")

# add the variable with the SAME name as in the gamm. note that ID is not included and we predict without ID
ecomon <-  ecomon |>  
  mutate(Month = as.numeric(format(date, format="%m")),
         fMonth = as.factor(Month), #order of factors is the same
         percZ_stn = (tow_depth/sta_depth),
         Zstation = sta_depth)

old_ecomon <- ecomon

# to decide: what to do with station depth > 500m
# 1) remove (18 points removed)
ecomon <- ecomon |>  filter(sta_depth < 500)
# OR
# 2) predict as if station depth is 500m
# ecomon <-  ecomon %>%  mutate(Zstation=ifelse(Zstation > 500, 500, Zstation)) 

# decide if you need to correct or not
# Sorochan criteria
# > 200 m in EcoMon, and if the sampling depth was > 15 m above bottom

# my criteria: < 95% and > 15m above bottom. because stations sampled less 
#  than 200m not always sampled accurately. 
#  minimum depth in GAM is 40 so avoiding extrapolation.
ecomon <- ecomon |>  
  mutate(to_correct = percZ_stn < 95 & 
           (sta_depth - tow_depth) > 15 & 
           sta_depth > 40,
         CIV_CVI_m2 = c6_m2 + c5_m2 + c4_m2) # change to CV-CVI if necessary
                              
# predict and correct the data
ecomon <- ecomon |>  
  mutate(predicted_pcum = ifelse(to_correct, 
                                 predict(mod3, ecomon, type = "response", 
                                         exclude ='s(ID)', 
                                         newdata.guaranteed = T), 
                                 1), # it is 1 if no correction : CIV-CVI/1
         corrected_CIV_CVI_m2 = CIV_CVI_m2/predicted_pcum)

ecomon <- ecomon |>
  mutate(Month = factor(month.abb[Month], 
                        levels = month.abb))

result_plot <- ggplot(ecomon |> filter(to_correct), 
       aes(x = CIV_CVI_m2/10^6, y = corrected_CIV_CVI_m2/10^6)) +
  geom_abline() +
  geom_point(aes(col = sta_depth), alpha = .7) + #shape = to_correct,
  #facet_wrap(~Month) + 
  theme_bw() +
  scale_x_continuous( #trans = "log10", 
                     name = expression("Observed late-stage" ~ 
                       italic("C. finmarchicus") ~ "(ind ⋅"~10^-6~m^2~")")) +
  scale_y_continuous(#trans = "log10", 
    name = expression("Corrected late-stage" ~ 
                        italic("C. finmarchicus") ~ "(ind ⋅"~10^-6~m^2~")")) +
  labs(col = "Bathymetry (m)", shape = "Corrected") +
  scale_color_viridis_c(option = "turbo")
  #coord(xlim = c(0, 1.3), ylim = c(0, 1.3))

result_plot

# code to save result plot and ecomon dataset to file
if (FALSE) {
  root <- "/mnt/ecocast/projects/students/ojohnson/brickman/figure_files"
  
  pdf(file = file.path(root, "figure2_verticalcorrection.pdf"),
      width = 7, height = 5)
  print(result_plot)
  dev.off()
  
  readr::write_csv(ecomon, file.path(root, "vertcorr_cfin_ecomon.csv.gz"))
}
