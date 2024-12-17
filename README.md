
# Predicting novel *Eubalaena glacialis* foraging habitat under future climate scenarios in the Northwest Atlantic 

*Codebase used to develop models presented in Johnson et al. manuscript. Runs within the Bigelow Laboratory of Ocean Sciences' `ecocast` server.*

The critically endangered North Atlantic right whale (Eubalaena glacialis) faces significant anthropogenic mortalities. Recent climatic shifts in traditional habitats have caused abrupt changes in right whale distributions, challenging traditional conservation strategies. Tools that can help anticipate new areas where E. glacialis might forage could inform proactive management. In this study, we trained boosted regression tree algorithms with fine-resolution modeled environmental covariates to build Calanus species-specific models of historical and future E. glacialis foraging habitat distributions on the northwest Atlantic shelf, from the Mid-Atlantic Bight to the Labrador Shelf. We determined foraging suitability using E. glacialis foraging thresholds for Calanus spp. adjusted by a bathymetry-dependent bioenergetic correction factor based on known foraging behavior constraints. Models were then projected to 2046-2065 and 2066-2085 modeled climatologies for representative concentration pathway (RCP) 4.5 and 8.5 scenarios with the goal of identifying potential foraging habitat shifts. The models had generally high performance (AUC > 0.9) and indicated bottom conditions and bathymetry as important covariates. Historical (1990-2015) projections aligned with known areas of high foraging habitat suitability as well as potential suitable areas on the Labrador Shelf. Future projections suggested that potential foraging habitat suitability would decrease in parts of the Gulf of Maine and Southwestern Gulf of Saint Lawrence, while potential habitat would be maintained or improved in the Western Scotian Shelf, Bay of Fundy, Newfoundland and Labrador Shelves, and along some continental shelf breaks. Overall, suitable habitat is projected to decline. Directing some survey efforts towards emerging potential foraging habitats can enable conservation management to anticipate the type of distribution shifts that have led to high mortality in the past. 

## Version 6 

**v6.01 - v6.03**

*5/27/2023 - Present*

### Changes

* **Final tuning parameters and corrections for publishable manuscript**
* Revisions to plot code, color schemes, and layouts
* Use of corrected *C. hyperboreus* dataset

### Example Plots

**Combined *Calanus* spp. present-day tau-h-patch probability, v6.03**

<img src = "readme_imgs/image1.png" width = "60%" alt = "Raw present predictions">

**Combined *Calanus* spp. habitat shift maps, RCP 8.5 2075 vs. present-day, v6.03**

<img src = "readme_imgs/image2.png" width = "60%" alt = "Habitat shift maps">
