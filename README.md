*Codebase used to develop models presented in Johnson et al. manuscript. Runs within the Bigelow Laboratory of Ocean Sciences' `ecocast` server.*

# Suitability of foraging habitat for *Eubalaena glacialis* under future climate scenarios in the Northwest Atlantic 

Omi H. Johnson<sup>1, 2</sup>, Stéphane Plourde<sup>3</sup>, Caroline Lehoux<sup>3</sup>, Camille H. Ross<sup>1, 4, 5</sup>, Benjamin Tupper<sup>1</sup>, Christopher D. Orphanides<sup>6</sup>, Harvey J. Walsh<sup>6</sup>, Nicholas R. Record<sup>1*</sup>

1. Tandy Center for Ocean Forecasting, Bigelow Laboratory for Ocean Sciences, East Boothbay, ME, USA.
2. Khoury College of Computer Science, Northeastern University, Boston, MA, USA.
3. Fisheries and Oceans Canada, Institut Maurice-Lamontagne, Mont-joli, Québec, Canada. 
4. Anderson Cabot Center for Ocean Life, New England Aquarium, Boston, MA, USA.
5. Darling Marine Center, School of Marine Sciences, University of Maine, Walpole, ME, USA.
6. NOAA National Marine Fisheries Service, Northeast Fisheries Science Center, Narragansett, Rhode Island, USA.

* Corresponding author email: nrecord@bigelow.org

**Abstract**

The critically endangered North Atlantic right whale (*Eubalaena glacialis*) faces significant anthropogenic mortality. Recent climatic shifts in traditional habitats have caused abrupt changes in right whale distributions, challenging traditional conservation strategies. Tools that can help anticipate new areas where *E. glacialis* might forage could inform proactive management. In this study, we trained boosted regression tree algorithms with fine-resolution modeled environmental covariates to build prey copepod (*Calanus*) species-specific models of historical and future distributions of *E. glacialis* foraging habitat on the northwest Atlantic shelf, from the Mid-Atlantic Bight to the Labrador Shelf. We determined foraging suitability using *E. glacialis* foraging thresholds for *Calanus* spp. adjusted by a bathymetry-dependent bioenergetic correction factor based on known foraging behavior constraints. Models were then projected to 2046–2065 and 2066–2085 modeled climatologies for representative concentration pathway scenarios RCP 4.5 and RCP 8.5 with the goal of identifying potential shifts in foraging habitat. The models had generally high performance (area under the receiver operating characteristic curve > 0.9) and indicated ocean bottom conditions and bathymetry as important covariates. Historical (1990–2015) projections aligned with known areas of high foraging habitat suitability as well as potential suitable areas on the Labrador Shelf. Future projections suggested that the suitability of potential foraging habitat would decrease in parts of the Gulf of Maine and southwestern Gulf of Saint Lawrence, while potential habitat would be maintained or improved on the western Scotian Shelf, in the Bay of Fundy, on the Newfoundland and Labrador shelves, and at some locations along the continental shelf breaks. Overall, suitable habitat is projected to decline. Directing some survey efforts towards emerging potential foraging habitats can enable conservation management to anticipate the type of distribution shifts that have led to high mortality in the past. 

---

## Version 6 

**v6.01 - v6.04**

*5/27/2023 - Present*

### Changes

* **Final tuning parameters and corrections**
* Revisions to plot code, color schemes, and layouts
* Use of corrected *C. hyperboreus* dataset

### Example Plots

**Combined *Calanus* spp. present-day tau-h-patch probability**

<img src = "readme_imgs/image1.png" width = "60%" alt = "Raw present predictions">

**Combined *Calanus* spp. habitat shift maps, RCP 8.5 2075 vs. present-day**

<img src = "readme_imgs/image2.png" width = "60%" alt = "Habitat shift maps">