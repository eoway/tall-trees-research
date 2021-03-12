# tall-trees-research
Borneo tall trees research using ForestGEO and ForestPlots inventory data and Global Airborne Observatory remote sensing data to evaluate whether tall/large trees/species are structurally and functionally distinct from other trees and species.

## Organization of this repo:

- Draft manuscript is in main folder
- Data is in Google Drive Tall_trees_research folder
- Scripts for data cleaning and analyses are in separately labeled folders
- Papers folder contains references for tall trees manuscript and other background reading


## Data cleaning (under construction --- still updating the protocols for mort_dat & CSS_PSS_dat):

Data cleaning is separated into N(4) different data cleaning protocols: 

- main_dat  (see clean_main_dat.R)
  - combine taxonomic info and clean
  - calculate and fill Julian dates
  - add/modify columns and factors for consistency across datasets
  - join wood density from Global Wood Density Database (GWDD) with ForestGEO data by species
  - convert 'unknown' spp to 'indet'
  - update DFstatus
  - remove Danum 50-ha outliers based on stems with extreme relative growth rates 
  
- growth_dat  (see clean_growth_dat.R)
	- all: 
		- retain only live stems
		- exclude stems if: 
			- shrunk > 25% of initial DBH (rgr < -0.25)
			- grew > 7.5 mm (annual increment > 7.5)

- mort_dat  (see clean_mort_dat.R)
  	- retain only a single observation per tree -> exclude all stem duplicates except the largest DBH
	- exclude status = 'prior' - for Lambir censuses 1, 2, and 3
	- update status as alive or dead depending on 'broken below' considering all stems per tree - for Danum 50 censuses 1 and 2; Lambir censuses 1, 3, and 3
	- update status as alive or dead depending on 'stem_gone' considering all stems per tree - for Lambir census 4
      
- CSS_PSS_dat  (see clean_css_pss_dat.R)
	- exclude dead stems
	- exclude broken below
	- keep all other (i.e., don't exclude based on outlier growth rates, etc.)


## Manuscript Plots:
- located at the end of the heights.r and growth_rates_NH_EO_NH.r scripts
- first run heights.r and then run growth_rates_NH_EO_NH.r
	- You don't have to necessarily run the whole heights script, just the portion before "Random Plots"

