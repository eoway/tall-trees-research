# tall-trees-research
Borneo tall trees research using ForestGEO and ForestPlots inventory data and Global Airborne Observatory remote sensing data to evaluate whether tall/large trees/species are structurally and functionally distinct from other trees and species.

## Organization of this repo:

- Draft manuscript is in main folder
- Data folder contains data for all analyses
- Scripts for data cleaning and analyses are in separately labeled folders
- Papers folder contains references for tall trees manuscript and other background reading


## Data cleaning (under construction --- still updating the protocols for mort_dat & CSS_PSS_dat):

Data cleaning is separated into N(4) different data cleaning protocols: 

- main_dat  (see clean_main_dat.R)
  - combine taxonomic infor and clean
  - calculate and fill Julian dates
  - add/modify columns and factors for consistency across datasets
  - join wood density from Global Wood Density Database (GWDD) with ForestGEO data by species
  - convert 'unknown' spp to 'indet'
  - update DFstatus
  - remove Danum 50-ha outliers based on stems with extreme relative growth rates 
  - fill DBH for alive [not dead/broken below?] stems using DBH from previous census (where DBH == NA or 0)
 
- growth_dat  (see clean_growth_dat.R)
	- all: 
		- retain only live stems
		- exclude stems if: 
			- shrunk > 25% of initial DBH (rgr < -0.25)
			- grew > 7.5 mm (annual increment > 7.5)

- mort_dat  (see clean_mort_dat.R)
  	- retain only a single observation per tree -> exclude all stem duplicates except the largest DBH
	- exclude status = 'prior' - for Lambir censuses 1, 2, and 3
	- update status as alive or dead depending on 'stem_gone' considering all stems per tree - for Lambir census 4
      
- CSS_PSS_dat  (see clean_css_pss_dat.R)
	- exclude dead stems
	- exclude broken below
	- keep all other (i.e., don't exclude based on outlier growth rates, etc.)

