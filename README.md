# caterpillars-analysis-public

## Analysis related to Caterpillars Count! citizen science project and phenological mismatch between vegetation, caterpillars, and birds

### `/InTheMiddle`

Scripts, data, and figures exploring how structured and unstructured citizen science data can be used to study patterns in caterpillar occurrence and phenology from the book chapter:

Di Cecco, G. and Hurlbert, A.H. Caterpillar patterns in space and time: insights from and contrasts between two citizen science datasets. In S. Koptur & R.J. Marquis (Eds). Caterpillars in the Middle. 

### `/code`

- `2019-2020_cat_site_comparisons.r`: comparing Caterpillars Count! sites between 2019-2020
- `/agu`: scripts for making figures used at AGU poster
- `analysis_functions.r`: general analysis functions for using Caterpillars Count! data
- `annual_trends.r`: plot annual trends from Caterpillars Count! arthropod data

- `/exploratory`: misc scripts for exploratory analyses
  - `cat_revi_match.r`: calculate mismatch between Caterpillars Count! phenology and REVI nestling period
  - `caterpillar_biomass_validation_summary.R`: Summarize availability of data for caterpillar species in Abarcas/Ries lab trait database in iNaturalist larval data in MA, GA, and MD regions 
  - `caterpillar_density_hex.R`: Caterpillars Count! survey density in hex cells
  - `cc_inat_family_composition.R`: Plot pie charts of family composition of iNaturalist and Caterpillars Count! records
  - `cc_subset_traitbased_lep_compare.R`: Subset Caterpillars Count! data to sites with 6 good weeks in any year to compare with adult butterfly phenocurves
  - `combine_inat_cats_adults.R`: Overlap in species composition and phenology estimates in iNaturalist adult moths and iNaturalist caterpillar records
  - `compare_inat_cc.R`: Comparing phenology curves in iNaturalist caterpillars and moths with Caterpillars Count! data
  - `coweeta_max_biomass.R`: Coweeta phenometrics
  - `coweeta_pick_comparison.r`: Compare phenology from Coweeta caterpillars and Discover Life Moths
  - `daymet_climate_extraction.r`: Extract daymet data at Caterpillars Count! site coordinates
  - `exploring_survey_effort.r`: Plotting Caterpillars Count! site effort in space
  - `get_climate_veg_npn_data.r`: Get Daymet GDD and NPN first leaf data at Caterpillars Count! sites
  - `get_ebird_phenology.r`: eBird bar chart phenology data for Red-eyed Vireo at Caterpillars Count! sites
  - `historical_revi_data.r`: Get eBird Red-eyed Vireo phenology from Nipissing, Ontario
  - `inat_effort.r`: Calculate metrics related to user effort on iNatuarlist, normalize caterpillar observations by total Insect observations per week
  - `inat_pheno_map.r`: Plot iNaturalist caterpillar phenology at lat-lon cells
  - `nc_butterflies_phenology.R`: patterns in phenology from NC Butterflies dataset
  - `nc_moths_inaturalist_phenology.R`: compare patterns in phenology between NC Moths dataset and adult moth records in iNaturalist
  - `nc_moths_phenology.R`: patterns in phenology from NC Moths dataset
  - `phenology.r`: Rainbow phenocurves for Caterpillars Count! sites
  
- `hex_grids_with_cell_centers.R`: produce a csv with hex cells for eastern North America and their lat/lon centerpoints
- `/inat_higher_taxon`: scripts for getting higher taxonomic information for iNaturalist caterpillar and sawfly records from ITIS
- `plant_species.r`: get taxonomic information about plant host species in Caterpillars Count! data
- `reading_datafiles_without_users.r`: read in anonymized Caterpillars Count! data
- `summaryGraphs.r`: Graph summaries for Caterpillars Count! sites
- `summaryStats.r`: Calculate effort summaries for Caterpillars Count! sites
- `unc_BIOL101_summary.r`: summarize BIOL101 Caterpillars Count! observations
- `/undergrad_projects`: scripts associated with undergrad projects related to caterpillar defense traits and Coweeta phenology

### `/data`: raw data and derived datasets for analyses

Any data files not in subfolder are raw data, including iNaturalist and Caterpillars Count! records

- `/birds`:
- `/CountyBoundary`:
- `/derived_data`: subsets and intermediate summaries of iNaturalist and Caterpillars Count! data for analyses
- `/env`: environmental variables - climate, NPN greenup
-`/frass`: frass trap data
- `/maps`: geographic layers and hexgrid files
  - `/hexgrid_materials`: shapefiles and scripts for hexgrid layer
- `/revi`: eBird records for Red-eyed Vireo
- `/taxonomy`: scripts for obtaining higher level taxonomy for species in iNaturalist, Moths of NC (MNC) and Butterflies of NC (BNC) datasets

### `/figs`

- `/butterflies-nc`: figures from analyses using NC Butterflies data 
- `/caterpillars-count`: figures from analyses using Caterpillars Count! data
- `/cross-comparisons`: figures comparing phenological pattersn in NC butterflies, NC moths, iNaturalist, and Caterpillars Count! data
- `/inaturalist`: figures from analyses using iNaturalist data
- `/moths-nc`: figures from analyses using NC Moths data

### `/iNatUserBehavior`

Scripts, data, and figures for understanding spatiotemporal biases and user behavior patterns in iNaturalist citizen science data associated with the manuscript:

Di Cecco, G., Barve, V., Belitz, M.W., Stucky, B.J., Guralnick, R.P., and Hurlbert, A.H. Observing the observers: how participants contribute data to iNaturalist and implications for biodiversity science.
 
