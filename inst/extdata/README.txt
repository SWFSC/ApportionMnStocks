This directory contains files for data from three publications used in the 
ApportionMnStocks R package, R scripts for processing these files for use in this 
package, and final data files for the package. All files are listed below.


# Data files

(1) Curtis et al. (2025)
    - lxi.rda 
    - lxi.mc.rda
    - Curtis_etal_2025_metadata.txt

(2) Becker et al. (2020)
    - Becker_etal_2020_CCE_SDMs_Mn_AvgDens_CVs_1996_2018_set2018.csv
    - Becker_etal_2020_metadata.txt

(3) Wright et al. (2021)
    - Wright_etal_2021_HumpbackWhale_ModelledAbundance_SalishSea.dbf
    - Wright_etal_2021_HumpbackWhale_ModelledAbundance_SalishSea.prj
    - Wright_etal_2021_HumpbackWhale_ModelledAbundance_SalishSea.shp
    - Wright_etal_2021_HumpbackWhale_ModelledAbundance_SalishSea.shx
    - Wright_etal_Data_Dictionary_EN_FR_PRISMM.htm
    - Wright_etal_2021_REFERENCES_EN_FR_PRISMM.htm


# additional R scripts and associated files

(1) get_USWCEEZ.r extracts the U.S. West Coast EEZ from the mregions2 package and 
saves it as USWestCoastEEZ.rda for use in further scripts.

(2) convert.shp.salish.r converts the SHP files from Wright et al. (2021) to CSV 
format, saved as Wright.den.salish.csv.

(3) summarize.density.r summarizes SDM raster data from Becker et al. (2020) to 
abundance per 0.1 degree latitude, runs density.salish.r (which extrapolates
Canadian density estimates to U.S. Salish Sea waters), and saves the combined 
results to mn.den.rda.

(4) create.sysdata.R creates the sysdata.rda file for the package from the 
following data files: lxi.rda, lxi.mc.rda, and mn.den.rda.


# Updating the package

If only the proportion data files - lxi.rda and lxi.mc.rda - are updated, then 
set this directory as the working directory and run create.sysdata.R. 

Updating or changing density map data will require more involved code editing.
