# Convert Wright et al (2021) SHP for humpback whales in Salish Sea to CSV
# (SHP files accessed 12 December 2024 from https://open.canada.ca/data/en/dataset/39546277-b33e-4f80-8a2d-3ca1ce5b1401/resource/f6a3ecdc-554e-4a16-b9a2-4fea16d460ba)
library(sf)
den.salish.sf <- st_read("Wright_etal_2021_HumpbackWhale_ModelledAbundance_SalishSea.shp")
den.salish.df <- st_drop_geometry(den.salish.sf)
write.csv(den.salish.df, file="Wright.den.salish.csv", row.names=FALSE)

# could perhaps also convert to lon/lat using something like this thread:
# https://stackoverflow.com/questions/74470368/using-sp-terra-raster-packages-in-r-to-convert-albers-equal-area-to-lat-long