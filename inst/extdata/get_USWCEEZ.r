# get US West Coast EEZ polygon

# Citation: 
# Flanders Marine Institute (2023). Maritime Boundaries Geodatabase, version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/628
# Fernández Bejarano S, Pohl L (2024). mregions2: Access Data from Marineregions.org: Gazetteer & Data Products. doi:10.32614/CRAN.package.mregions2 https://doi.org/10.32614/CRAN.package.mregions2

library(mregions2)
library(sf)

# # grab US EEZ from Marine Regions data
# usrgns <- gaz_search("United States Exclusive Economic Zone")
useez <- gaz_search(8456) |> gaz_geometry()
# recast as list of polygons instead of multipolygon
useez <- useez$the_geom |> st_cast("POLYGON")
# # plot to double check
# library(ggplot2)
# useez |> ggplot() + geom_sf()

# subset US West Coast EEZ
bbox <- data.frame(lon=c(-130,-130,-115,-115,-130), lat=c(30,50,50,30,30)) |>
  st_as_sf(coords = c("lon", "lat"), 
           crs = st_crs(useez)) |> 
  summarise(geometry=st_combine(geometry)) |>
  st_cast("POLYGON") |>
  st_as_sfc()
useez.wc <- useez[st_contains(bbox, useez, sparse=FALSE)]
# # plot to double check
# useez.wc |> ggplot() + geom_sf()

save(useez.wc, file="USWestCoastEEZ.rdata")
