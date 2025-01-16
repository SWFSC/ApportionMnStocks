# load libraries
library(sf)
library(dplyr)
library(magrittr)

# import 
## (1) multi-year average density surface for Mn based on 1996 to 2018 period 
##    (with year set to 2018 throughout to eliminate abundance trends) 
##    (Becker et al 2020)
## (2) 2018 density surface for Mn in Salish Sea from Wright et al (2021)
mn.den <- read.csv("C:/Users/kacurtis/Data/Research/Github/Mn-StockProportions-CMR-dev/Becker_etal_2020_CCE_SDMs_Mn_AvgDens_CVs_1996_2018_set2018.csv")
## US West Coast EEZ subset from Maritime Boundaries Geodatabase (Flanders Marine Institute, 2023)
load("USWestCoastEEZ.rda")
## lxi for latbins
attach("lxi.rda")
lbin <- lxi %>% select(latbin, minlat, maxlat) %>% distinct()
  
# edit dataframe
mn.den %<>% 
  rename(area=area_km, x.den=Mn.Avg.Dens, cv.den=Mn.CV.Dens) %>% 
  mutate(mlon=mlon-360)

# extrapolate northernmost densities to EEZ boundary
## get northernmost density line
ndl <- mn.den %>% filter(mlat==48)
## extrapolate to 48.5
nd <- bind_rows(ndl,ndl,ndl,ndl,ndl)
nd$mlat <- rep(seq(48.1,48.5,0.1), each=nrow(ndl))
mn.den %<>% bind_rows(nd)
rm(ndl, nd)
## subset mn.den in US EEZ
### convert mn.den to sf
mn.den.sf <- mn.den %>%
  st_as_sf(coords = c("mlon", "mlat"),
           crs = st_crs(useez.wc))
### subset to points in USWC EEZ and convert back to df
mn.den.sf$ineez <- as.vector(st_intersects(useez.wc, mn.den.sf, sparse = FALSE))
mn.den.eez <- bind_cols(st_drop_geometry(mn.den.sf),
                        st_coordinates(mn.den.sf)) %>%
  rename(mlon=X, mlat=Y) %>% 
  #### add 48.5 to ineez
  mutate(ineez=if_else(mlat==48.5 & mlon>=(-125), TRUE, ineez)) %>% 
  filter(ineez) %>% select(-ineez)
rm(useez.wc, mn.den)

# calculate humpbacks per 0.1 degree latitude for USWC from Becker et al (2020)
## get non-Salish latitude breaks
denbreaks <- union(lbin$minlat[!grepl("Salish",lbin$latbin)], lbin$maxlat[!grepl("Salish",lbin$latbin)])
mn.n.line <- mn.den.eez %>% 
  # cut into lat bins excluding Salish Sea
  mutate(latbin = cut(mlat, breaks=denbreaks)) %>%
  # sum abundance per decimal latitude
  group_by(mlat, latbin) %>% 
  summarize(n.mn=sum(area*x.den)) %>%
  ungroup()

# add Salish Sea bin from Wright et al (2021)
den.salish.df <- read.csv("Wright.den.salish.csv")
source("density.salish.r")
mn.n.line %<>% bind_rows(data.frame(mlat=49, latbin=last(lbin$latbin), n.mn=n.salish))
save(mn.n.line, file="mn.den.rda")
