# Calculate total abundance for Salish Sea
# Note difference in units between Becker et al 2020 and Wright et al 2021:
# - Becker: area (km^2), average 2018 density (1/km^2)
# - Wright: abund per pixel, area in m2

# subset Wright densities to strata in Becker and Calambokidis spreadsheet (pers. comm., 2024)
den.salish.strat <- 
  den.salish.df %>% 
  # convert area to km2
  mutate(area.km2=area_m2/1000000) %>% 
  select(-area_m2) %>% 
  # subset relevant portion of Wright et al 
  # (1) south of 49 (Y=446000 from Wright et al Fig 6, marine regions, and point plots)
  # (2) east of -124.65 (X>1100000) where extrapolation of Becker et al (2020) for outer coast ends
  filter(Y<446000 & X>1100000) %>% 
  # approximate Becker and Calambokidis (pers. comm.) Salish Sea strata
  mutate(bstrat=case_when(Y>382000 & X>1150000 ~ "SJSG",   # 48.42 for Becker, and lon cutoff due to overlap with SJF in west
                          # X<1128000 ~ "SJFW",   # -124.26 for Becker, but they combined
                          TRUE ~ "SJF"))

# summarize
den.salish.strat.sum <- den.salish.strat %>% 
  # summarize
  group_by(bstrat) %>% 
  summarize(Nhat=sum(Nhat), area.km2=sum(area.km2), den=Nhat/area.km2)

# get areas of US EEZ in strata (combine Puget and San Juans/Strait of Georgia)
## create Salish Sea polygon for US EEZ
### load USWC EEZ
load("USWestCoastEEZ.rda")
### create bounding box
bounding_poly <- st_as_sf(data.frame(id = 1),
                          geometry = st_sfc(st_polygon(list(rbind(c(-124.65,49), c(-124.65,48), c(-123.5,48), c(-123.5,47),c(-118,47),c(-118,49),c(-124.65,49))))),
                          crs = st_crs(useez.wc))
### subset Salish Sea polygon
salish_eez <- st_intersection(useez.wc, bounding_poly)
### create bounding boxes for strata
bounding_poly_sjsgps <- st_as_sf(data.frame(id = 1),
                              geometry = st_sfc(st_polygon(list(rbind(c(-123.4,48.42), c(-123.4,49.1), c(-122,49.1), c(-122,47),c(-123.4,47),c(-122.77, 48.14),c(-122.75,48.24), c(-122.65, 48.38),c(-122.65,48.42),c(-123.4,48.42))))),
                              crs = st_crs(useez.wc))
sjsgps <- st_intersection(salish_eez, bounding_poly_sjsgps)
sjf <- st_difference(salish_eez, bounding_poly_sjsgps)
# plot(sjsgps, col=2)
# plot(sjf, col=5, add=T)
n.sjsgps <- as.numeric(st_area(sjsgps))/1000000 * den.salish.strat.sum$den[den.salish.strat.sum$bstrat=="SJSG"]
n.sjf <- as.numeric(st_area(sjf))/1000000 * den.salish.strat.sum$den[den.salish.strat.sum$bstrat=="SJF"]
n.salish <- n.sjsgps + n.sjf
rm(n.sjsgps, n.sjf, sjsgps, sjf, salish_eez, bounding_poly, bounding_poly_sjsgps)
# n.sjsgps very similar to using Elizabeth's higher density for San Juans and lower density for Puget
