# creates regionPolys.rda
# add a region column to USstates
# dissolve by region, save
data(edPov)
data(USstates)

tmp <- edPov %>% 
  rename(ST = StateAb)
USstates@data <- left_join(data.frame(USstates), tmp[, c('ST', 'region')], by = 'ST') 
tmp <- unionSpatialPolygons(USstates, IDs = USstates@data$region)
dat <- data.frame(region = row.names(tmp), stringsAsFactors =  FALSE)
row.names(dat) <- row.names(tmp)
tmp <- SpatialPolygonsDataFrame(tmp, dat)
regionPolys <- tmp
save(regionPolys, file = 'data/regionPolys.rda', compress = 'xz') 