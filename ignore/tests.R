##
# to do 
#
# continue generic ggplot2 infrastructure
#  make sure I have all categorical geoms (points,bars, etc.)
#  start with bivariate plots - xy scatters, hists, etc.
#  mmgrouped plot - lots to do with this (boxplots, etc.)
# dimensions of combined plots if dot_legend is used 
#  it has something to do with grob heights e.g. before rbind/cbind of gtables at bottom of mmplot
#  do somethign like this   plots[[3]]$heights[2:3] <- unit(c(-1, -1), 'cm')
# conf limits for plots if appropriate
# plot aesthetics - look at how maggrittr does it

## notes
# bin 2d - supply bins, binwidth arg? 
# optional argument for color ramp of
# bivariate geoms where id of categories not relevant (bin2d, include labels with ggrepl?)

# three dimensional relationships (requires a z variable) - geom_contour, geom_tile, geom_raster, geom_rect



## mmplot with grouping variables, see ex below and lines 121 in mmplot
# need to reorganize data in long format

##
# some tests

# stat data, data.frame
data('edPov')

# map data
data("USstates") 
statePolys <- create_map_table(USstates, IDcolumn="ST") 

##
# nominal 

# standard
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "point","point", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=17,
  map.link=c("StateAb","ID"),
  flip = F
)

## 
# flipped
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "point","point", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=17,
  map.link=c("StateAb","ID"),
  flip = T
)

##
# test different panel types 1
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "bar","bar", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=17,
  map.link=c("StateAb","ID")
)

##
# test different panel types 2
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "jitter","text", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=17,
  map.link=c("StateAb","ID")
)

##
# test different panel types 3
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "label","step", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=17,
  map.link=c("StateAb","ID")
)

##
# bivariate

## 
# points
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "point", "map"),
  panel.data=list("state", c("pov", "ed"), NA),
  ord.by="ed", grouping=17,
  map.link=c("StateAb","ID")
)

## 
# bin2d
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "bin2d", "map"),
  panel.data=list("state", c("pov", "ed"), NA),
  ord.by="ed", grouping=17,
  map.link=c("StateAb","ID"),
  flip = T
)

## 
# count
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "count", "map"),
  panel.data=list("state", c("pov", "ed"), NA),
  ord.by="ed", grouping=17,
  map.link=c("StateAb","ID"),
  flip = T
)

## 
# density_2d
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "density_2d", "map"),
  panel.data=list("state", c("pov", "ed"), NA),
  ord.by="ed", grouping=17,
  map.link=c("StateAb","ID"),
  flip = T
)

##
# hex

mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "hex", "map"),
  panel.data=list("state", c("pov", "ed"), NA),
  ord.by="ed", grouping=17,
  map.link=c("StateAb","ID")
)

##
# path

mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "path", "map"),
  panel.data=list("state", c("pov", "ed"), NA),
  ord.by="ed", grouping=17,
  map.link=c("StateAb","ID")
)

## 
# points and dot_legend
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "dot_legend", "point", "map"),
  panel.data=list("state", NA, c("pov", "ed"), NA),
  ord.by="ed", grouping=17,
  map.link=c("StateAb","ID"),
  flip = T
)

##
# mmgroupedplot
# not really but try this with mmplot

data(regionPolys)
create_map_table(regionPolys, ID= 'region')
data(edPov)

mmplot(stat.data=edPov, map.data=regionPolys,
  panel.types=c("labels", "bar", "map"),
  panel.data=list("region", c("pov", "ed"), NA),
  ord.by="ed", grouping=NULL,
  map.link=c("region", "region"),
  flip = T
)





data("WSA3")
wsa.polys<-create_map_table(WSA3)
national.polys<-subset(wsa.polys, hole==0 & plug==0)
national.polys<-transform(national.polys, ID="National", region=4,
poly=region*1000 + poly)
wsa.polys<-rbind(wsa.polys,national.polys)
data("vegCov")

## 
# points and dot_legend
mmplot(stat.data = vegCov, map.data = wsa.polys,
  panel.types=c("map", , "bar"),
  panel.data=list(NA, "Category", '', 'Estimate.U'),
  ord.by="ed", grouping=17,
  map.link=c("Subpopulation", "ID")
)

# standard plot, ok
mmgroupedplot(stat.data=vegCov,
  map.data=wsa.polys,
  panel.types=c("map", "labels", "bar_cl", "bar_cl"),
  panel.data=list(NA,"Category",
  list("Estimate.P","LCB95Pct.P","UCB95Pct.P"),
  list("Estimate.U","LCB95Pct.U","UCB95Pct.U")),
  grp.by="Subpopulation",
  cat="Category",
  map.link=c("Subpopulation", "ID")
  )

# standard plot, ok
mmgroupedplot(stat.data=vegCov,
  map.data=wsa.polys,
  panel.types=c("map", "labels", "dot", "bar_cl"),
  panel.data=list(NA,"Category",
  list("Estimate.P","LCB95Pct.P","UCB95Pct.P"),
  list("Estimate.U","LCB95Pct.U","UCB95Pct.U")),
  grp.by="Subpopulation",
  cat="Category",
  map.link=c("Subpopulation", "ID"),
  flip = F
  )

# ok
mmgroupedplot(stat.data=vegCov,
  map.data=wsa.polys,
  panel.types=c("map", "labels", "bar_cl", "bar_cl"),
  panel.data=list(NA,"Category",
  list("Estimate.P","LCB95Pct.P","UCB95Pct.P"),
  list("Estimate.U","LCB95Pct.U","UCB95Pct.U")),
  grp.by="Subpopulation",
  cat="Category",
  map.link=c("Subpopulation", "ID")
  )

