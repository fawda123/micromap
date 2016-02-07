##
# to do 
#
# continue generic ggplot2 infrastructure
#  make sure I have all categorical geoms (points,bars, etc.)
#  start with bivariate plots - xy scatters, hists, etc.
#  mmgrouped plot - lots to do with this (boxplots, etc.)
# conf limits for plots if appropriate
# plot aesthetics - look at how maggrittr does it

##
# some tests

# stat data, data.frame
data('edPov')

# map data
data("USstates") 
statePolys <- create_map_table(USstates, IDcolumn="ST") 

##
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
# test different panel types 4
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "point", "map"),
  panel.data=list("state", c("pov", "ed"), NA),
  ord.by="ed", grouping=17,
  map.link=c("StateAb","ID"),
  flip = T
)

##
# mmgroupedplot

data("WSA3")
wsa.polys<-create_map_table(WSA3)
national.polys<-subset(wsa.polys, hole==0 & plug==0)
national.polys<-transform(national.polys, ID="National", region=4,
poly=region*1000 + poly)
wsa.polys<-rbind(wsa.polys,national.polys)
data("vegCov")

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

