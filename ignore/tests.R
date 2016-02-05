##
# to do 
#
# continue generic ggplot2 infrastructure
# conf limits for plots if appropriate

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
  ord.by="pov", grouping=5,
  map.link=c("StateAb","ID"),
  flip = F
)

##
# diff grouping
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
# test different panel types
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "bar","bar", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=5,
  map.link=c("StateAb","ID")
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

