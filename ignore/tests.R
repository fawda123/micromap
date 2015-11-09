##
# some tests

# stat data, data.frame
data('edPov')

# map data
data("USstates") 
statePolys <- create_map_table(USstates, IDcolumn="ST") 

##
# returns warning about aspect ratio, maps should align with other panels
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "dot","dot", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=5,
  median.row=T,
  map.link=c("StateAb","ID"),
  flip = F
)

##
# map aspect ratios fixed at 1:1, maps should align with other panels
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "dot","dot", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=17,
  map.link=c("StateAb","ID"),
  flip = F
)

## 
# problem...
mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "dot","dot", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=17,
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

mmgroupedplot(stat.data=vegCov,
  map.data=wsa.polys,
  panel.types=c("map", "labels", "bar_cl", "bar_cl"),
  panel.data=list(NA,"Category",
  list("Estimate.P","LCB95Pct.P","UCB95Pct.P"),
  list("Estimate.U","LCB95Pct.U","UCB95Pct.U")),
  grp.by="Subpopulation",
  cat="Category",
  map.link=c("Subpopulation", "ID"),
  flip = F
  )
