
# NEWS

## micromap 1.9.2.9000

* Added option for landscape layout (have not submitted PR yet)


```r
# stat data, data.frame
data('edPov')

# map data
data("USstates") 
statePolys <- create_map_table(USstates, IDcolumn="ST") 

mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "dot","dot", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=5,
  median.row=T,
  map.link=c("StateAb","ID"),
  flip = T
)
```

* Added methods for `mmplot` to use spatialPolygonsDataFrame as sole inputs.  This eliminates need for separate tabular and spatial data inputs. 


```r
###
# old method, still retained

# stat data, data.frame
data('edPov')

# map data
data("USstates") 
statePolys <- create_map_table(USstates, IDcolumn="ST") 

mmplot(stat.data=edPov, map.data=statePolys,
  panel.types=c("labels", "dot","dot", "map"),
  panel.data=list("state","pov","ed", NA),
  ord.by="pov", grouping=5,
  median.row=T,
  map.link=c("StateAb","ID")
)

###
# new method spatialpolygonsdataframe data input only

# stat data 
data(edPov)

# map data
data("USstates") 
USstates@data <- edPov

mmplot(map.data = USstates,
  panel.types=c("labels", "dot", "dot", "map"),
  panel.data=list("state","ed","pov", NA),
  ord.by = "ed", grouping = 5,
  median.row = T
) 

###
# or just the raw USstates polygon
data(USstates)

mmplot(map.data=USstates,
  panel.types=c("labels", "dot", "dot", "map"),
  panel.data=list("ST_NAME","AREA_KM","PERIM_KM", NA),
  ord.by = "AREA_KM", grouping = 5,
  median.row = T
) 
```

* Vignette is compiled with micromap install and is now recognized with R documentation


```r
# to view available vignettes
vignette(package = 'micromap')

# to open
vignette('Introduction_Guide')
```

* Modified color options for perceptual groups. The new version passes the argument to colorRampPalette to create a continuous vector for each perceptual group. This eliminates the need to supply a color argument that has the same length as the grouping argument.


```r
# illustrate color assigment with mmplot

data("edPov")

data("USstates")

statePolys <- create_map_table(USstates, 'ST')

# two color spectrum
mmplot(stat.data=edPov,
  map.data=statePolys,
  panel.types=c('labels', 'dot', 'dot','map'),
  panel.data=list('state','pov','ed', NA),
  ord.by='pov',   
  colors = c('red', 'blue'),     
  grouping=5, median.row=T,
  map.link=c('StateAb','ID'))

# one color with NA values (similar behavior as the previous version)
mmplot(stat.data=edPov,
  map.data=statePolys,
  panel.types=c('labels', 'dot', 'dot','map'),
  panel.data=list('state','pov','ed', NA),
  ord.by='pov',   
  colors = c('red', NA, NA, NA, NA),     
  grouping=5, median.row=T,
  map.link=c('StateAb','ID'))

# a brewer.pal color Spectrum
mmplot(stat.data=edPov,
  map.data=statePolys,
  panel.types=c('labels', 'dot', 'dot','map'),
  panel.data=list('state','pov','ed', NA),
  ord.by='pov',   
  colors = brewer.pal(11, 'PiYG'),     
  grouping=5, median.row=T,
  map.link=c('StateAb','ID'))
```

* Micromap no longer creates a new graphics device window for each plot (for Shiny compatability).

* Removed `scales = "free_y"` to keep correct aspect ratio for maps (still need to work on).
