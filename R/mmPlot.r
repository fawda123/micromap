#' Linked Micromaps
#'
#' Creates a linked micromap, displaying specified polygons and their associated statistical summary displays; differentiated by color.
#' 
#' @param stat.data table of statistics for display
#' @param map.data table of polygons to be associated with each item in stat.data.
#' @param panel.types vector of panel types to specify the layout of the plot (e.g. c('map', 'labels', 'dot.cl'))
#' @param panel.data a list (of lists) of data to be used with each panel (e.g. list(NA, 'Names', list('lower.bound','estimate','upper.bound')).
#' @param map.link a vector with the name of the columns from stat.data and map.data, respectively, on which to join.
#' @param nPanels the number of panels, which is not expected to be set by the user.  The default is the length of panel.types.
#' @param ord.by The column name from stat.data with which to order the lines of the output graphic for a standard mmplot or identifier column on which to group the categorized mmplot.
#' @param grp.by The column name from stat.data with which to order the lines of the output graphic for a standard mmplot or identifier column on which to group the categorized mmplot.
#' @param rev.ord specifies whether the plot should be displayed in reverse order of the ranking column. The default is FALSE.
#' @param grouping the number of lines per perceptual group (for the standard mmplot only). Can be a single number to have the same numer in each group or a vector of numbers for unequal groupings.
#' @param cat category column within stats table for a categorization type mmplot.
#' @param colors a vector of colors for the perceptual groups.  The default is brewer.pal(max(grouping), 'Spectral') for mmplot and brewer.pal(10, 'Spectral') for mmgroupedplot).  The colors are passed to \code{\link[grDevices]{colorRampPalette}} to create a continuous color vector equal in length to the groupings. 
#' @param map.color the color to fill in previously displayed polygons.
#' @param map.all by default, mmplot will only plot the polygons associated with data in the stats table; map.all = TRUE will show all the polygons in the polygon table regardless of whether they are actively referred to.
#' @param map.color2 the color to fill in previously displayed polygons.
#' @param two.ended.maps the resulting micromaps will highlight previously referenced polygons (see map.color2) up to the median perceptual group then switch to highlighting all polygons that are still to be referenced later.
#' @param panel.att a list of panel specific attributes to be altered (see mmplot documentation).
#' @param plot.header the overall title to be placed on the mmplot.
#' @param plot.header.size size of the overall title to be placed on the mmplot.
#' @param plot.header.color color of the overall title to be placed on the mmplot.
#' @param plot.footer the overall footer to be placed under the mmplot.
#' @param plot.footer.size size of the overall footer to be placed under the mmplot.
#' @param plot.footer.color color of the overall footer to be placed under the mmplot.
#' @param plot.width width of the overall plot in inches. Defaults to 7.
#' @param plot.height height of the overall plot in inches. Defaults to 7.
#' @param map.spacing the vertical spacing between maps measured in lines. Perceptual group spacing does not affect map spacing so as to leave the maps as large as possible. The user can increase map spacing using this argument. Defaults to 1.
#' @param plot.pGrp.spacing the vertical spacing between perceptual groups measured in lines. Defaults to 1.
#' @param plot.grp.spacing the vertical spacing between groups measured in lines. Defaults to 1.   
#' @param plot.panel.spacing the vertical spacing between panels measured in lines. Defaults to 1.
#' @param plot.panel.margins the horizontal spacing between panels measured in lines. THIS IS LEGACY CODE AND SHOULD NOT BE USED.
#' @param flip logical to make plot landscape
#' @param ... Additional arguments passed to or from other methods.
#'   
#' @return A list of ggplot2 objects with entries for each individual panel.
#' 
#' @note See the Introduction Guide for a full list of the options available for altering micromaps.
#'
#' @author Quinn Payton \email{Payton.Quinn@@epa.gov}
#' 
#' @import gtable
#' 
#' @keywords hplot
#' 
#' @aliases mmplot mmgroupedplot
#' 
#' @name mmplot
#' 
#' @examples
#' \dontrun{
#' # initial example
#' 
#' data("USstates")
#' head(USstates@@data)
#' statePolys <- create_map_table(USstates, 'ST')
#' head(statePolys)
#' 
#' data("edPov")
#' 
#' 
#' # map data
#' data("USstates") 
#' statePolys <- create_map_table(USstates, IDcolumn="ST") 
#' 
#' ##
#' # standard
#' mmplot(stat.data=edPov, map.data=statePolys,
#'   panel.types=c("labels", "dot","dot", "map"),
#'   panel.data=list("state","pov","ed", NA),
#'   ord.by="pov", grouping=5,
#'   map.link=c("StateAb","ID"),
#'   flip = F
#' )
#' 
#' 
#' }
mmplot <- function(map.data, ...) UseMethod('mmplot')

#' @rdname mmplot
#' 
#' @method mmplot SpatialPolygonsDataFrame
mmplot.SpatialPolygonsDataFrame <- function(map.data, ...){
  
  # separate map.data and stat.data from the combined input
  # put in format used by the default method
  map.data@data$ID <- row.names(map.data@data)
  stat.data <- map.data@data   
  map.data <- create_map_table(map.data, IDcolumn = 'ID')
  map.link <- c('ID', 'ID')
  
  # use default method
  mmplot.default(map.data, stat.data, map.link = map.link, ...)
  
}

#' @rdname mmplot
#' 
#' @method mmplot default
mmplot.default <- function(map.data, 
  stat.data, 
  panel.types, 
  panel.data, 		
  map.link,				
  ord.by, 
  rev.ord = FALSE,	
  grouping, 		
  colors = brewer.pal(11, "Spectral"),	
  flip = FALSE, 
  fill.regions = 'aggregate', 
  text.align = 'right'
){		

  # sanity checks
  chk_len <- lapply(panel.data, length)
  if(any(unlist(chk_len) > 2))
    stop('No more than two variables per list element in panel.data')

  # rename function inputs
  dStats <- stat.data
  dMap <- map.data

  # get plot colors
  colors = colorRampPalette(colors)(grouping)

  ##
  # sanity checks

  # stop if panel.data doesnt match the attribute data
  if(any(!na.omit(unlist(panel.data)) %in% names(stat.data))) 
    stop('panel.data must be in stat.data')
  
  ##########################
  ## data reorganization ###
  ##########################
  # move dStats into the DF variable, add an overall rank column, a perceptual group column based on ranks, a
  # 	within perceptual group ranking, and a column specifying if any entry row should be the median row 												
  DF <- create_DF_rank(dStats, ord.by, grouping, rev.ord)

  # Rearrange the map data table, with a rank, pGrp, and pGrpOrd column
  if(!is.null(map.data) & any(panel.types == 'map')){
    
    # clear out any NA rows
    dMap <- dMap[!(is.na(dMap$coordsx)|is.na(dMap$coordsy)),]
  
    # if map.all is not specified we remove all polygons from mapDF that
    # 	which are not linked to the stat table
    w <- match(dMap[,map.link[2]], unique(DF[,map.link[1]]))
    mapDF <- dMap[!is.na(w),]
  
    # make sure there is a hole and plug column. If not, just insert dummy columns 
    if(!'hole'%in%names(mapDF)) mapDF$hole <- 0
    if(!'plug'%in%names(mapDF)) mapDF$plug <- 0
  
    # link the pGrp and rank columns for plot order and facetting and
    #  make sure they're numeric    
    tmpDF <- unique(DF[,c('pGrp','rank', map.link[1])])
    w <- match(mapDF[,map.link[2]], tmpDF[,map.link[1]])
    mapDF <- cbind(pGrp = tmpDF$pGrp[w],rank = tmpDF$rank[w], mapDF)
    mapDF$pGrp <- as.numeric(mapDF$pGrp)
    mapDF$rank <- as.numeric(mapDF$rank)
  
    # link the pGrpOrd column for plot order and facetting
    tmpDF <- unique(DF[,c('pGrpRank', map.link[1])])
    w <- match(mapDF[,map.link[2]], tmpDF[,map.link[1]])
    mapDF <- cbind(pGrpOrd = tmpDF$pGrpRank[w], mapDF)
    mapDF$pGrpOrd <- as.numeric(mapDF$pGrpOrd)
    
    m.rank <- max(DF$rank+1)/2
    mapDF$m.pGrp <- DF$pGrp[DF$rank==floor(m.rank)] + .5

  }
  
  # set up a list to store plot objects to be created later
  # 	note: each panel in the plot is a "ggplot object" 
  nPanels <- length(panel.types)
  plots <- vector("list", nPanels)
  
  ###############################
  ##### create plot objects #####
  ###############################

  # each section builds a plot object of the specified type
  for(p in 1:nPanels){	
  
    # categorical plot types
    if(length(panel.data[[p]]) == 1){
      
      pltypes <- c('bar', 'jitter', 'label', 'line', 'point', 'step', 'text', 'dot_legend')
      
      # check correct panel type for one variable
      if(!any(panel.types[p] %in% c('map', 'labels', pltypes))){
        txt <- paste('Incorrect panel.type for panel.data:', panel.types[p], 'for', panel.data[[p]])
        stop(txt)
      }
      
      # maps
      if(panel.types[p] == 'map'){
  
  	    plots[[p]]  <- RankMaps(plots[[p]], mapDF, colors = colors, flip = flip, fill.regions = fill.regions)
  
      } 
    
      # labels
      if(panel.types[p] == 'labels'){
        
        txt <- paste0(panel.types[p], '_build(DF, panel.data[[p]], align = text.align, flip = flip)')
        plots[[p]] <- eval(parse(text = txt))
        
      }
      
      # others
      if(panel.types[p] %in% c(pltypes)){
        
        txt <- paste0(panel.types[p], '_build(DF, panel.data[[p]], colors = colors, flip = flip)')
  	    plots[[p]] <- eval(parse(text = txt))
       
      }	
      
    }
    
    # bivariate plot types
    if(length(panel.data[[p]]) == 2){
      
      pltypes <- c('bin2d', 'contour', 'count', 'density_2d', 'hex', 'path', 'point', 'raster')
      
      # check correct panel type for one variable
      if(!any(panel.types[p] %in% pltypes)){
        txt <- paste('Incorrect panel.type for panel.data:', panel.types[p], 'for', panel.data[[p]])
        stop(txt)
      }
      ptypes <- gsub('^point$', 'point2', panel.types)

      # create the plot
      txt <- paste0(ptypes[p], '_build(DF, panel.data[[p]], colors = colors, flip = flip)')
  	  plots[[p]] <- eval(parse(text = txt))
      
    }
  }
  
  ##############################
  ##### construct the plot #####
  ##############################

  ## use gtable to left align 
  plots <- lapply(plots, ggplotGrob)
  toeval <- paste0('plots[[', seq(length(plots)), ']]')
  toeval <- paste(toeval, collapse = ', ')
  if(flip) toeval <- paste0('rbind(', toeval, ', size = "first")')
  else toeval <- paste0('cbind(', toeval, ', size = "first")')
  plots <- eval(parse(text = toeval))

  # plot with some spacing around margins
  grid.arrange(plots, vp =  viewport(height=unit(0.95, "npc"), width=unit(0.95, "npc")))
    
} 

