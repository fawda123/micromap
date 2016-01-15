#' labels plot
labels_build <- function(pl, p, DF, att, flip = FALSE){ 

	DF$tmp.labels <- DF[,unlist(att[[p]]$panel.data)]

	 #** The defaults are set up so that the text will, in theory, cover the entire 
	 #** width of the panel, whiel maximizing text height as well
	 #** With the way the defaults are set up. default text size needs to be doubled
	tmp.tsize <- att[[p]]$text.size*4
	DF$tmp.y <- DF$pGrpOrd*att[[p]]$text.size
	tmp.limsy <- c((min(-DF$pGrpOrd)-.5)*att[[p]]$text.size, 
			(max(-DF$pGrpOrd)+.5)*att[[p]]$text.size)

	mln <- max(nchar(as.character(DF$tmp.labels)))
	# sets up text orientation given align and flip args
	if(flip){
    
	  if (att[[p]]$align=='right') {
  		tmp.limsx <- c(0,mln)
  		DF$tmp.adj <- 0
      }
    if (att[[p]]$align=='left') {
  		tmp.limsx <- c(-mln,0)
  		DF$tmp.adj <- 1
    }
	  
	} else {
	  
    if (att[[p]]$align=='right') {
  		tmp.limsx <- c(-mln,0)
  		DF$tmp.adj <- 1
      }
    if (att[[p]]$align=='left') {
  		tmp.limsx <- c(0,mln)
  		DF$tmp.adj <- 0
    }
	  
	}
	
  if (att[[p]]$align=='center') {
		tmp.limsx <- c(-mln/2,mln/2)
		DF$tmp.adj <- .5
    }

	  #################################
	  #################################

	# flip to landscape
	if(flip){
	  
    pl <- ggplot(DF) +
    	     	geom_text(aes(x = tmp.y, y = 0, label = tmp.labels, 
    				hjust=tmp.adj, vjust=.4), angle = 90,
    				family=att[[p]]$text.font, fontface=att[[p]]$text.face, size=tmp.tsize) +
    	     	facet_grid(.~pGrp) +
        		scale_colour_manual(values=att$colors) 
  
	} else {
	  
	  pl <- ggplot(DF) +
	     	geom_text(aes(x = 0, y = -tmp.y, label = tmp.labels, 
				hjust=tmp.adj, vjust=.4), 
				family=att[[p]]$text.font, fontface=att[[p]]$text.face, size=tmp.tsize) +
	     	facet_grid(pGrp~.) +
    		scale_colour_manual(values=att$colors) 
	  
	}

	# remove everything except plot content
  pl <- gen_cln(pl, limsx=tmp.limsx, limsy=tmp.limsy, flip = flip)
	pl

}

#' generic panel function
pan_build <- function(pl, p, ptype, DF, att, flip = FALSE){
 
  # stop if panel type does not exist
  if(!ptype %in% c('dot', 'bar'))
    stop(paste0("unknown panel type -- '", ptype, "'", sep = ''))

  # subset data to plot
  xvar <- att[[p]]$panel.data
  toplo <- DF[, c(xvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% xvar] <- 'xvar'

  # padding for pgrps by facet
  tmp.limsx <- range(DF[,unlist(att[[p]]$panel.data)], na.rm=T)
	if(any(!is.na(att[[p]]$xaxis.ticks))) tmp.limsx <- range(c(tmp.limsx, att[[p]]$xaxis.ticks))
	tmp.limsx <- tmp.limsx + c(-1,1) * diff(tmp.limsx)*.05
	tmp.limsy <- -(range(DF$pGrpOrd) + c(-1,1) * .5)
	if(diff(tmp.limsx)==0) tmp.limsx <- tmp.limsx + c(-.5,.5)
  
  # make plot
  pl <- eval(parse(text = paste(ptype, '_build(toplo, att, flip = flip)',sep = '')))

  # clean up plot
	pl <- plt_cln(pl, limsx=tmp.limsx, limsy=tmp.limsy, flip = flip)
	
	return(pl) 
	
}

#'
dot_build <- function(pl, att, flip = FALSE){ 
 
  # make plot
  pl <- ggplot(pl)

  if(flip){
    
    pl <- pl + 
  		geom_point(aes(x = pGrpOrd, y = xvar, colour = factor(color))) +
  	  facet_grid(.~pGrp) + 
  		scale_colour_manual(values=att$colors, guide='none')   
    
  } else {
    
    pl <- pl +   
      geom_point(aes(x=xvar, y=-pGrpOrd, colour=factor(color))) +
  	  facet_grid(pGrp~.) + 
  		scale_colour_manual(values=att$colors, guide='none') 
    
  }
  
  return(pl)
	
}

#'
bar_build <- function(pl, att, flip = FALSE){ 
  
  # make plot
  pl <- ggplot(pl)

  if(flip){
    
    pl <- pl + 
  		geom_bar(aes(x = pGrpOrd, y = xvar, fill = factor(color)), stat = 'identity') +
  	  facet_grid(.~pGrp) +
  		scale_fill_manual(values=att$colors, guide='none')   
    
  } else {
    
    pl <- pl +   
      geom_bar(aes(x = -pGrpOrd, y = xvar, fill = factor(color)), stat = 'identity') + 
  	  coord_flip() + 
      facet_grid(pGrp~.) +
  		scale_fill_manual(values=att$colors, guide='none') 
    
  }
  
  return(pl)

}

######
# everything below here needs to be dealt with somehow (remove, modify, etc.)

#'
ranks_build <- function(pl, p, DF, att, flip = FALSE){ 
	tmp.tsize <- att[[p]]$size*4
	DF$tmp.y <- -DF$pGrpOrd*att[[p]]$size
	tmp.limsy <- c((min(-DF$pGrpOrd)-.5)*att[[p]]$size, 
			(max(-DF$pGrpOrd)+.5)*att[[p]]$size)

	mln <- max(nchar(as.character(DF$rank)))
	  if (att[[p]]$align=='right') {
			tmp.limsx <- c(-mln,0)
			DF$tmp.adj <- .9
	    }
	  if (att[[p]]$align=='left') {
			tmp.limsx <- c(0,mln)
			DF$tmp.adj <- .1
	    }
	  if (att[[p]]$align=='center') {
			tmp.limsx <- c(-mln/2,mln/2)
			DF$tmp.adj <- .5
	    }
	
	  #################################
	  #################################


	pl <- ggplot(DF) +
		geom_text(aes(x=0, y=tmp.y, label=rank, hjust=tmp.adj, vjust=.4), 
				font=att[[p]]$font, face=att[[p]]$face, size=tmp.tsize) +
     		facet_grid(pGrp~., scales="free_y", space="free") 

	pl <- plot_opts(p, pl, att)		
	pl <- graph_opts(p, pl, att)		
	pl <- axis_opts(p, pl, att, limsx=tmp.limsx, limsy=tmp.limsy, border=FALSE)

	pl
	
}

#' 
dot_legend_build <- function(pl, p, DF, att, flip = FALSE){ 
	DF$tmp.data <- rep(0, nrow(DF))

	tmp.limsy <- -(range(DF$pGrpOrd) + c(-1,1) * .5)
	tmp.limsx <- c(-.5,.5)

	ncolors <- length(att$colors)

	  #################################
	  #################################


	pl <- ggplot(DF) 

	if(att[[p]]$point.border) pl <- pl + geom_point(aes(x=tmp.data, y=-pGrpOrd), 
									colour='black',
									size=att[[p]]$point.size*2.5, 
									pch=att[[p]]$point.type)

	pl <- pl + 
		geom_point(aes(x=tmp.data, y=-pGrpOrd, colour=factor(color)), 
				size=att[[p]]$point.size*2, pch=att[[p]]$point.type) +
	     	facet_grid(pGrp~., scales="free_y", space="free") +
		scale_colour_manual(values=att$colors, guide='none') 

	pl <- plot_opts(p, pl, att)		
	pl <- graph_opts(p, pl, att)		
	pl <- axis_opts(p, pl, att, limsx=tmp.limsx, limsy=tmp.limsy)

	pl
}

#'
dot_cl_build <- function(pl, flip = FALSE){ 

  NULL
	
}

#'
bar_cl_build <- function(pl, flip = FALSE){ 
  
  NULL
  
}

#'
box_summary_build <- function(pl, flip = FALSE){ 
	if(length(att[[p]]$panel.data)==5) iCols <- c(1,2,2,3,4,4,5) else iCols <- 1:7
	tmp.data <- DF[,unlist(att[[p]]$panel.data)]
	tmp.data <- tmp.data[,iCols]
	names(tmp.data) <- paste('tmp.data',1:7,sep='')
	DF <- cbind(DF, tmp.data)

	DF$tmp.adj <- att[[p]]$graph.bar.size

	tmp.limsx <- range(DF[,unlist(att[[p]]$panel.data)], na.rm=T)
	if(any(!is.na(att[[p]]$xaxis.ticks))) tmp.limsx <- range(c(tmp.limsx, att[[p]]$xaxis.ticks))
	tmp.limsx <- tmp.limsx + c(-1,1) * diff(tmp.limsx)*.05
	tmp.limsy <- -(range(DF$pGrpOrd) + c(-1,1) * .5)


	ncolors <- length(att$colors)

	  #################################
	  #################################


	pl  <- 
		ggplot(DF) +
	     	geom_errorbarh(aes(x=tmp.data1, xmin=tmp.data1, xmax=tmp.data7, y=-pGrpOrd), 
				height=.9*att[[p]]$graph.bar.size) + 
		geom_rect(aes(xmin=tmp.data2, ymin=-pGrpOrd-(tmp.adj/4), 
					xmax=tmp.data5, ymax=-pGrpOrd+(tmp.adj/4), 
					fill=factor(color), colour='black')) +
		geom_rect(aes(xmin=tmp.data3, ymin=-pGrpOrd-(tmp.adj/2), 
					xmax=tmp.data6, ymax=-pGrpOrd+(tmp.adj/2), 
					fill=factor(color), colour='black')) +
		geom_segment(aes(x=tmp.data4, y=-pGrpOrd-(tmp.adj/2), 
					xend=tmp.data4, yend=-pGrpOrd+(tmp.adj/2)), colour='black') +
		facet_grid(pGrp~., scales="free_y", space="free") +
		scale_colour_manual(values='black', guide='none') +
		scale_fill_manual(values=att$colors, guide='none')

	if(!any(is.na(att[[p]]$add.line))){
		if(length(att[[p]]$add.line.col)==1) att[[p]]$add.line.col <- rep(att[[p]]$add.line.col[1], length(att[[p]]$add.line))
		if(length(att[[p]]$add.line.typ)==1) att[[p]]$add.line.typ <- rep(att[[p]]$add.line.typ[1], length(att[[p]]$add.line))
		if(length(att[[p]]$add.line.size)==1) att[[p]]$add.line.size <- rep(att[[p]]$add.line.size[1], length(att[[p]]$add.line))

		for(j in 1:length(att[[p]]$add.line)) pl <- pl + geom_vline(xintercept = att[[p]]$add.line[j], 
										data=DF, colour=att[[p]]$add.line.col[j], 
										linetype=att[[p]]$add.line.typ[j],
										size=att[[p]]$add.line.size[j])
	  }


	pl <- plot_opts(p, pl, att)		
	pl <- graph_opts(p, pl, att)		
	pl <- axis_opts(p, pl, att, limsx=tmp.limsx, limsy=tmp.limsy, expx=FALSE, flip = flip)

	pl
}


	