#' labels plot
labels_build <- function(DF, align, labels, flip = FALSE){ 

	#** The defaults are set up so that the text will, in theory, cover the entire 
	#** width of the panel, whiel maximizing text height as well
	#** With the way the defaults are set up. default text size needs to be doubled
	tmp.tsize <- 4
	DF$tmp.y <- DF$pGrpOrd*tmp.tsize
	tmp.limsy <- c((min(-DF$pGrpOrd)) * tmp.tsize, 
			(max(-DF$pGrpOrd)) * tmp.tsize)

	DF$labels <- DF[, labels]
	mln <- max(nchar(as.character(DF[, labels])))

	# sets up text orientation given align and flip args
	if(flip){
    
	  if (align=='right') {
  		tmp.limsx <- c(0,mln)
  		tmp.adj <- 0
      }
    if (align=='left') {
  		tmp.limsx <- c(-mln,0)
  		tmp.adj <- 1
    }
	  
	} else {
	  
    if (align=='right') {
  		tmp.limsx <- c(-mln,0)
  		tmp.adj <- 1
      }
    if (align=='left') {
  		tmp.limsx <- c(0,mln)
  		tmp.adj <- 0
    }
	  
	}
	
  if (align=='center') {
		tmp.limsx <- c(-mln/2,mln/2)
		tmp.adj <- .5
    }

	##
	# create plot

	# flip to landscape
	if(flip){
	  
    pl <- ggplot(DF) +
    	     	geom_text(aes(x = tmp.y, y = 0, label = labels, 
    				hjust=tmp.adj, vjust=.4), angle = 90,
    				family='sans', fontface='plain', size=tmp.tsize) +
    	     	facet_grid(.~pGrp)
  
	} else {
	  
	  pl <- ggplot(DF) +
	     	geom_text(aes(x = 0, y = -tmp.y, label = labels, 
				hjust=tmp.adj, vjust=.4), 
				family='sans', fontface='plain', size=tmp.tsize) +
	     	facet_grid(pGrp~.)
	  
	}

	# remove everything except plot content
  pl <- gen_cln(pl, limsx=tmp.limsx, limsy=tmp.limsy, flip = flip)
	pl

}

#' 
dot_legend_build <- function(DF, p, colors, flip = FALSE){ 
	
  # housekeeping
	DF$tmp.data <- rep(0, nrow(DF))
  ncolors <- length(colors)
	limsy <- c(min(-DF$pGrpOrd), max(-DF$pGrpOrd))
  limsx <- c(0, 0)
  
  # create plots
	pl <- ggplot(DF) 

	if(flip){
	  
	  pl <- pl + 
		  geom_point(aes(x=pGrpOrd, y = tmp.data, colour=factor(color))) +
	       	facet_grid(. ~ pGrp)
	
  } else {
    
	  pl <- pl + 
		  geom_point(aes(x=tmp.data, y=-pGrpOrd, colour=factor(color))) +
	       	facet_grid(pGrp~.)
	  
  }
	
	#final plot
  pl <- pl + 
    scale_colour_manual(values=colors, guide='none') 
	
  pl <- gen_cln(pl, limsx = limsx, limsy = limsy, flip = flip)
	pl
	
}

#'
bar_build <- function(DF, dat, colors = colors, flip = FALSE){ 
  
  # subset data to plot
  xvar <- dat
  toplo <- DF[, c(xvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat] <- 'xvar'
  
  # make plot
  pl <- ggplot(toplo)

  if(flip){
    
    pl <- pl + 
  		geom_bar(aes(x = pGrpOrd, y = xvar, fill = factor(color)), stat = 'identity') +
  	  facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +   
      geom_bar(aes(x = -pGrpOrd, y = xvar, fill = factor(color)), stat = 'identity') + 
  	  coord_flip() + 
      facet_grid(pGrp~.) 
    
  }
  
  #final plot
  pl <- pl + 
    scale_fill_manual(values=colors, guide='none') 
    
  pl <- plt_cln(pl, flip = flip)

  return(pl)  	
  
}

#'
bin2d_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat[1]
  yvar <- dat[2]
  toplo <- DF[, c(xvar, yvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat[1]] <- 'xvar'
  names(toplo)[names(toplo) %in% dat[2]] <- 'yvar'
    
  # make plot
  pl <- ggplot(toplo)

  if(flip){
    
    pl <- pl + 
      geom_bin2d(aes(x = xvar, y = yvar)) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_bin2d(aes(x=xvar, y=yvar)) + 
      facet_grid(pGrp~.)
    
  }

  # final plot
  pl <- pl + 
    theme(legend.position = 'none') + 
    scale_fill_gradientn(colours = colors)

  # clean it up
  pl <- plt_cln(pl, flip = flip, biv = TRUE)

  # clean up plot
	return(pl) 
	
}

#'
count_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat[1]
  yvar <- dat[2]
  toplo <- DF[, c(xvar, yvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat[1]] <- 'xvar'
  names(toplo)[names(toplo) %in% dat[2]] <- 'yvar'
    
  # make plot
  pl <- ggplot(toplo)

  if(flip){
    
    pl <- pl + 
      geom_count(aes(x = xvar, y = yvar)) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_count(aes(x=xvar, y=yvar)) + 
      facet_grid(pGrp~.)
    
  }

  # final plot
  pl <- pl + 
    theme(legend.position = 'none') + 
    scale_fill_gradientn(colours = colors)

  # clean it up
  pl <- plt_cln(pl, flip = flip, biv = TRUE)

  # clean up plot
	return(pl) 
	
}

#'
density_2d_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat[1]
  yvar <- dat[2]
  toplo <- DF[, c(xvar, yvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat[1]] <- 'xvar'
  names(toplo)[names(toplo) %in% dat[2]] <- 'yvar'
    
  # make plot
  pl <- ggplot(toplo)

  if(flip){
    
    pl <- pl + 
      geom_density_2d(aes(x = xvar, y = yvar)) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_density_2d(aes(x=xvar, y=yvar)) + 
      facet_grid(pGrp~.)
    
  }

  # final plot
  pl <- pl + 
    theme(legend.position = 'none') + 
    scale_colour_gradientn(colours = colors)

  # clean it up
  pl <- plt_cln(pl, flip = flip, biv = TRUE)

  # clean up plot
	return(pl) 
	
}

#'
hex_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat[1]
  yvar <- dat[2]
  toplo <- DF[, c(xvar, yvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat[1]] <- 'xvar'
  names(toplo)[names(toplo) %in% dat[2]] <- 'yvar'
    
  # make plot
  pl <- ggplot(toplo)

  if(flip){
    
    pl <- pl + 
      geom_hex(aes(x = xvar, y = yvar)) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_hex(aes(x=xvar, y=yvar)) + 
      facet_grid(pGrp~.)
    
  }

  # final plot
  pl <- pl + 
    theme(legend.position = 'none') + 
    scale_fill_gradientn(colours = colors)

  # clean it up
  pl <- plt_cln(pl, flip = flip, biv = TRUE)

  # clean up plot
	return(pl) 
	
}

#'
jitter_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat
  toplo <- DF[, c(xvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat] <- 'xvar'

  # make plot
  pl <- ggplot(toplo)
  
  if(flip){
    
    pl <- pl + 
      geom_jitter(aes(x = pGrpOrd, y = xvar, colour = factor(color), fill = factor(color))) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_jitter(aes(x=xvar, y=-pGrpOrd, colour=factor(color), fill = factor(color))) + 
      facet_grid(pGrp~.)
    
  }
  
  # final plot
  pl <- pl + 
    scale_colour_manual(values = colors, guide = 'none') + 
    scale_fill_manual(values = colors, guide = 'none')
    theme(legend.position = 'none')
  
  # clean it up
  pl <- plt_cln(pl, flip = flip)

  # clean up plot
	return(pl) 
	
}

#'
label_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat
  toplo <- DF[, c(xvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat] <- 'xvar'

  # make plot
  pl <- ggplot(toplo)
  
  if(flip){
    
    pl <- pl + 
      geom_label(aes(x = pGrpOrd, y = xvar, label = xvar, colour = factor(color))) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_label(aes(x=xvar, y=-pGrpOrd, label = xvar, colour=factor(color))) + 
      facet_grid(pGrp~.)
    
  }
  
  # final plot
  pl <- pl + 
    scale_colour_manual(values = colors, guide = 'none') + 
    theme(legend.position = 'none')
  
  # clean it up
  pl <- plt_cln(pl, flip = flip)

  # clean up plot
	return(pl) 
	
}

#'
path_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat[1]
  yvar <- dat[2]
  toplo <- DF[, c(xvar, yvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat[1]] <- 'xvar'
  names(toplo)[names(toplo) %in% dat[2]] <- 'yvar'
    
  # make plot
  pl <- ggplot(toplo)

  if(flip){
    
    pl <- pl + 
      geom_path(aes(x = xvar, y = yvar)) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_path(aes(x=xvar, y=yvar)) + 
      facet_grid(pGrp~.)
    
  }

  # final plot
  pl <- pl + 
    theme(legend.position = 'none') + 
    scale_colour_gradientn(colours = colors)

  # clean it up
  pl <- plt_cln(pl, flip = flip, biv = TRUE)

  # clean up plot
	return(pl) 
	
}


#' geom_point builder
#' 
#' @param ptype chr string for panel type corresponding to ggplot geom, e.g., point, bar, line, etc.
#' @param DF data.frame of actual data
#' @param dat chr string indicating column in DF for data to plot
#' @param colors chr string of colors for the perceptual groups
#' @param flip logical if portrait or landscape
point_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat
  toplo <- DF[, c(xvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat] <- 'xvar'

  # make plot
  pl <- ggplot(toplo)
  
  if(flip){
    
    pl <- pl + 
      geom_point(aes(x = pGrpOrd, y = xvar, colour = factor(color), fill = factor(color))) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_point(aes(x=xvar, y=-pGrpOrd, colour=factor(color), fill = factor(color))) + 
      facet_grid(pGrp~.)
    
  }
  
  # final plot
  pl <- pl + 
    scale_colour_manual(values = colors, guide = 'none') + 
    scale_fill_manual(values = colors, guide = 'none')
    theme(legend.position = 'none')
  
  # clean it up
  pl <- plt_cln(pl, flip = flip)

  # clean up plot
	return(pl) 
	
}

#' for bivariate plots
point2_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat[1]
  yvar <- dat[2]
  toplo <- DF[, c(xvar, yvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat[1]] <- 'xvar'
  names(toplo)[names(toplo) %in% dat[2]] <- 'yvar'
    
  # make plot
  pl <- ggplot(toplo)
  
  if(flip){
    
    pl <- pl + 
      geom_point(aes(x = xvar, y = yvar, colour = factor(color), fill = factor(color))) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_point(aes(x=xvar, y=yvar, colour=factor(color), fill = factor(color))) + 
      facet_grid(pGrp~.)
    
  }
  
  # final plot
  pl <- pl + 
    scale_colour_manual(values = colors, guide = 'none') + 
    scale_fill_manual(values = colors, guide = 'none')
    theme(legend.position = 'none')

  # clean it up
  pl <- plt_cln(pl, flip = flip, biv = TRUE)

  # clean up plot
	return(pl) 
	
}

#' 
line_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat
  toplo <- DF[, c(xvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat] <- 'xvar'

  # make plot
  pl <- ggplot(toplo)
  
  if(flip){
    
    pl <- pl + 
  		geom_line(aes(x = pGrpOrd, y = xvar)) +
  	  facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +   
      geom_line(aes(x = -pGrpOrd, y = xvar)) + 
  	  coord_flip() + 
      facet_grid(pGrp~.) 
    
  }
  
  # clean it up
  pl <- plt_cln(pl, flip = flip)

  # clean up plot
	return(pl) 
	
}

#' 
step_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat
  toplo <- DF[, c(xvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat] <- 'xvar'

  # make plot
  pl <- ggplot(toplo)
  
  if(flip){
    
    pl <- pl + 
  		geom_step(aes(x = pGrpOrd, y = xvar)) +
  	  facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +   
      geom_step(aes(x = -pGrpOrd, y = xvar)) + 
  	  coord_flip() + 
      facet_grid(pGrp~.) 
    
  }
  
  # clean it up
  pl <- plt_cln(pl, flip = flip)

  # clean up plot
	return(pl) 
	
}

#'
text_build <- function(DF, dat, colors = colors, flip = FALSE){

  # subset data to plot
  xvar <- dat
  toplo <- DF[, c(xvar, 'pGrp', 'pGrpOrd', 'color')]
  names(toplo)[names(toplo) %in% dat] <- 'xvar'

  # make plot
  pl <- ggplot(toplo)
  
  if(flip){
    
    pl <- pl + 
      geom_text(aes(x = pGrpOrd, y = xvar, label = xvar, colour = factor(color))) + 
      facet_grid(.~pGrp)
    
  } else {
    
    pl <- pl +
      geom_text(aes(x=xvar, y=-pGrpOrd, label = xvar, colour=factor(color))) + 
      facet_grid(pGrp~.)
    
  }
  
  # final plot
  pl <- pl + 
    scale_colour_manual(values = colors, guide = 'none') + 
    theme(legend.position = 'none')
  
  # clean it up
  pl <- plt_cln(pl, flip = flip)

  # clean up plot
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


	