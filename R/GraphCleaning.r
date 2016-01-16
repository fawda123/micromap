##########################
##### theme settings #####
##########################

plt_cln <- function(pl, limsx = NA, limsy = NA, border = TRUE, flip = FALSE){

  ##  
  # flip limits if flip true
  if(flip) limsy <- rev(-1 * limsy[1:2])
  
  ##
  # xaxis 
	 
	# set axis limits
  xstr.limits <- as.character(paste0('c(',min(limsx), ',', max(limsx),')'))
	xstr.limits <- paste0(", limits =", xstr.limits)
	xstr <- paste0("scale_x_continuous(", xstr.limits, ')')

  ##
	# yaxis 

	ystr.limits <- as.character(paste('c(',min(limsy), ',', max(limsy),')'))
	ystr.limits <- paste(", limits=", ystr.limits)
	ystr <- paste("scale_y_continuous(", ystr.limits, ')')

	if(flip){
	  xstr <- gsub('_x_', '_y_', xstr)
	  ystr <- gsub('_y_', '_x_', ystr)
	}
	
	##
	# set limits
  pl <- pl +
    eval(parse(text = ystr)) + 
    eval(parse(text = xstr))
  
  ##
  # border
  
	if(border) 
	  pl <- pl + theme(
	    panel.border = element_rect(colour = "black", fill=NA, size=0.8), 
	    axis.line = element_blank()
	  )

  ##
  # remove background color, fac labs
  pl <- pl + theme(
    panel.grid.major = element_line(colour = 'grey'), 
    panel.background = element_rect(colour = 'white', fill = 'white'),
    strip.background = element_blank(), 
	  strip.text.x = element_blank(), 
		strip.text.y = element_blank()
    )
   
  ##
  # format ticks, axis labs, axis text
  pl <- pl +
    theme(
      axis.ticks = element_blank(), 
      axis.title = element_blank()
    )
  
  if(flip){
    
    pl <- pl + theme(axis.text.x = element_blank())
    
  } else {
   
    pl <- pl+ theme(axis.text.y = element_blank()) 
    
  }
  
	pl

}

# remove everything but plot content, generic for labels and maps
#
# pl input ggplot object
# limsx xlimits
# limsy ylimits
# flip logical if flip is true
gen_cln <- function(pl, limsx, limsy, flip){
  
  ##  
  # flip limits if flip true
  if(flip) limsy <- rev(-1 * limsy[1:2])
  
  ##
  # xaxis 
	 
	# set axis limits
  xstr.limits <- as.character(paste0('c(',min(limsx), ',', max(limsx),')'))
	xstr.limits <- paste0("limits =", xstr.limits)
	xstr <- paste0("scale_x_continuous(", xstr.limits, ')')

  ##
	# yaxis 

	ystr.limits <- as.character(paste('c(',min(limsy), ',', max(limsy),')'))
	ystr.limits <- paste(", limits=", ystr.limits)
	ystr <- paste("scale_y_continuous(", ystr.limits, ')')

	if(flip){
	  xstr <- gsub('_x_', '_y_', xstr)
	  ystr <- gsub('_y_', '_x_', ystr)
	}
	
	##
	# set limites
  pl <- pl +
    eval(parse(text = ystr)) + 
    eval(parse(text = xstr))
  
  # remove everything else but labels
  pl <- pl + theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.text = element_blank(), 
    axis.title = element_blank(), 
    axis.ticks = element_blank(), 
    panel.background = element_rect(colour = 'white', fill = 'white'),
    strip.background = element_blank(), 
	  strip.text.x = element_blank(), 
		strip.text.y = element_blank()
    )
   
  return(pl)
  
}

