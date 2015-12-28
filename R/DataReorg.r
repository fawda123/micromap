create_DF_rank <- function(data, ord.by, group, rev.ord=FALSE){
	# data=dStats; group=grouping
	DF <- data

	DF$rank <- rank(DF[,ord.by], ties.method = "first")# create a new rank column
	if(rev.ord) DF$rank <- max(DF$rank)+1 - DF$rank
	DF <- DF[order(DF$rank),]
	
	m.rank <- (max(DF$rank)+1)/2
  
	group <- rep(group, ceiling(nrow(DF)/group))

  iGroups2 <- cumsum(group) 
	
	DF$pGrp <- as.numeric(cut(DF$rank, c(0,iGroups2), labels=1:length(iGroups2))) 
	# create a new perceptual group column based on rank
			
	pGrpStats <- aggregate(DF$rank, list(DF$pGrp), length)
	names(pGrpStats) <- c('pGrp','length')	
	pGrpStats$addOrd <- 0

	DF <- merge(DF, pGrpStats[,c('pGrp','addOrd')])
	
	DF$pGrpRank <- sapply(1:nrow(DF), function(i) sum(DF$rank[i]>subset(DF, DF$pGrp==DF$pGrp[i])$rank)+1)
	DF$pGrpOrd <- DF$pGrpRank + DF$addOrd
	DF$color <-DF$pGrpRank
		
	DF
}


create_DF_cat <- function(data, grp.by, cat){
	DF <- data 

	tGroups <- unique(DF[,grp.by])
	DF$pGrp <- match(DF[,grp.by],tGroups)									
	tCats <- unique(DF[,cat])
	DF$pGrpOrd <- match(DF[,cat], tCats)	
	DF$color <-DF$pGrpOrd

	DF								
}

