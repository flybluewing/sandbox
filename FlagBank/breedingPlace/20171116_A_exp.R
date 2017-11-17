
pZh <- as.matrix(FB$zh)
pCode <- pZh[20,]
pArea <- pZh[1:19,]


getMatchLst.fixed <- function( pCode ,pArea ){

	matchLst <- list()
	for( rIdx in 1:nrow(pArea) ){
		indices <- which(pArea[rIdx,]==pCode)
		if( 0<length(indices) ){
			matchLst[[1+length(matchLst)]] <-
				list( hIdx=rIdx ,fIdx=indices )
		}
	} # for(rIdx)

	return( matchLst )
} # getMatchLst.fixed()

