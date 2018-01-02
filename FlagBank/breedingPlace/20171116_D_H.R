# 20171116_D_H.R ÇÑ±Û

getPtnReb <- function( pMtx ,pDepth=1 ,pGetAll=F ){

	rDepth <- pDepth - 1
	lastMtx <- pMtx[(nrow(pMtx)-rDepth):nrow(pMtx),]

	ptnLst <- list( )
	for( hIdx in (nrow(pMtx)-1):pDepth ){
		chkMtx <- pMtx[(hIdx-rDepth):hIdx,,drop=F]
		if( all(lastMtx==chkMtx) ){
			ptnObj <- list( hIdx=hIdx ,lastMtx=lastMtx ,depth=pDepth )
			ptnObj$nextRow <- pMtx[hIdx+1,]
			ptnLst[[1+length(ptnLst)]] <- ptnObj
		}
	}

	if( pGetAll ){
		return( ptnLst )
	} else {
		if( 0<length(ptnLst) ){
			return( ptnLst[[1]] )
		} else {
			return( NULL )
		}
	}
} # getPtnReb

