# 20171116_D_H.R ÇÑ±Û


getPtnRebGrp <- function( pStdMtx ,pNextJump=1 ){

	filtGrpLst <- list()
	for( filtColNum in ncol(pStdMtx):2 ){
		chkColMtx <- combinations(ncol(zhF),filtColNum)
		filtLst <- list()
		for( chkColIdx in 1:nrow(chkColMtx) ){
			filtObj <- list( filtCol=chkColMtx[chkColIdx,] )
			stdCodeMtx <- pStdMtx[,filtObj$filtCol]
			ptn <- NULL
			for( dIdx in 3:1 ){
				ptn <- getPtnReb( stdCodeMtx[1:(nrow(stdCodeMtx)-pNextJump+1),] ,pDepth=dIdx )
				if( !is.null(ptn) ){
					filtObj$ptn <- ptn
					filtObj$depth <- dIdx
					filtObj$nextRow <- stdCodeMtx[ptn$hIdx+pNextJump,]
					break
				}
			}
			filtLst[[1+length(filtLst)]] <- filtObj
		} # for(chkColIdx)

		availFlag <- sapply( filtLst ,function(p){ !is.null(p$depth) } )
		filtLst <- filtLst[ availFlag ]

		filtGrpLst[[1+length(filtGrpLst)]] <- list( filtColNum=filtColNum ,filtLst=filtLst )
	} # for( filtColNum )

	rObj <- list( filtGrpLst=filtGrpLst ,nextJump=pNextJump )
	rObj$filt <- function( pAllMtx ){
			rstLst <- list()
			for( aIdx in 1:nrow(pAllMtx) ){
				rstObj <- list( aIdx=aIdx ,survive=TRUE )
				for( fgIdx in length(rObj$filtGrpLst):1 ){
					filtLst <- rObj$filtGrpLst[[fgIdx]]$filtLst
					for( fIdx in seq_len(length(filtLst)) ){
						if( all(filtLst[[fIdx]]$nextRow==pAllMtx[aIdx,filtLst[[fIdx]]$filtCol]) ){
							rstObj$rebFound <- c( fgIdx ,fIdx )
							rstObj$rebVal <- filtLst[[fIdx]]$nextRow
							rstObj$rebIdx <- filtLst[[fIdx]]$filtCol
							rstObj$survive <- FALSE
							break
						}
					}
					if( !rstObj$survive ){
						break
					}
				}
				rstLst[[1+length(rstLst)]] <- rstObj
			} # for(aIdx)
			return( rstLst )
		} # rObj$filt( )

	return( rObj )
} # getPtnRebGrp()




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

