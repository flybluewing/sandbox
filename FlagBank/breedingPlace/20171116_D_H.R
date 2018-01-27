# pZH <- gEnv$zhF	;pMapSize=3		;pNaVal=-1
getRebMap <- function( pZH ,pMapSize=3 ,pNaVal=0 ){

	lastZoid <- pZH[nrow(pZH),]
	scanSpan <- (nrow(pZH)-pMapSize):(nrow(pZH)-1)
	rMtx <- matrix( pNaVal ,nrow=length(scanSpan) ,ncol=ncol(pZH) )

	for( rIdx in 1:length(scanSpan) ){
		for( cIdx in 1:length(lastZoid) ){
			matIdx <- which(lastZoid==pZH[scanSpan[rIdx],cIdx])
			if( 0 < length(matIdx) ){
				rMtx[rIdx,cIdx] <- matIdx
			}
		}
	}
	
	return(rMtx)

} # getRebMap()


# 20171116_D_H.R ÇÑ±Û
minFreqCnt <- function( pMtx ,pSize=3 ,pInitVal=0 ){

	minCntMtx <- matrix( pInitVal ,nrow=nrow(pMtx) ,ncol=2*pSize )
	colnames(minCntMtx) <- c(sprintf("val%d",1:pSize),sprintf("Cnt%d",1:pSize))

	for( rIdx in 1:nrow(pMtx) ){
		valType <- sort(unique(pMtx[rIdx,]))
		for( vIdx in 1:length(valType) ){
			if(vIdx>pSize){
				break
			}
			minCntMtx[rIdx,vIdx] <- valType[vIdx]
			minCntMtx[rIdx,vIdx+pSize] <- sum(pMtx[rIdx,]==valType[vIdx])
		}
	}

	return( minCntMtx )

} # minFreqCnt()


#	pZoid <- lastZoid;	pValSpan=0:4;	pBase=10
getQGrp <- function( pZoid ,pValSpan ,pBase=10 ){

	grpLst <- list()
	pCode <- pZoid %/% pBase
	for( vIdx in 1:length(pValSpan) ){
		idx <- which( pCode %in% pValSpan[vIdx] )
		grpLst[[1+length(grpLst)]] <- pZoid[idx]
	}
	idx <- which( !(pCode%in%pValSpan) )
	grpLst[[1+length(grpLst)]] <- pZoid[idx]

	grpCnt <- sapply( grpLst ,length )
	names(grpCnt) <- c( as.character(pValSpan) ,"exid" )

	rObj <- list( grpCnt=grpCnt ,grpLst=grpLst )
	rObj$filt <- function( pGrpLst ,pMin=1 ){
			grpCnt <- rObj$grpCnt
			grpCnt[grpCnt==0] <- NA
			rFlag <- rep( 0 ,length(pGrpLst) )
			for( gIdx in 1:length(pGrpLst) ){
				sLoc <- which(grpCnt==pGrpLst[[gIdx]]$grpCnt)
				if( 0==length(sLoc) ){
					next
				}

				for( sIdx in sLoc ){
					if( pMin>grpCnt[sIdx] )
						break
					if( all(rObj$grpLst[[sIdx]]==pGrpLst[[gIdx]]$grpLst[[sIdx]]) ) {
						rFlag[gIdx] <- grpCnt[sIdx]
						break
					}
				}
			} # for(gIdx)

			return( rFlag )
		} # rObj$filt()

	return( rObj )
} # getQGrp()



getTblCnt <- function( pMtx ,pTblVal=NULL ){
	
	tblVal <- pTblVal
	if( is.null(tblVal) ){
		tblVal <- sort(unique(as.vector(pMtx)))
	}

	accMtx <- matrix( 0 ,ncol=(length(tblVal)+1) ,nrow=nrow(pMtx) )
	colnames(accMtx) <- c(as.character(tblVal),"exid")
	exidCol <- ncol(accMtx)
	for( rIdx in 1:nrow(pMtx) ){
		for( cIdx in 1:ncol(pMtx) ){
			colIdx <- which(tblVal==pMtx[rIdx,cIdx])
			if( 0==length(colIdx) ){
				accMtx[rIdx,exidCol] <- accMtx[rIdx,exidCol]+1
			} else {
				accMtx[rIdx,colIdx[1]] <- accMtx[rIdx,colIdx[1]]+1
			}
		}
	}

	return( accMtx )
} # getTblCnt()


scanSameRow <- function( pMtx ,pThld=NULL ){

	thld <- ifelse( is.null(pThld) ,ncol(pMtx) ,pThld )

	nRow.pMtx <- nrow(pMtx)
	sameRowLst <- list()
	for( aIdx in 1:(nRow.pMtx-1) ){
		for( bIdx in (aIdx+1):nRow.pMtx ){
			cnt = sum(pMtx[aIdx,]==pMtx[bIdx,])
			if( cnt>=thld ){
				sameRowObj <- list( aIdx=aIdx ,bIdx=bIdx ,cnt=cnt )
				sameRowLst[[1+length(sameRowLst)]] <- sameRowObj
				break
			}
		}
	}

	if( 0==length(sameRowLst) ){
		rMtx <- matrix( 0 ,ncol=3 ,nrow=0 )
		colnames( rMtx ) <- c("aIdx","bIdx","cnt")
		return( rMtx )
	}

	rMtx <- do.call( rbind 
				,lapply(sameRowLst,function(p){ c(p$aIdx,p$bIdx,p$cnt) }) 
			)
	colnames( rMtx ) <- c("aIdx","bIdx","cnt")
	return( rMtx )
} # scanSameRow



getPtnRebGrp2 <- function( pStdMtx ,pNextJump=1 ,pDepthSpan=5:2 ){
	filtLst <- list()
	histLen <- nrow(pStdMtx)
	for( cIdx in 1:ncol(pStdMtx) ){
		filtObj <- list( cIdx=cIdx )
		ptn <- NULL
		for( dIdx in pDepthSpan ){
			ptn <- getPastPtn( pStdMtx[1:(histLen-pNextJump+1),cIdx] ,pDepth=dIdx ,pScanAll=F )
			if( !is.null(ptn) ){
				filtObj$ptn <- ptn
				filtObj$depth <- dIdx
				filtObj$nextVal <- pStdMtx[ptn$fIdx+pNextJump,cIdx]
				break
			}
		}
		filtLst[[1+length(filtLst)]] <- filtObj
	} # for(cIdx)

	remVal=rep(NA,ncol(pStdMtx))
	for( idx in 1:length(filtLst) ){
		if( !is.null(filtLst[[idx]]$nextVal) ){
			remVal[idx] <- filtLst[[idx]]$nextVal
		}
	}

	rObj <- list( remVal=remVal ,nextJump=pNextJump ,depthSpan=pDepthSpan ,filtLst=filtLst )
	rObj$filt <- function( pAllMtx ,pSurviveLimit=0 ){

		rstLst <- list()
		for( aIdx in 1:nrow(pAllMtx) ){
			rstObj <- list( aIdx=aIdx )
			rstObj$matchCnt <- sum(rObj$remVal==pAllMtx[aIdx,],na.rm=T)
			rstObj$survive <- rstObj$matchCnt<=pSurviveLimit
			rstLst[[1+length(rstLst)]] <- rstObj
		} # for( aIdx )

		return(rstLst)
	} # rObj$filt()

	return( rObj )

} # getPtnRebGrp2()

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
	rObj$filt <- function( pAllMtx ,pLevel=NULL ){
			if( is.null(pLevel) ){
				pLevel <- length(rObj$filtGrpLst)
			}

			rstLst <- list()
			for( aIdx in 1:nrow(pAllMtx) ){
				rstObj <- list( aIdx=aIdx ,survive=TRUE )
				for( fgIdx in length(rObj$filtGrpLst):1 ){
					if( pLevel<fgIdx ){
						next
					}
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

