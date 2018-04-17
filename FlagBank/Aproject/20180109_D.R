# 20180109_D.R 교차모델

# pMtx <- gEnv$zhF[1:(nrow(gEnv$zhF)-1),]	;pZoid <- gEnv$zhF[nrow(gEnv$zhF),]
fnd2SeqReb <- function( pMtx ,pZoid ,pDepth=50 ,pSrcNum=2 ){

	scanFnc <- function( ptn ,zoid ){
		for( idx in 1:5 ){
			if( all(ptn==zoid[idx:(idx+1)]) ){
				return( idx )
			}
		}
		return( NULL )
	} # scanFnc()

	fndLst <- list()
	for( idx in 1:pDepth ){
		hIdx <- nrow(pMtx) - (idx-1)
		if( 1>hIdx ){
			break
		}
		for( aIdx in 1:5 ){
			fnd <- scanFnc( pMtx[hIdx,aIdx:(aIdx+1)] ,pZoid )
			if( !is.null(fnd) ){
				fndLst[[1+length(fndLst)]] <- list( idx=idx ,aIdx=aIdx ,fnd=fnd ,hIdx=hIdx )
				if( pSrcNum == length(fndLst) ){
					return(fndLst) # reach to max level
				}
			}
		} # aIdx
	} # hIdx

	return( fndLst )

} # fnd2SeqReb()

fnd3SeqReb <- function( pMtx ,pZoid ,pDepth=50 ,pSrcNum=2 ){

	scanFnc <- function( ptn ,zoid ){
		for( idx in 1:4 ){
			if( all(ptn==zoid[idx+0:2]) ){
				return( idx )
			}
		}
		return( NULL )
	} # scanFnc()

	fndLst <- list()
	for( idx in 1:pDepth ){
		hIdx <- nrow(pMtx) - (idx-1)
		if( 1>hIdx ){
			break
		}
		for( aIdx in 1:4 ){
			fnd <- scanFnc( pMtx[hIdx,aIdx+0:2] ,pZoid )
			if( !is.null(fnd) ){
				fndLst[[1+length(fndLst)]] <- list( idx=idx ,aIdx=aIdx ,fnd=fnd ,hIdx=hIdx )
				if( pSrcNum == length(fndLst) ){
					return(fndLst) # reach to max level
				}
			}
		} # aIdx
	} # hIdx

	return( fndLst )

} # fnd3SeqReb()



rstLst <- list()
for( hIdx in 100:nrow(gEnv$zhF) ){
	fndLst <- fnd2SeqReb( gEnv$zhF[1:(hIdx-1),] ,gEnv$zhF[hIdx,] ,pSrcNum=4 )
	rstLst[[1+length(rstLst)]] <- fndLst
}
#	5까지 23/703

rstLst <- list()
for( hIdx in 100:nrow(gEnv$zhF) ){
	fndLst <- fnd3SeqReb( gEnv$zhF[1:(hIdx-1),] ,gEnv$zhF[hIdx,] )
	rstLst[[1+length(rstLst)]] <- fndLst
}
#	50,48,5,39번이 있었다... 50 가자.


kCnt <- sapply( rstLst ,length )

maxIdx <- sapply( rstLst ,function( p ){
				if( 2>length(p) ) return( 1000 )
				return( p[[2]]$idx )
			})

