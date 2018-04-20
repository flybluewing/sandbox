# 20180109_D.R 교차모델


cutEadge.banReb3 <- function( gEnv ,allIdx ,pDebug=F ){

    allIdx.len <- length(allIdx)

	# flagLst.base, flag.base
    flagLst.base <- vector( "list" ,allIdx.len )
    lastZoid <- gEnv$zhF[nrow(gEnv$zhF) ,]
    for( idx in seq_len(allIdx.len) ){
        cnt <- sum( lastZoid %in% gEnv$allZoidMtx[allIdx[idx],] )
        if( cnt > 2 ){ 
            flagLst.base[[idx]] <- cnt
        }
    }
	flag.base <- sapply( flagLst.base ,function(p){ 0==length(p) })

	# flagLst.cv, flag.cv
    flagLst.cv <- vector( "list" ,allIdx.len )
    azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){sort(unique(p))})
    for( azColIdx in 1:6 ){
        for( vIdx in azColValLst[[azColIdx]] ){
            tRawMtx <- gEnv$zhF[gEnv$zhF[,azColIdx]==vIdx ,,drop=F]
            lastH <- tRawMtx[nrow(tRawMtx),]
            for( idx in seq_len(allIdx.len) ){
                if( vIdx != gEnv$allZoidMtx[allIdx[idx],azColIdx] ){
                    next
                }
                cnt <- sum( gEnv$allZoidMtx[allIdx[idx],] %in% lastH )
                if( cnt>2 ){
                    flagLst.cv[[idx]][[1+length(flagLst.cv[[idx]])]] <- c(azColIdx,vIdx,cnt)
                }
            }
        }
    }
	flag.cv <- sapply( flagLst.cv ,function(p){ 3>length(p) })

    rObj <- list( idStr="cutEadge.banReb3" )
    rObj$flag <- flag.base & flag.cv
	if( pDebug ){
		rObj$flagLst.base <- flagLst.base	;rObj$flagLst.cv <- flagLst.cv
	}
    return( rObj )

} # cutEadge.banReb3()



getChkRareObj <- function( pMtx ,pThld=0.07 ){

	# val.len 값이 너무 낮을 경우에 대한 보완이 필요하다.
	val.len <- nrow(pMtx)
	colValLst <- apply( pMtx ,2 ,function(p){sort(unique(p))})
	colTblLst <- apply( pMtx ,2 ,function(p){table(p)})
	freqMtxLst <- mapply( function( val ,tbl ){
								rbind( val , pThld > (tbl[as.character(val)]/val.len) )
							} 
					,colValLst ,colTblLst )
	
	rObj <- list( freqMtxLst=freqMtxLst ,thld=pThld )
	rObj$isRare <- function( pVal ,pIdx ){
		idxFlag <- rObj$freqMtxLst[[pIdx]][1,]==pVal
		return( rObj$freqMtxLst[[pIdx]][2,idxFlag] )
	} # rObj$isRare()

	return( rObj )

} # getRareObj()

