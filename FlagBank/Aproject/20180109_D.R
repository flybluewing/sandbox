# 20180109_D.R 교차모델


cutEadge.getBanStepRebCStep <- function( gEnv ,allIdx ,pDebug=F ){
	# 보완점 : flagLst.cv 에 대해서도 getChkRareObj()활용필요.

    scanPtn <- getPtnScanner()$stepReb
    allIdx.len <- length(allIdx)
    allCodeMtx <- gEnv$allZoidMtx[allIdx,2:6,drop=F] - gEnv$allZoidMtx[allIdx,1:5,drop=F]

	# flagLst.base ,flag.base ---------------------------------------------------------------
    flagLst.base <- vector("list",allIdx.len)
	flagLst.base.val <- vector("list",allIdx.len)
    stdCodeMtx <- gEnv$zhF[,2:6] - gEnv$zhF[,1:5]
    for( azColIdx in 1:5 ){
        banPtn <- scanPtn( stdCodeMtx[,azColIdx] )
        for( idx in seq_len(allIdx.len) ){
            if( allCodeMtx[idx,azColIdx]%in%banPtn ){
                flagLst.base[[idx]][[1+length(flagLst.base[[idx]])]] <- c( azColIdx )
				flagLst.base.val[[idx]][[1+length(flagLst.base.val[[idx]])]] <- allCodeMtx[idx,azColIdx]
            }
        }
    }
	rareObj <- getChkRareObj( stdCodeMtx ,pThld=0.07 )
	flag.base <- sapply( seq_len(allIdx.len) ,function(idx){
						if( 1<length(flagLst.base[[idx]]) ){
							return(FALSE)
						}
						if( 1==length(flagLst.base[[idx]]) ){
							# 한개 일치뿐이지만 rare한 값이라면 필터링 대상.(생존대상 아님.)
							rst <- rareObj$isRare( flagLst.base.val[[idx]] ,flagLst.base[[idx]] )
							return( rst==0 ) # 흔한 값의 일치이므로 생존으로 인정.(TRUE반납)
						}
						return( TRUE )
					})


	# flagLst.cv ,flag.cv ---------------------------------------------------------------
    flagLst.cv <- vector("list",allIdx.len)
    azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){sort(unique(p))})
    for( azColIdx in 1:6 ){
        for( vIdx in azColValLst[[azColIdx]] ){
            tRawMtx <- gEnv$zhF[gEnv$zhF[,azColIdx]==vIdx ,,drop=F]
            tStdMtx <- tRawMtx[,2:6,drop=F] - tRawMtx[,1:5,drop=F]
            for( colIdx in (1:ncol(tStdMtx)) ){
                banVal <- scanPtn( tStdMtx[,colIdx] )
                for( idx in seq_len(allIdx.len) ){
                    if( vIdx!=gEnv$allZoidMtx[allIdx[idx],azColIdx] ){
                        next
                    }
                    if( allCodeMtx[idx,colIdx]%in%banVal ){
                        flagLst.cv[[idx]][[1+length(flagLst.cv[[idx]])]] <- c(azColIdx,vIdx,colIdx)
                    }
                }
            }
        } # vIdx
    }
	flag.cv <- sapply( flagLst.cv ,function(p){1>=length(p)} )
	
    rObj <- list( idStr="cutEadge.getBanStepRebCStep" )
    rObj$flag <- flag.base | flag.cv
	if( pDebug ){
		rObj$flagLst.base <- flagLst.base
		rObj$flagLst.cv <- flagLst.cv
	}

    return( rObj )

} # cutEadge.getBanStepRebCStep()


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

