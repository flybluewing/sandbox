# 20180109_D.R 교차모델


cutEadge.getBanPtnColVal <- function( gEnv ,allIdx ,pDebug=F ){

	allIdx.len <- length(allIdx)
	valMtx <- gEnv$zhF

	azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){unique(p)} )
	flagLst.cv <- vector( "list" ,allIdx.len )
	banLst <- lapply( seq_len(allIdx.len) ,function(idx){ vector("list",6) })
	# banLst[[aIdx]][[colIdx]][["valIdx"]]  $colIdx $vIdx  $ptnLst $ptnIdx $chkCnt
	for( azColIdx in 1:6 ){
		for( vIdx in azColValLst[[azColIdx]] ){
			tValMtx <- valMtx[valMtx[,azColIdx]==vIdx ,,drop=F]
			banObj <- getBanPtn( tValMtx )
            if( is.null(banObj) ){
                next
            }
			banRst <- banObj$chkMatchAny( gEnv$allZoidMtx[allIdx,,drop=F] ,pExcCol=azColIdx ,pDebug=T )

			for( idx in seq_len(allIdx.len) ){
				if( 0==length(banRst$rstLst[[idx]]) ){
					next
				}
				zoid <- gEnv$allZoidMtx[ allIdx[idx] ,]
				if( zoid[azColIdx]!=vIdx ){
					next
				}
				flagLst.cv[[idx]][[ 1+length(flagLst.cv[[idx]]) ]] <- c(azColIdx,vIdx)

				banLst[[idx]][[azColIdx]][[as.character(vIdx)]] <-
						list( colIdx=azColIdx ,vIdx=vIdx 
								,ptnLst=banObj$ptnLst[ banRst$rstLst[[idx]] ]
								,ptnIdx=banRst$rstLst[[idx]] ,chkCnt=banRst$chkCntLst[[idx]] 
							)
                # 추가 제약조건 : azColIdx를 제외하고, 2개 컬럼 이상
                #                1개 뿐이면 희귀값인지 검토..
                #                희귀값이 아니더라도 azColIdx 2개에 걸쳐서 나왔다면 제외.
			}
		} # vIdx
	} # azColIdx

    rObj <- list( idStr="cutEadge.getBanPtnColVal" )
    rObj$flag <- 0==sapply(flagLst.cv,length)
    if( pDebug ){
        rObj$flagLst.cv <- flagLst.cv
    }
    return( rObj )

} # cutEadge.getBanPtnColVal()



kCnt <- sapply( flagLst.cv ,length )



