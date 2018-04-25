# 20180109_D.R 교차모델


localHisMtx <- gEnv$zhF[gEnv$zhF[,1]%in%4 ,]

pMtx <- localHisMtx[,2:6] %% 10
pMtx <- gEnv$zhF %% 10

colStr <- c("hIdx","size")
rstMtx <- matrix( 0 ,nrow=nrow(pMtx) ,ncol=length(colStr) )	;colnames(rstMtx)<-colStr
rstLst <- list()
for( hIdx in 2:nrow(pMtx) ){
	idx <- which( pMtx[hIdx,]==pMtx[(hIdx-1),] )
	idxLen <- length(idx)
	if( 1<idxLen ){
		cStep <- idx[2:idxLen] - idx[1:(idxLen-1)]
		if( 1<sum(cStep==1) ){
			rstMtx[hIdx,] <- c(hIdx,sum(cStep==1))
			rstObj <- list( hIdx=hIdx, zoidMtx=rbind(pMtx[hIdx-1,],pMtx[hIdx,]) )
			rstLst[[1+length(rstLst)]] <- rstObj
		}
	}
}
#	zhF 에 대해서는 3개 연속,
#	localHisMtx 에 대해서는 2개 연속을 제거하기로 한다.

table( rstMtx[,"size"] )

chkIdx <- which(rstMtx[,"hIdx"]>0)



chkPtnObj <- function(){
	# 이들은 각각 사이즈가 제한되어야 할 듯.(용도에 따라)
	rObj <- list()
	rObj$rebPtn <- function( pVal ){
		# 1,2,3,1,2,3
		
	} # rObj$rebPtn()
	rObj$symPtn <- function( pVal ){
		# 1,2,3,2,1  or 1,2,2,1
	} # rObj$symPtn
	rObj$gradPtn <- function( pVal ){
		# 1,2,3,4,5
	}
	return( rObj )
} # chkPtnObj()


cutEadge.banReb10RemSeq <- function( gEnv ,allIdx ,pDebug=F ){
	# %% 10 이 연달아 3개 같은 패턴 재현

	# pValMtx <- tail(gEnv$zhF%%10)	;pBase <- c( 2,0,2,8,7,9 )
	chk3Seq <- function( pValMtx ,pBase ,pSize=3 ){
		valLen <- ncol(pValMtx)
		baseLen <- length(pBase)
		spanBase <- 1:pSize - 1
		flag <- rep( NA ,nrow(pValMtx) )
		for( idx in seq_len(nrow(pValMtx)) ){
			for( aIdx in 1:(valLen-pSize+1) ){
				for( bIdx in 1:(baseLen-pSize+1) ){
					aSpan <- pValMtx[idx,aIdx+spanBase]
					bSpan <- pBase[bIdx+spanBase]
					if( all(aSpan==bSpan) ){
						flag[idx] <- bIdx
						break
					}
				} # bIdx
			} # aIdx
		} # idx

		return( flag )
	} # chk3Seq()

    allIdx.len <- length(allIdx)

	flag <- chk3Seq( gEnv$allZoidMtx[allIdx,,drop=F]%%10 
						,gEnv$zhF[nrow(gEnv$zhF),]%%10 
						,pSize=3 
					)
	flag.base <- is.na(flag)
	flagLst.base <- lapply( flag.base ,function(p){ if(p) integer(0) else 1 })

    flagLst.cv <- vector("list",allIdx.len)
    azColValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){sort(unique(p))})
    for( azColIdx in 1:6 ){
        for( vIdx in azColValLst[[azColIdx]] ){
            tRawMtx <- gEnv$zhF[gEnv$zhF[,azColIdx]==vIdx ,,drop=F]
            tRawMtx <- tRawMtx[,-azColIdx,drop=F]
			if( 0==nrow(tRawMtx) ){
				next
			}
			flag <- chk3Seq( gEnv$allZoidMtx[allIdx,-azColIdx,drop=F]%%10 
							,tRawMtx[nrow(tRawMtx),] %% 10
							,pSize=3
						)
            for( idx in seq_len(allIdx.len) ){
                if( vIdx!=gEnv$allZoidMtx[allIdx[idx],azColIdx] ){
                    next
                }
				if( !is.na(flag[idx]) ){
					flagLst.cv[[idx]][[1+length(flagLst.cv[[idx]])]] <- c(azColIdx,vIdx)
				}
			}
        } # vIdx
    }
	flag.cv <- 1 > sapply( flagLst.cv ,length ) 

    rObj <- list( idStr="cutEadge.banReb10RemSeq" )
    rObj$flag <- flag.base & flag.cv
	if( pDebug ){
		rObj$flagLst.base	<- flagLst.base
		rObj$flagLst.cv		<- flagLst.cv
	}
    return( rObj )

} # cutEadge.banReb10RemSeq()


kCnt <- sapply( flagLst.cv ,length )

            for( idx in seq_len(allIdx.len) ){
                if( vIdx!=gEnv$allZoidMtx[allIdx[idx],azColIdx] ){
                    next
                }
                fndLst <- fnd3SeqReb( tRawMtx ,gEnv$allZoidMtx[allIdx[idx],-azColIdx] ,pDepth=50 ,pSrcNum=3  )
                # fndLst <- fnd2SeqReb( tRawMtx ,gEnv$allZoidMtx[allIdx[idx],-azColIdx] ,pDepth=50 ,pSrcNum=2  )
                if( 2<=length(fndLst) ){
                    fndLst[[1]]$azColIdx <- azColIdx
                    fndLst[[1]]$vIdx <- vIdx
                    flagLst.cv[[idx]][[1+length(flagLst.cv[[idx]])]] <- fndLst
                }
            }
