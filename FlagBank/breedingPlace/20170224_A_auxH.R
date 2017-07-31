




val.stageLst <- function( pSL.std ,pSL.mod ,pDebug=F ){

				if( length(pSL.std)!=length(pSL.mod) ){
					return("length is not same")
				}

				for( idx in 1:length(pSL.std) ){
					if( length(pSL.std[[idx]]$matchLst)!=length(pSL.mod[[idx]]$matchLst) ){
						return(sprintf(" matchLst size is not same. idx:%d",idx))
					}
					if( 0<length(pSL.std[[idx]]$matchLst) ){
						for( mIdx in 1:length(pSL.std[[idx]]$matchLst) ){
							# NA is removed by film.
							# mfIdxMtx
							dimChk <- dim(pSL.std[[idx]]$matchLst[[mIdx]]$mfIdxMtx) != dim(pSL.mod[[idx]]$matchLst[[mIdx]]$mfIdxMtx)
							if( is.na(dimChk) || is.null(dimChk) || (0==length(dimChk)) || any(dimChk) ){
									return(sprintf(" mfIdxMtx dim is not match (idx:%d mIdx:%d)",idx,mIdx))
							}
							fMtx <- pSL.std[[idx]]$matchLst[[mIdx]]$mfIdxMtx != pSL.mod[[idx]]$matchLst[[mIdx]]$mfIdxMtx
							if( any(as.vector(fMtx)) ){
								return(sprintf(" mfIdxMtx is not match (idx:%d mIdx:%d)",idx,mIdx))
							}
							
							# mfMtx
							dimChk <- dim(pSL.std[[idx]]$matchLst[[mIdx]]$mfMtx) != dim(pSL.mod[[idx]]$matchLst[[mIdx]]$mfMtx)
							if( is.na(dimChk) || is.null(dimChk) || (0==length(dimChk)) || any(dimChk) ){
									return(sprintf(" mfMtx dim is not match (idx:%d mIdx:%d)",idx,mIdx))
							}
							fMtx <- pSL.std[[idx]]$matchLst[[mIdx]]$mfMtx != pSL.mod[[idx]]$matchLst[[mIdx]]$mfMtx
							if( any(as.vector(fMtx)) ){
								return(sprintf(" mfMtx is not match (idx:%d mIdx:%d)",idx,mIdx))
							}
							
							# compPair
							dimChk <- dim(pSL.std[[idx]]$matchLst[[mIdx]]$compPair) != dim(pSL.mod[[idx]]$matchLst[[mIdx]]$compPair)
							if( is.na(dimChk) || is.null(dimChk) || (0==length(dimChk)) || any(dimChk) ){
									return(sprintf(" compPair dim is not match (idx:%d mIdx:%d)",idx,mIdx))
							}
							compPairStr.std <- sort(apply(pSL.std[[idx]]$matchLst[[mIdx]]$compPair ,1 ,function(p){paste(p,collapse=".")} ))
							compPairStr.mod <- sort(apply(pSL.mod[[idx]]$matchLst[[mIdx]]$compPair ,1 ,function(p){paste(p,collapse=".")} ))
							if( any(compPairStr.std!=compPairStr.mod) ){
								return(sprintf(" compPair is not match (idx:%d mIdx:%d)",idx,mIdx))
							}
							
							if( pDebug ){
								k.FLogStr(sprintf(" val.stageLst() idx:%d ,mIdx:%d",idx,mIdx))
							}
						}
					} # if
				}
				return("It's same. OK.")
		}

#	pMatch.raw : slideWindow의 matchLst.raw[[n]]
slideWindow.mlRaw.idx <- function( pMatch.raw ,pMatch.raw.idx ,pMatchLst ){
		# pMatch.raw.idx <- 1
		# pMatch.raw <- stage$matchLst.raw[[pMatch.raw.idx]]
		# pMatchLst <- stage$matchLst
		rFlag <- rep( F ,length(pMatchLst) )
		
		if( 0==length(pMatchLst) ){
			return( rFlag )
		}
		for( idx in 1:length(pMatchLst) ){
			if( pMatch.raw$matNum != nrow(pMatchLst[[idx]]$mfIdxMtx) )
				next
			
			if( pMatch.raw.idx %in% pMatchLst[[idx]]$comPair[,1] ){
				rFlag[idx] <- T
				next
			}
			if( pMatch.raw.idx %in% pMatchLst[[idx]]$comPair[,2] ){
				rFlag[idx] <- T
				next
			}
		}
		
		return( rFlag )
	}




# po.std 에서 compPair 값이 벡터와 매트릭스가 혼재되어서 있는 문제 처리.
# po <- po.std
fix.stdStageLst <- function( po ){
    k.FLogStr("Start",pAppend=F)
    for( sIdx in 1:length(po$stageLst) ){
        if( 0==length(po$stageLst[[sIdx]]$matchLst) ){
            next
        }
        for( mIdx in 1:length(po$stageLst[[sIdx]]$matchLst) ){
            # k.FLogStr(sprintf("   sIdx:%d   mIdx:%d ",sIdx,mIdx))
            if( "integer" == class(po$stageLst[[sIdx]]$matchLst[[mIdx]]$compPair) ){
                k.FLogStr(sprintf("   found sIdx:%d   mIdx:%d ",sIdx,mIdx))
                po$stageLst[[sIdx]]$matchLst[[mIdx]]$compPair <- matrix(po$stageLst[[sIdx]]$matchLst[[mIdx]]$compPair,nrow=1)
            }
        }
    } # sIdx

    k.FLogStr("Finish",pAppend=F)
}

# idx크기에 따른 compPair 수량 조사
#		1     2     3     4     5     6     7 
#	21247 14424  7344  3183   970   252    18 
spc.idxnum <- function( po ){
        mtxLst <- list()
        pStageLst <- po$stageLst
        for( sIdx in 1:length(pStageLst)){
            m.size <- length(pStageLst[[sIdx]]$matchLst)
            if( 0==m.size ){
                next
            }

            cName = c("sIdx","cp.nr","idx.num")
            mtx <- matrix(0,nrow=m.size,ncol=length(cName))
            colnames(mtx) <- cName
            mtx[,"sIdx"] <- sIdx

            for( mIdx in 1:m.size ){
                m <- pStageLst[[sIdx]]$matchLst[[mIdx]]
                mtx[mIdx,"cp.nr"]   <- nrow(m$compPair)
                mtx[mIdx,"idx.num"] <- nrow(m$mfIdxMtx)
            }
            mtxLst[[(1+length(mtxLst))]] <- mtx
        }


        t.mtx <- do.call( rbind ,mtxLst )

        k <- tapply( t.mtx[,"idx.num"], t.mtx[,"sIdx"], sum )
        return(k)
    }


