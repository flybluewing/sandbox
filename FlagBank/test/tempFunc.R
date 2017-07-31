# findOverlap <- function( pMatchInfo ,pDebug=F ){
#	pSL.std <- po.std$stageLst; pSL.mod <- po.mod$stageLst

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
							fMtx <- pSL.std[[idx]]$matchLst[[mIdx]]$mfIdxMtx != pSL.mod[[idx]]$matchLst[[mIdx]]$mfIdxMtx
							if( any(as.vector(fMtx)) ){
								return(sprintf(" mfIdxMtx is not match (idx:%d mIdx:%d)",idx,mIdx))
							}
							
							# mfMtx
							fMtx <- pSL.std[[idx]]$matchLst[[mIdx]]$mfMtx != pSL.mod[[idx]]$matchLst[[mIdx]]$mfMtx
							if( any(as.vector(fMtx)) ){
								return(sprintf(" mfMtx is not match (idx:%d mIdx:%d)",idx,mIdx))
							}
							
							# compPair
							fMtx <- pSL.std[[idx]]$matchLst[[mIdx]]$compPair != pSL.mod[[idx]]$matchLst[[mIdx]]$compPair
							if( any(as.vector(fMtx)) ){
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



