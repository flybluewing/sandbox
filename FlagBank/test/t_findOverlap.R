# ======================================================================================================
#원본
findOverlap <- function( pMatchInfo ,pDebug=F ){

						matchLst <- pMatchInfo$matchLst
						dLogTerm <- 2000
						if( pDebug ){
							lterm <- length(matchLst) %/% 50
							lterm <- ifelse( 5>lterm ,5 ,lterm )
							dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
							k.FLogStr(sprintf("    matchLst length : %d (Log term:%d)",length(matchLst),dLogTerm))
						}

						cumCount <- 0;
						idStrs <- character(0)
						cumObjLst <- list()
						for( oIdx in length(matchLst):2 ){
							for( iIdx in (oIdx-1):1 ){
								mObj <- compareFilm.lf( matchLst ,oIdx ,iIdx )
								if( 0==nrow(mObj$mfIdxMtx) )
									next

								saveIdx <- which(idStrs==mObj$idStr)
								if( 0==length(saveIdx) ){
									idStrs <- c(idStrs,mObj$idStr)
									cumObjLst[[length(cumObjLst)+1]] <- mObj
								} else {
									cumObjLst[[saveIdx]]$compPair <- rbind(cumObjLst[[saveIdx]]$compPair, mObj$compPair)
								}
								cumCount <- cumCount+1
							} # for(iIdx)

							if( pDebug ){
								if( 0==(oIdx%%dLogTerm) )
									k.FLogStr(sprintf("       oIdx : %d",oIdx))
							}
						}

						rObj <- list( depth=(pMatchInfo$depth+1) )
						rObj$cumCount=cumCount
						rObj$idStrs <- idStrs
						rObj$matchLst <- cumObjLst
						rObj$idStr.winDef <- pMatchInfo$idStr.winDef
						rObj$timeStamp <- Sys.time()
						return( rObj )
				}


# ======================================================================================================
# 개조
findOverlap <- function( pMatchInfo ,pDebug=F ){

						matchLst <- pMatchInfo$matchLst
						dLogTerm <- 2000
						if( pDebug ){
							lterm <- length(matchLst) %/% 50
							lterm <- ifelse( 5>lterm ,5 ,lterm )
							dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
							k.FLogStr(sprintf("    matchLst length : %d (Log term:%d)",length(matchLst),dLogTerm))
						}

						cumCount <- 0;
						idStrs <- character(0)
						cumObjLst <- list()
						for( oIdx in length(matchLst):2 ){
							for( iIdx in (oIdx-1):1 ){
								mObj <- compareFilm.lf( matchLst ,oIdx ,iIdx )
								if( 0==nrow(mObj$mfIdxMtx) )
									next

								saveIdx <- which(idStrs==mObj$idStr)
								if( 0==length(saveIdx) ){
									idStrs <- c(idStrs,mObj$idStr)
									cumObjLst[[length(cumObjLst)+1]] <- mObj
								} else {
									# cumObjLst[[saveIdx]]$compPair <- rbind(cumObjLst[[saveIdx]]$compPair, mObj$compPair)
									cumObjLst[[saveIdx]]$compPairLst[[(1+length(cumObjLst[[saveIdx]]$compPairLst))]] <- mObj$compPairLst[[1]]

								}
								cumCount <- cumCount+1
							} # for(iIdx)
							if( pDebug ){
								if( 0==(oIdx%%dLogTerm) )
									k.FLogStr(sprintf("       oIdx : %d",oIdx))
							}
						}

						if( 0 < length(cumObjLst) ){
							for( idx in 1:length(cumObjLst) ){
								if( is.null(cumObjLst[[idx]]$compPairLst) )
									next

								totNRow <- length(cumObjLst[[idx]]$compPairLst)
								compPair <- matrix(0,nrow=totNRow,ncol=length(cumObjLst[[idx]]$compPairLst[[1]]))					
								for( lIdx in 1:length(cumObjLst[[idx]]$compPairLst) ){
									compPair[lIdx,] <- cumObjLst[[idx]]$compPairLst[[lIdx]]
								}
								cumObjLst[[idx]]$compPair <- compPair
								cumObjLst[[idx]]$compPairLst <- NULL
							} # for(idx)
						}

						rObj <- list( depth=(pMatchInfo$depth+1) )
						rObj$cumCount=cumCount
						rObj$idStrs <- idStrs
						rObj$matchLst <- cumObjLst
						rObj$idStr.winDef <- pMatchInfo$idStr.winDef
						rObj$timeStamp <- Sys.time()
						return( rObj )
				}


# ======================================================================================================
# 개조 2






