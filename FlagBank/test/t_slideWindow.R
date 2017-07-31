# 원본
slideWindow <- function( pWin ){
			slideArea <- pWin$height:pWin$fieldDim[1]
			wind <- pWin$field[pWin$initPos:(pWin$initPos-pWin$height+1),]
			matchLst <- list()
			for( saIdx in slideArea ){
				# saIdx <- 4
				matDefMtx <- pWin$field[saIdx:(saIdx-pWin$height+1),]
				if( saIdx!=pWin$initPos ){
					matDefMtx[wind != matDefMtx[,]] <- NA
				} else {
					matDefMtx[,] <- NA
				}
				rObj <- list(matMtx=matDefMtx)
				rObj$matNum <- sum(!is.na(matDefMtx))
				cName <- c("his","row","col","val")
				rObj$idxMtx <- matrix( 0 ,nrow=rObj$matNum ,ncol=length(cName) )
				colnames(rObj$idxMtx) <- cName
				inputIdx <- 1
				for( rIdx in 1:nrow(matDefMtx) ){
					indices <- which(!is.na(matDefMtx[rIdx,]))
					if( 0==length(indices) )
						next
					for( cIdx in indices ){
						rObj$idxMtx[inputIdx,] <- c(saIdx,rIdx,cIdx,rObj$matMtx[rIdx,cIdx])
						inputIdx <- inputIdx+1
					}
				}

				matchLst[[length(matchLst)+1]] <- rObj
			} # for(saIdx)

			matNum <- sapply(matchLst,function(p){p$matNum})
			# matNum[order(matNum,decreasing=T)[1:20]]
			# matchLst <- matchLst[order(matNum,decreasing=T)[1:20]]

			cumCount <- 0;
			idStrs <- character(0)
			cumObjLst <- list()
			for( oIdx in length(matchLst):2 ){
				for( iIdx in (oIdx-1):1 ){
					mObj <- compareFilm( matchLst ,oIdx ,iIdx )
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
			}

			rObj <- list( depth=1 )
			rObj$cumCount=cumCount
			rObj$idStrs <- idStrs
			rObj$matchLst <- cumObjLst
			rObj$idStr.winDef <- pWin$idStr
			rObj$timeStamp <- Sys.time()

			return( rObj )
	}


# ==================================================================================================
# ==================================================================================================
# 개조


slideWindow <- function( pWin ){
			slideArea <- pWin$height:pWin$fieldDim[1]
			wind <- pWin$field[pWin$initPos:(pWin$initPos-pWin$height+1),]
			matchLst <- list()
			for( saIdx in slideArea ){
				# saIdx <- 4
				matDefMtx <- pWin$field[saIdx:(saIdx-pWin$height+1),]
				if( saIdx!=pWin$initPos ){
					matDefMtx[wind != matDefMtx[,]] <- NA
				} else {
					matDefMtx[,] <- NA
				}
				rObj <- list(matMtx=matDefMtx)
				rObj$matNum <- sum(!is.na(matDefMtx))
				cName <- c("his","row","col","val")
				rObj$idxMtx <- matrix( 0 ,nrow=rObj$matNum ,ncol=length(cName) )
				colnames(rObj$idxMtx) <- cName
				inputIdx <- 1
				for( rIdx in 1:nrow(matDefMtx) ){
					indices <- which(!is.na(matDefMtx[rIdx,]))
					if( 0==length(indices) )
						next
					for( cIdx in indices ){
						rObj$idxMtx[inputIdx,] <- c(saIdx,rIdx,cIdx,rObj$matMtx[rIdx,cIdx])
						inputIdx <- inputIdx+1
					}
				}

				matchLst[[length(matchLst)+1]] <- rObj
			} # for(saIdx)

			matNum <- sapply(matchLst,function(p){p$matNum})
			# matNum[order(matNum,decreasing=T)[1:20]]
			# matchLst <- matchLst[order(matNum,decreasing=T)[1:20]]

			cumCount <- 0;
			idStrs <- character(0)
			cumObjLst <- list()
			for( oIdx in length(matchLst):2 ){
				for( iIdx in (oIdx-1):1 ){
					mObj <- compareFilm( matchLst ,oIdx ,iIdx )
					if( 0==nrow(mObj$mfIdxMtx) )
						next
					
					saveIdx <- which(idStrs==mObj$idStr)
					if( 0==length(saveIdx) ){
						idStrs <- c(idStrs,mObj$idStr)
						cumObjLst[[length(cumObjLst)+1]] <- mObj
					} else {
						#cumObjLst[[saveIdx]]$compPair <- rbind(cumObjLst[[saveIdx]]$compPair, mObj$compPair)
						cumObjLst[[saveIdx]]$compPairLst[[(1+length(cumObjLst[[saveIdx]]$compPairLst))]] <- mObj$compPairLst[[1]]
					}
					cumCount <- cumCount+1
				} # for(iIdx)
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

			rObj <- list( depth=1 )
			rObj$cumCount=cumCount
			rObj$idStrs <- idStrs
			rObj$matchLst <- cumObjLst
			rObj$idStr.winDef <- pWin$idStr
			rObj$timeStamp <- Sys.time()

			return( rObj )
	}

# ==================================================================================================
# ==================================================================================================
# 개조 2

	
# kkk -----------------------------------------------------------------------
# kkk -----------------------------------------------------------------------

