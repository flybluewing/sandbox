
filtRMF <- function( pRawMatch ){
					rNum <- length(unique(pRawMatch$idxMtx[,"row"]))
					if( 1>=rNum )
						return( F )
						
					return( T )
			}

# ==================================================================================================
# ==================================================================================================
# 개조
# 	pWin<-winDef;	pMatNum=3;	pCpMtxScale=200;	pFiltRMF=NULL;	pDebug=T
slideWindow <- function( pWin ,pMatNum=0 ,pCpMtxScale=100 ,pFiltRMF=NULL ,pDebug=F ){

			slideArea <- 1:(pWin$fieldDim[1]-pWin$height+1)
			wind <- pWin$field[pWin$initPos:(pWin$initPos+pWin$height-1),]
			matchLst <- list()
			for( saIdx in slideArea ){
				# saIdx <- 4
				matDefMtx <- pWin$field[saIdx:(saIdx+pWin$height-1),]
				if( saIdx!=pWin$initPos ){
					matDefMtx[wind != matDefMtx[,]] <- NA
				} else {
					matDefMtx[,] <- NA
				}
				rObj <- list(matMtx=matDefMtx)
				rObj$idx.fr <- saIdx
				rObj$matNum <- sum(!is.na(matDefMtx))
				cName <- c("his","row","col","val")
				rObj$idxMtx <- matrix( 0 ,nrow=rObj$matNum ,ncol=length(cName) )
				colnames(rObj$idxMtx) <- cName
				inputIdx <- 1
				for( rIdx in 1:nrow(matDefMtx) ){
					indices <- which(!is.na(matDefMtx[rIdx,]))
					if( pMatNum>length(indices) )
						next
					for( cIdx in indices ){
						rObj$idxMtx[inputIdx,] <- c(saIdx,rIdx,cIdx,rObj$matMtx[rIdx,cIdx])
						inputIdx <- inputIdx+1
					}
				}

				if( rObj$matNum>=pMatNum ){
					matchLst[[length(matchLst)+1]] <- rObj
				}

			} # for(saIdx)

			if( !is.null(filtRMF) ){
				filtF <- sapply(matchLst,filtRMF)
			}
			matchLst <- matchLst[filtF]
			# matNum <- sapply(matchLst,function(p){p$matNum})
			# matNum[order(matNum,decreasing=T)[1:20]]
			# matchLst <- matchLst[order(matNum,decreasing=T)[1:20]]

			dLogTerm <- 2000
			if( pDebug ){
				lterm <- length(matchLst) %/% 50
				lterm <- ifelse( 5>lterm ,5 ,lterm )
				dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
				k.FLogStr(sprintf("    matchLst length : %d (Log term:%d)",length(matchLst),dLogTerm))
			}

			matchLst.size <- length(matchLst)
			buffSize <- matchLst.size*pCpMtxScale
			cumCount <- 0;
			idStrs <- character(0)
			cumObjLst <- list()
			compPairLst <- list();	compPairLst.cost <- 0
			compPairMtx.val <- matrix(-1,ncol=2,nrow=buffSize) # compPair 관련 속도향상을 위해.
			compPairMtx.idx <- rep(-1,buffSize)	# 값이 입력된 곳은 자연히 정수일 것이므로.
			compPairMtx.cursor <- 1
			if( 0 < matchLst.size ){
				for( oIdx in matchLst.size:2 ){
					for( iIdx in (oIdx-1):1 ){
						mObj <- compareFilm( matchLst ,oIdx ,iIdx )
						if( 0==nrow(mObj$mfIdxMtx) )
							next
						
						saveIdx <- which(idStrs==mObj$idStr)
						if( 0==length(saveIdx) ){
							idStrs <- c(idStrs,mObj$idStr)
							cumObjLst[[length(cumObjLst)+1]] <- mObj
							saveIdx <- length(idStrs)
						}
						
						# k.FLogStr(sprintf("  oIdx:%d  iIdx:%d compPairMtx.cursor:%d of %d",oIdx,iIdx,compPairMtx.cursor,buffSize))
						compPairMtx.idx[compPairMtx.cursor] <- saveIdx
						compPairMtx.val[compPairMtx.cursor,] <- mObj$compPair
						compPairMtx.cursor <- compPairMtx.cursor+1
						if( buffSize < compPairMtx.cursor ){
							# k.FLogStr(sprintf("  ERROR!! compPairMtx.val overflow (pCpMtxScale:%d)",pCpMtxScale),pConsole=T)
							cpTStamp <- Sys.time()
							cpObj <- list( compPairMtx.val=compPairMtx.val )
							cpObj$compPairMtx.idx <- compPairMtx.idx
							compPairLst[[(1+length(compPairLst))]] <- cpObj
							compPairMtx.val[,] <- -1;	compPairMtx.idx[] <- -1;	compPairMtx.cursor <- 1
							compPairLst.cost <- compPairLst.cost + (Sys.time()-cpTStamp)
							k.FLogStr(sprintf("  compPairMtx.val overflow (comPairLst:%d)",length(compPairLst)),pConsole=T)
						}

						cumCount <- cumCount+1
					} # for(iIdx)
					if( pDebug ){
						if( 0==(oIdx%%dLogTerm) )
							k.FLogStr(sprintf("       oIdx : %d  compPairMtx.cursor:%d(%d%%)",oIdx,compPairMtx.cursor,(compPairMtx.cursor*100)%/%buffSize))
					}
				}
			} # if

			tStamp <- Sys.time()
			compPairMtx.val <- compPairMtx.val[(0<compPairMtx.idx),]
			compPairMtx.idx <- compPairMtx.idx[(0<compPairMtx.idx)]
			if( 0 < length(cumObjLst) ){
				for( idx in 1:length(cumObjLst) ){
					cumObjLst[[idx]]$compPair <- compPairMtx.val[compPairMtx.idx==idx,,drop=F]
					if( 0<length(compPairLst) ){
						for( lIdx in 1:length(compPairLst) ){
							cumObjLst[[idx]]$compPair <- rbind( cumObjLst[[idx]]$compPair, compPairLst[[lIdx]]$compPairMtx.val[compPairLst[[lIdx]]$compPairMtx.idx==idx,,drop=F] )
						}
					}
				} # for(idx)
			}
			if( pDebug ){
				k.FLogStr(sprintf("   time cost : %.1f (comPairLst:%d create cost:%.1f)",Sys.time()-tStamp,length(compPairLst),compPairLst.cost),pConsole=T)
			}

			rObj <- list( depth=1 )
			rObj$cumCount=cumCount
			rObj$idStrs <- idStrs
			rObj$matchLst <- cumObjLst
			rObj$matchLst.raw <- matchLst
			rObj$idStr.winDef <- pWin$idStr
			rObj$timeStamp <- Sys.time()

			return( rObj )
	}

# ======================================================================================================
# 개조
#	findOverlap( )
#		pMCsub : 병렬처리를 위한 서브루틴으로 실행하는 모드
#			matchLst들은 compPair를 갖지 않는 대신, 전체데이터(compPairMtx.val)가 반환된다.
#	pMatchInfo=stage; pMCsub=F; pMatNum=1; pCpMtxScale=100; pDebug=F
findOverlap <- function( pMatchInfo ,pMCsub=F ,pMatNum=0 ,pCpMtxScale=100 ,pDebug=F ){

						matchLst <- pMatchInfo$matchLst
						dLogTerm <- 2000
						if( pDebug ){
							lterm <- length(matchLst) %/% 50
							lterm <- ifelse( 5>lterm ,5 ,lterm )
							dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
							k.FLogStr(sprintf("    matchLst length : %d (Log term:%d)",length(matchLst),dLogTerm))
						}

						matchLst.size <- length(matchLst)
						buffSize <- matchLst.size*pCpMtxScale
						cumCount <- 0;
						idStrs <- character(0)
						cumObjLst <- list()
						compPairLst <- list();	compPairLst.cost <- 0
						compPairMtx.val <- matrix( -1,ncol=2,nrow=buffSize ) # compPair 관련 속도향상을 위해.
						compPairMtx.idx <- rep( -1, buffSize )	# 값이 입력된 곳은 자연히 정수일 것이므로.
						compPairMtx.cursor <- 1

						for( oIdx in length(matchLst):2 ){
							for( iIdx in (oIdx-1):1 ){
								mObj <- compareFilm.lf( matchLst ,oIdx ,iIdx )
								if( pMatNum > nrow(mObj$mfIdxMtx) )
									next

								saveIdx <- which(idStrs==mObj$idStr)
								if( 0==length(saveIdx) ){
									idStrs <- c(idStrs,mObj$idStr)
									cumObjLst[[length(cumObjLst)+1]] <- mObj
									saveIdx <- length(idStrs)
								}
								# k.FLogStr(sprintf("  oIdx:%d  iIdx:%d",oIdx,iIdx))
								compPairMtx.idx[compPairMtx.cursor] <- saveIdx
								compPairMtx.val[compPairMtx.cursor,] <- mObj$compPair
								compPairMtx.cursor <- compPairMtx.cursor+1
								if( buffSize < compPairMtx.cursor ){
									# k.FLogStr(sprintf("  ERROR!! compPairMtx.val overflow (pCpMtxScale:%d)",pCpMtxScale),pConsole=T)
									cpTStamp <- Sys.time()
									cpObj <- list( compPairMtx.val=compPairMtx.val )
									cpObj$compPairMtx.idx <- compPairMtx.idx
									compPairLst[[(1+length(compPairLst))]] <- cpObj
									compPairMtx.val[,] <- -1;	compPairMtx.idx[] <- -1;	compPairMtx.cursor <- 1
									compPairLst.cost <- compPairLst.cost + (Sys.time()-cpTStamp)
									k.FLogStr(sprintf("  compPairMtx.val overflow (comPairLst:%d)",length(compPairLst)),pConsole=T)
								}

								cumCount <- cumCount+1
							} # for(iIdx)
							if( pDebug ){
								if( 0==(oIdx%%dLogTerm) )
									k.FLogStr(sprintf("       oIdx : %d  compPairMtx.cursor:%d(%d%%)",oIdx,compPairMtx.cursor,(compPairMtx.cursor*100)%/%buffSize))
							}
						}

						tStamp <- Sys.time()
						compPairMtx.val <- compPairMtx.val[(0<compPairMtx.idx),]
						compPairMtx.idx <- compPairMtx.idx[(0<compPairMtx.idx)]
						if( !pMCsub ){ # 병렬 처리를 위한 서브루틴이 아닌 경우.
							if( 0 < length(cumObjLst) ){
								for( idx in 1:length(cumObjLst) ){
									cumObjLst[[idx]]$compPair <- compPairMtx.val[compPairMtx.idx==idx,,drop=F]
									if( 0<length(compPairLst) ){
										for( lIdx in 1:length(compPairLst) ){
											cumObjLst[[idx]]$compPair <- rbind( cumObjLst[[idx]]$compPair, compPairLst[[lIdx]]$compPairMtx.val[compPairLst[[lIdx]]$compPairMtx.idx==idx,,drop=F] )
										}
									}
								} # for(idx)
							}
						}
						if( pDebug ){
							k.FLogStr(sprintf("   time cost : %.1f (comPairLst:%d create cost:%.1f)",Sys.time()-tStamp,length(compPairLst),compPairLst.cost),pConsole=T)
						}

						rObj <- list( depth=(pMatchInfo$depth+1) )
						rObj$cumCount=cumCount							
						rObj$idStrs <- idStrs
						rObj$matchLst <- cumObjLst
						rObj$idStr.winDef <- pMatchInfo$idStr.winDef
						rObj$timeStamp <- Sys.time()
						if( pMCsub ){
							rObj$compPairMtx.val <- compPairMtx.val
							rObj$compPairMtx.idx <- compPairMtx.idx
						}

						return( rObj )
				}


# 개조
compareFilm <- function( pMatchLst ,pIdxA ,pIdxB ){

		mfMtx <- (!is.na(pMatchLst[[pIdxA]]$matMtx)) & (!is.na(pMatchLst[[pIdxB]]$matMtx)) 
			# match flag mtx

		cName <- c("row","col")
		mfIdxMtx <- matrix( 0 ,nrow=sum(mfMtx) ,ncol=length(cName) )
		mtxInputIdx <- 1
		for( rIdx in 1:nrow(mfMtx) ){
			cSpan <- which( mfMtx[rIdx,] )
			if(0==length(cSpan))
				next
			for( cIdx in cSpan ){
				mfIdxMtx[mtxInputIdx,] <- c(rIdx,cIdx)
				mtxInputIdx <- mtxInputIdx+1
			}
		}
		colnames(mfIdxMtx) <- cName

		# idxStr <- apply(mfIdxMtx,1,function(p){paste(sprintf("%04d",p),collapse="")})
		idxStr <- apply(mfIdxMtx,1,function(p){paste(p,collapse=".")})
		rMObj <- list( idStr = paste(idxStr,collapse=".") )
		rMObj$mfIdxMtx <- mfIdxMtx
		rMObj$mfMtx <- mfMtx
		rMObj$compPair <- sort(c(pIdxA,pIdxB))
		
		return( rMObj )
	}


# 개조
compareFilm.lf <- function( pMatchLst ,pIdxA ,pIdxB ){

		mfMtx <- (pMatchLst[[pIdxA]]$mfMtx & pMatchLst[[pIdxB]]$mfMtx)
			# match flag mtx

		cName <- c("row","col")
		mfIdxMtx <- matrix( 0 ,nrow=sum(mfMtx) ,ncol=length(cName) )
		mtxInputIdx <- 1
		for( rIdx in 1:nrow(mfMtx) ){
			cSpan <- which( mfMtx[rIdx,] )
			if(0==length(cSpan))
				next
			for( cIdx in cSpan ){
				mfIdxMtx[mtxInputIdx,] <- c(rIdx,cIdx)
				mtxInputIdx <- mtxInputIdx+1
			}
		}
		colnames(mfIdxMtx) <- cName

		# idxStr <- apply(mfIdxMtx,1,function(p){paste(sprintf("%04d",p),collapse="")})
		idxStr <- apply(mfIdxMtx,1,function(p){paste(p,collapse=".")})
		rMObj <- list( idStr = paste(idxStr,collapse="") )
		rMObj$mfIdxMtx <- mfIdxMtx
		rMObj$mfMtx <- mfMtx
		rMObj$compPair <- sort(c(pIdxA,pIdxB))

		return( rMObj )
	}

