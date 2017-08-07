#
#

filtF.lastRowMiss <- function( pMatPtn ,pIdxMtx ){
		rIdx <- unique(pIdxMtx[,1])
		return( (1>=length(rIdx)) || !(nrow(pMatPtn) %in% rIdx) )
	}
filtF.limitedRow <- function( pMatPtn ,pIdxMtx ){

		rIdx <- unique(pIdxMtx[,1])
		predNum <- sum( pIdxMtx[,1]==nrow(pMatPtn) )

		return( 	1 >=length(rIdx)
				||	0 == predNum
				||	( (2*predNum) > nrow(pIdxMtx) )
			)
	}

scanRawPattern.filtF <- function( pMatPtn ,pIdxMtx ){
					rNum <- length(unique(pIdxMtx[,"row"]))
					if( 1>=rNum ){
						return( T )
					}

					return( F )
			}


getIdxMtx <- function( pMtx ){

		cName <- c("row","col","val")
		rIdxMtx <- matrix( 0 ,nrow=sum(!is.na(pMtx)) ,ncol=length(cName) )
		colnames(rIdxMtx) <- cName

		iCur <- 1 # input position cursor
		for( rIdx in seq_len(nrow(pMtx)) ){ # rIdx <- 1
			valIdxSpan <- which( !is.na(pMtx[rIdx,]) )
			for( cIdx in valIdxSpan ){ # cIdx <- valIdxSpan[1]
				rIdxMtx[iCur,"row"] <- rIdx
				rIdxMtx[iCur,"col"] <- cIdx
				rIdxMtx[iCur,"val"] <- pMtx[rIdx,cIdx]
				iCur <- iCur + 1
			} # for(cIdx)
		} # for(rIdx)

		return( rIdxMtx )
	}


# pWin=winDef;	pFiltF=filtF.limitedRow;	pBuffSize=1;	pDebug=T
scanRawPattern <- function( pWin ,pFiltF=NULL ,pBuffSize=1 ,pDebug=F ){

			dLogTerm <- 2000
			if( pDebug ){
				lterm <- pWin$fieldDim[1] %/% 50
				lterm <- ifelse( 50>lterm ,50 ,lterm )
				dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
				k.FLogStr(sprintf("    initPos : %d fieldDim size : %d (Log term:%d)",pWin$initPos,pWin$fieldDim[1],dLogTerm))
			}

			slideArea <- 1:(pWin$fieldDim[1]-pWin$height+1)
			stdField <- pWin$field[pWin$initPos:(pWin$initPos+pWin$height-1),]
			stdField.naFilm <- is.na(stdField)
			if( 0==sum(stdField.naFilm) ){
				stdField.naFilm <- NULL
			}
			fieldDim <- dim(stdField)

			matchLst <- list();		matIdStr<-character(0)
			cName.idxMtx <- c("row","col","val")
			compPairLst <- list();
			compPairMtx.val <- matrix(-1,ncol=2,nrow=pWin$fieldDim[1])
			colnames(compPairMtx.val) <- c("hIdx","dupIdx")
			for( hIdx in slideArea ){ # hIdx <- slideArea[1]
			
				if( pWin$initPos == hIdx )
					next

				curField <- pWin$field[hIdx:(hIdx+pWin$height-1),]
				if( !is.null(stdField.naFilm) )
					curField[stdField.naFilm] <- NA
				curField[curField!=stdField] <- NA
				
				# ------------
				matNum <- sum(!is.na(curField))
				if(0==matNum)
					next

				idxMtx <- matrix( 0 ,nrow=matNum ,ncol=length(cName.idxMtx) )
				colnames(idxMtx) <- cName.idxMtx
				inputCur <- 1
				for( rIdx in 1:fieldDim[1] ){
					fIdx <- which(!is.na(curField[rIdx,]))
					if( 0==length(fIdx) )
						next
					for( cIdx in fIdx ){
						idxMtx[inputCur,1] <- rIdx;	idxMtx[inputCur,2] <- cIdx;	idxMtx[inputCur,3] <- curField[rIdx,cIdx]
						inputCur <- inputCur+1
					}
				} # for

				if( !is.null(pFiltF) ){
					if( pFiltF( curField, idxMtx ) )
						next
				}
				
				idStr <- apply(idxMtx,1,function(p){paste(p[1:2],collapse=".")})
				idStr <- paste( idStr ,collapse=" " )

				compPairMtx.val[hIdx ,"hIdx"] <- hIdx
				dupId <- which(matIdStr==idStr)
				if( 0==length(dupId) ){
					rObj <- list(idStr=idStr)
					rObj$idxMtx <- idxMtx;	rObj$matNum <- matNum;	rObj$hIdx <- hIdx
					rObj$matMtx <- curField
					matIdStr <- c(matIdStr,idStr)
					matchLst[[(1+length(matchLst))]] <- rObj
					compPairMtx.val[hIdx ,"dupIdx"] <- hIdx
				} else {
					compPairMtx.val[hIdx ,"dupIdx"] <- matchLst[[(dupId[1])]]$hIdx
				}

				if( pDebug ){
					if( 0==(hIdx%%dLogTerm) )
						k.FLogStr(sprintf("       [initPos:%3d]hIdx : %d of %d",pWin$initPos,hIdx,pWin$fieldDim[1]))
				}
				
			} # for( hIdx )

			if( 0<length(matchLst)){
				for( lIdx in 1:length(matchLst) ){
					matchLst[[lIdx]]$compPair <- compPairMtx.val[ compPairMtx.val[,"dupIdx"]==matchLst[[lIdx]]$hIdx ,"hIdx"]
				}
			}

			rObj <- list( idStr.winDef=pWin$idStr ,initPos=pWin$initPos )
			rObj$buffSize <- pBuffSize
			rObj$matchLst <- matchLst
			rObj$timeStamp <- Sys.time()
			
			return(rObj)
			
		} # scanRawPattern()



# pWin=winDef ;pPtnObj=ptnObj ;pFiltF=NULL ;pBuffSize=1 ;pDebug=T
scanRawPattern.ptnLst <- function( pWin ,pPtnObj ,pFiltF=NULL ,pBuffSize=1 ,pDebug=F ){

			dLogTerm <- 2000
			if( pDebug ){
				lterm <- pWin$fieldDim[1] %/% 50
				lterm <- ifelse( 50>lterm ,50 ,lterm )
				dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
				k.FLogStr(sprintf("    initPos : %d fieldDim size : %d (Log term:%d)",pWin$initPos,pWin$fieldDim[1],dLogTerm))
			}

			ptnDim <- dim(pPtnObj$matMtx)
			slideArea <- 1:(pWin$fieldDim[1]-ptnDim[1]+1)
			stdField <- pPtnObj$matMtx
			stdField.naFilm <- is.na(stdField)
			stdField.itemSize <- sum( !stdField.naFilm )
			stdField.dim <- dim(stdField)
			# fieldDim <- dim(stdField)

			cName.idxMtx <- c("row","col","val")
			logMtx.f <- xor(pPtnObj$film, stdField.naFilm)
			idxMtx <- matrix( NA ,nrow=sum(logMtx.f) ,ncol=length(cName.idxMtx) )
			colnames(idxMtx) <- cName.idxMtx
			idxMtx.cur <- 1
			for( rIdx in 1:nrow(logMtx.f) ){
				cIndices <- which(logMtx.f[rIdx,])
				if( 0==length(cIndices) )
					next
				for( cIdx in cIndices ){
					idxMtx[idxMtx.cur,"row"] <- rIdx
					idxMtx[idxMtx.cur,"col"] <- cIdx
					idxMtx.cur <- idxMtx.cur+1
				}
			}

			matchLst <- list();		matIdStr<-character(0)
			compPairLst <- list();
			compPairMtx.val <- matrix(-1,ncol=2,nrow=pWin$fieldDim[1]) # compPair ???? ???????? ????.
			colnames(compPairMtx.val) <- c("hIdx","dupIdx")
			for( hIdx in slideArea ){ # hIdx <- slideArea[1]

				curField <- pWin$field[hIdx:(hIdx+ptnDim[1]-1),]

				# stdField ?? ??????? ???????? ???.
				if( stdField.itemSize != sum(stdField==curField, na.rm=T) )
					next

				curField[pPtnObj$film] <- NA

				for( rIdx in 1:nrow(idxMtx) ){
					idxMtx[rIdx,"val"] <- curField[idxMtx[rIdx,"row"],idxMtx[rIdx,"col"]]
				}

				if( !is.null(pFiltF) ){
					if( pFiltF( curField, idxMtx ) )
						next
				}

				idStr <- apply(idxMtx ,1 ,paste ,collapse=".")
				idStr <- paste( idStr ,collapse=" " )

				compPairMtx.val[hIdx ,"hIdx"] <- hIdx
				dupId <- which(matIdStr==idStr)
				if( 0==length(dupId) ){
					rObj <- list(idStr=idStr)
					rObj$idxMtx <- idxMtx;	rObj$hIdx <- hIdx
					rObj$matMtx <- curField
					matIdStr <- c(matIdStr,idStr)
					matchLst[[(1+length(matchLst))]] <- rObj
					compPairMtx.val[hIdx ,"dupIdx"] <- hIdx
				} else {
					compPairMtx.val[hIdx ,"dupIdx"] <- matchLst[[(dupId[1])]]$hIdx
				}

				if( pDebug ){
					if( 0==(hIdx%%dLogTerm) )
						k.FLogStr(sprintf("       hIdx : %d of %d",hIdx,pWin$fieldDim[1]))
				}
				
			} # for( hIdx )

			if( 0<length(matchLst)){
				for( lIdx in 1:length(matchLst) ){
					matchLst[[lIdx]]$compPair <- compPairMtx.val[ compPairMtx.val[,"dupIdx"]==matchLst[[lIdx]]$hIdx ,"hIdx"]
				}
			}

			rObj <- list( idStr.winDef=pWin$idStr ,initPos=pWin$initPos )
			rObj$buffSize <- pBuffSize
			rObj$matchLst <- matchLst
			rObj$timeStamp <- Sys.time()

			return(rObj)

		} # scanRawPattern()



createPtnObj <- function( pMatMtx ,pSearchDepth=1 ){

		r.size <- apply( pMatMtx ,1 ,function(p){ sum(!is.na(p)) } )
		rIdx <- which( r.size>0 )
		if( pSearchDepth >= length(rIdx) )
			return( NULL )

		film <- is.na(pMatMtx)
		rIdx <- rIdx[length(rIdx):(length(rIdx)-pSearchDepth+1)]
		matMtx <- pMatMtx
		matMtx[rIdx,] <- NA
		
		rObj <- list( rawMatMtx=pMatMtx ,film=film ,matMtx=matMtx ,searchDepth=pSearchDepth)
		return( rObj )
	} # createPtnObj

scanRawPattern.within <- function( pWin ,pIndices ,pFiltF=NULL ,pExcFilm=NULL ,pBuffSize=200 ,pDebug=F ){

			dLogTerm <- 2000
			if( pDebug ){
				lterm <- length(pIndices) %/% 50
				lterm <- ifelse( 50>lterm ,50 ,lterm )
				dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
				k.FLogStr(sprintf("    initPos : %d fieldDim size : %d (Log term:%d)",pWin$initPos,pWin$fieldDim[1],dLogTerm))
			}

			matchLst.size <- length(pIndices)
			buffSize <- matchLst.size*pBuffSize
			idStrs <- character(0)
			matchLst <- list()
			compPairLst <- list();	compPairLst.cost <- 0
			compPairMtx.val <- matrix(-1,ncol=2,nrow=buffSize)
			compPairMtx.idx <- rep(-1,buffSize)	
			compPairMtx.cursor <- 1
			for( oIdx in 1:(length(pIndices)-1) ){
				oIdx.idx <- pIndices[oIdx]
				matMtx.oIdx <- pWin$field[oIdx.idx:(oIdx.idx+pWin$height-1),]
				if( !is.null(pExcFilm) )
					matMtx.oIdx[pExcFilm] <- NA

				for( iIdx in (oIdx+1):length(pIndices) ){
					# k.FLogStr(sprintf(" oIdx:%d iIdx:%d",oIdx,iIdx))
					iIdx.idx <- pIndices[iIdx]
					matMtx.iIdx <- pWin$field[iIdx.idx:(iIdx.idx+pWin$height-1),]
					matMtx.f <- matMtx.oIdx == matMtx.iIdx
					matMtx.f[is.na(matMtx.f)] <- F

					matNum <- sum(matMtx.f)
					if( 0 == matNum )
						next

					matMtx.iIdx[!(matMtx.f)] <- NA

					idxMtx <- matrix( 0, nrow=matNum, ncol=3 ) # row, col, val
					idxMtx.cur <- 1
					for( rIdx in 1:nrow(matMtx.f)){
						colArea <- which(matMtx.f[rIdx,])
						for(cIdx in colArea ){
							idxMtx[idxMtx.cur,1] <- rIdx
							idxMtx[idxMtx.cur,2] <- cIdx
							idxMtx[idxMtx.cur,3] <- matMtx.iIdx[rIdx,cIdx]
							idxMtx.cur <- idxMtx.cur + 1
						}
					}
					
					if( !is.null(pFiltF) && pFiltF(matMtx.iIdx,idxMtx) )
						next

					idStr <- apply(idxMtx,1,function(p){paste(p[1:2],collapse=".")})
					idStr <- paste( idStr ,collapse=" " )
					
					saveIdx <- which( idStrs==idStr )
					if( 0==length(saveIdx) ){
						matchObj <- list( idStr=idStr ,matMtx=matMtx.iIdx ,idxMtx=idxMtx ,matNum=matNum )
						matchLst[[(1+length(matchLst))]] <- matchObj
						idStrs <- c(idStrs,idStr)
						saveIdx <- length(idStrs)
					}

					compPairMtx.idx[compPairMtx.cursor] <- saveIdx
					compPairMtx.val[compPairMtx.cursor,1] <- iIdx.idx
					compPairMtx.val[compPairMtx.cursor,2] <- oIdx.idx
					compPairMtx.cursor <- compPairMtx.cursor+1
					if( buffSize < compPairMtx.cursor ){
						cpTStamp <- Sys.time()
						cpObj <- list( compPairMtx.val=compPairMtx.val )
						cpObj$compPairMtx.idx <- compPairMtx.idx
						compPairLst[[(1+length(compPairLst))]] <- cpObj
						compPairMtx.val[,] <- -1;	compPairMtx.idx[] <- -1;	compPairMtx.cursor <- 1
						compPairLst.cost <- compPairLst.cost + (Sys.time()-cpTStamp)
						k.FLogStr(sprintf("  compPairMtx.val overflow (comPairLst:%d)",length(compPairLst)),pConsole=T)
					}

				} # for(iIdx)
				if( pDebug ){
					if( 0==(oIdx%%dLogTerm) )
						k.FLogStr(sprintf("       %dth oIdx(real idx:%d)  compPairMtx.cursor:%d(%d%%)",oIdx,oIdx.idx,compPairMtx.cursor,(compPairMtx.cursor*100)%/%buffSize))
				}
			} # for( oIdx )

			tStamp <- Sys.time()
			compPairMtx.val <- compPairMtx.val[(0<compPairMtx.idx),,drop=F]
			compPairMtx.idx <- compPairMtx.idx[(0<compPairMtx.idx)]
			if( 0<length(matchLst) ){
				for( idx in 1:length(matchLst) ){
					matchLst[[idx]]$compPair <- compPairMtx.val[compPairMtx.idx==idx,,drop=F]
					if( 0 < length(compPairLst) ){
						for( lIdx in 1:length(compPairLst) ){
							cpmL.val <- compPairLst[[lIdx]]$compPairMtx.val
							cpmL.idx <- compPairLst[[lIdx]]$compPairMtx.idx
							matchLst[[idx]]$compPair <- rbind(matchLst[[idx]]$compPair, cpmL.val[cpmL.idx==idx,,drop=F])
						}
					} # if
					matchLst[[idx]]$compPair.v <- sort(unique(as.vector(matchLst[[idx]]$compPair)))
				} # for( idx )
			}
			if( pDebug ){
				k.FLogStr(sprintf("   time cost : %.1f (comPairLst:%d create cost:%.1f)",Sys.time()-tStamp,length(compPairLst),compPairLst.cost),pConsole=T)
			}


			rObj <- list( idStr.winDef=pWin$idStr ,initPos=pWin$initPos )
			rObj$indices	<- pIndices
			rObj$excFilm	<- pExcFilm
			rObj$buffSize	<- pBuffSize
			rObj$matchLst	<- matchLst
			rObj$timeStamp	<- Sys.time()
			
			return(rObj)
			
		} # scanRawPattern()


#	pOverlapLst : overlap list. oldest overlap takes the first of list.
convOverlap2RawPtn <- function( pOverlapLst ,pRawPtn ){
			
			compPairLst <- NULL
			# mining compPair from latest overlap
			for( idx in length(pOverlapLst):1 ){ # idx <- length(pOverlapLst)
				ol <- pOverlapLst[[idx]]
				if( is.null(compPairLst) ){
					compPairLst <- lapply(ol$matchLst,function(p){cpV<-unique(as.vector(p$compPair)); return(cpV)})
				} else {
					compPairLst <- lapply( compPairLst ,function(p){ # p <- compPairLst[[1]]
												curCpLst <- lapply( ol$matchLst[p],function(p2){ # p2 <- ol$matchLst[p][[1]]
																		cpV <- as.vector(p2$compPair)
																		return(unique(cpV))
																	} )
												curCp <- do.call(c,curCpLst)
												return( unique(curCp) )
										})
				} # if
			} # for

			# find history index of ZH match to compPairLst
			hIdxLst <- lapply( compPairLst ,function(p){
								hIdx.ByP <-lapply( pRawPtn$matchLst[p] ,function(p2){
												return( as.vector(p2$compPair) )
											} )
								return( unique(do.call(c,hIdx.ByP)) )
						})
				# hIdxLst?? NULL?? ?????? ???.

			# create rawPtn from overlap
			rRawPtn <- pOverlapLst[[length(pOverlapLst)]]
			rRawPtn$initPos <- pRawPtn$initPos;	rRawPtn$indices <- pRawPtn$indices
			rRawPtn$excFilm <- pRawPtn$excFilm;	rRawPtn$buffSize <- pRawPtn$buffSize
			for( idx in 1:length(rRawPtn$matchLst) ){
				rRawPtn$matchLst[[idx]]$compPair <- sort(hIdxLst[[idx]])
				rRawPtn$matchLst[[idx]]$compPair.v <- rRawPtn$matchLst[[idx]]$compPair
			}

			return( rRawPtn )
		} # convOverlap2RawPtn()

findOverlap.filt <- function( pMatMtx ,pIdxMtx ){
							if( 2 > sum(!is.na(pMatMtx)) ){
								return( T )
							}
							return(F)
						}


#	- pScanIdx : used to devide findOverlap() work. (useless...)
#	findOverlap( pScanRst=winRstUnit$winRstLst[[1]] ,pBuffSize=200 ,pDebug=T )
#	pScanRst=ptnObjGrp$rawPtn ;pBuffSize=200 ;pDebug=T	;pExcFilm=NULL ;pScanIdx=NULL ;pFiltF=NULL
findOverlap <- function( pScanRst ,pBuffSize=200 ,pExcFilm=NULL ,pFiltF=NULL ,pDebug=F  ,pScanIdx=NULL ){

			dLogTerm <- 2000
			if( pDebug ){
				lterm <- length(pScanRst$matchLst) %/% 50
				lterm <- ifelse( 5>lterm ,5 ,lterm )
				dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
				k.FLogStr(sprintf("    matchLst length : %d (Log term:%d)",length(pScanRst$matchLst),dLogTerm))
			}

			matchLst.size <- length(pScanRst$matchLst)
			buffSize <- matchLst.size*pBuffSize
			idStrs <- character(0)
			matchLst <- list()
			compPairLst <- list();	compPairLst.cost <- 0
			compPairMtx.val <- matrix(-1,ncol=2,nrow=buffSize)
			compPairMtx.idx <- rep(-1,buffSize)	
			compPairMtx.cursor <- 1
			if( 0 < matchLst.size ){
				matDim <- dim(pScanRst$matchLst[[1]]$matMtx)
				matMtx.oIdx <- matrix( NA ,nrow=matDim[1] ,ncol=matDim[2] )
				oIdxSpan <- if(is.null(pScanIdx)){ matchLst.size:2 } else { pScanIdx }

				for( oIdx in oIdxSpan ){ # oIdx <- oIdxSpan[1]
					matMtx.oIdx[,] <- pScanRst$matchLst[[oIdx]]$matMtx[,]
					if( !is.null(pExcFilm) )
						matMtx.oIdx[pExcFilm] <- NA

					matMtx.oIdx.naF <- is.na(matMtx.oIdx)	# for speed.
					iIdxSpan <- if(is.null(pScanIdx)){ (oIdx-1):1 } else { matchLst.size:1 }

					for( iIdx in iIdxSpan ){ # iIdx <- iIdxSpan[1]

						if( oIdx == iIdx ) # pScanIdx ???? ????? ???? ????
							next

						matMtx.iIdx <- pScanRst$matchLst[[iIdx]]$matMtx
						matMtx.iIdx[matMtx.oIdx.naF] <- NA
						matMtx.iIdx[matMtx.iIdx!=matMtx.oIdx] <- NA
						
						matMtx.f <- !is.na(matMtx.iIdx)
						matNum <- sum(matMtx.f)
						if( 0 == matNum ){
							next
						}

						idxMtx <- matrix( 0, nrow=matNum, ncol=3 ) # row, col, val
						idxMtx.cur <- 1
						for( rIdx in 1:nrow(matMtx.f)){
							colArea <- which(matMtx.f[rIdx,])
							for(cIdx in colArea ){
								idxMtx[idxMtx.cur,1] <- rIdx
								idxMtx[idxMtx.cur,2] <- cIdx
								idxMtx[idxMtx.cur,3] <- matMtx.iIdx[rIdx,cIdx]
								idxMtx.cur <- idxMtx.cur + 1
							}
						}
						
						if( !is.null(pFiltF) && pFiltF(matMtx.iIdx,idxMtx) )
							next

						idStr <- apply(idxMtx,1,function(p){paste(p[1:2],collapse=".")})
						idStr <- paste( idStr ,collapse=" " )
						
						saveIdx <- which( idStrs==idStr )
						if( 0==length(saveIdx) ){
							matchObj <- list( idStr=idStr ,matMtx=matMtx.iIdx ,idxMtx=idxMtx ,matNum=matNum )
							matchLst[[(1+length(matchLst))]] <- matchObj
							idStrs <- c(idStrs,idStr)
							saveIdx <- length(idStrs)
						}

						compPairMtx.idx[compPairMtx.cursor] <- saveIdx
						compPairMtx.val[compPairMtx.cursor,1] <- iIdx
						compPairMtx.val[compPairMtx.cursor,2] <- oIdx
						compPairMtx.cursor <- compPairMtx.cursor+1
						if( buffSize < compPairMtx.cursor ){
							cpTStamp <- Sys.time()
							cpObj <- list( compPairMtx.val=compPairMtx.val )
							cpObj$compPairMtx.idx <- compPairMtx.idx
							compPairLst[[(1+length(compPairLst))]] <- cpObj
							compPairMtx.val[,] <- -1;	compPairMtx.idx[] <- -1;	compPairMtx.cursor <- 1
							compPairLst.cost <- compPairLst.cost + (Sys.time()-cpTStamp)
							k.FLogStr(sprintf("  compPairMtx.val overflow (comPairLst:%d)",length(compPairLst)),pConsole=T)
						}

					} # for(iIdx)
					if( pDebug ){
						if( 0==(oIdx%%dLogTerm) )
							k.FLogStr(sprintf("       oIdx : %d  compPairMtx.cursor:%d(%d%%)",oIdx,compPairMtx.cursor,(compPairMtx.cursor*100)%/%buffSize))
					}
				} # for( oIdx )
			} # if
			
			tStamp <- Sys.time()
			compPairMtx.val <- compPairMtx.val[(0<compPairMtx.idx),,drop=F]
			compPairMtx.idx <- compPairMtx.idx[(0<compPairMtx.idx)]
			if( 0<length(matchLst) ){
				for( idx in 1:length(matchLst) ){
					matchLst[[idx]]$compPair <- compPairMtx.val[compPairMtx.idx==idx,,drop=F]
					if( 0 < length(compPairLst) ){
						for( lIdx in 1:length(compPairLst) ){
							cpmL.val <- compPairLst[[lIdx]]$compPairMtx.val
							cpmL.idx <- compPairLst[[lIdx]]$compPairMtx.idx
							matchLst[[idx]]$compPair <- rbind(matchLst[[idx]]$compPair, cpmL.val[cpmL.idx==idx,,drop=F])
						}
					} # if
				} # for( idx )
			}
			if( pDebug ){
				k.FLogStr(sprintf("   time cost : %.1f (comPairLst:%d create cost:%.1f)",Sys.time()-tStamp,length(compPairLst),compPairLst.cost),pConsole=T)
			}

			rObj <- list( matchLst=matchLst )
			rObj$depth= ifelse( is.null(pScanRst$depth) ,1 ,pScanRst$depth+1 )
			rObj$scanIdx <- pScanIdx
			rObj$idStrs <- idStrs
			rObj$idStr.winDef <- pScanRst$idStr.winDef
			rObj$timeStamp <- Sys.time()

			return( rObj )

		} # findOverlap


findOverlap.tf <- function( pScanRst ,pBuffSize=200 ,pExcFilm=NULL ,pScanIdx=NULL ,pFiltF=NULL ,pTempFile="Temp_findOverlap" ,pDebug=F ){

			dLogTerm <- 2000
			if( pDebug ){
				lterm <- length(pScanRst$matchLst) %/% 50
				lterm <- ifelse( 5>lterm ,5 ,lterm )
				dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
				k.FLogStr(sprintf("    matchLst length : %d (Log term:%d)",length(pScanRst$matchLst),dLogTerm))
			}

			matchLst.size <- length(pScanRst$matchLst)
			buffSize <- matchLst.size*pBuffSize
			idStrs <- character(0)
			matchLst <- list()
			# compPairLst <- list();	compPairLst.cost <- 0
			compPairFileLst <- list();	compPairLst.cost <- 0
			compPairMtx.val <- matrix(-1,ncol=2,nrow=buffSize)
			compPairMtx.idx <- rep(-1,buffSize)	
			compPairMtx.cursor <- 1
			if( 0 < matchLst.size ){
				matMtx.oIdx <- pScanRst$matchLst[[1]]$matMtx
				oIdxSpan <- if(is.null(pScanIdx)){ matchLst.size:2 } else { pScanIdx }

				for( oIdx in oIdxSpan ){
					matMtx.oIdx[,] <- pScanRst$matchLst[[oIdx]]$matMtx[,]
					if( !is.null(pExcFilm) )
						matMtx.oIdx[pExcFilm] <- NA

					matMtx.oIdx.naF <- is.na(matMtx.oIdx)
					iIdxSpan <- if(is.null(pScanIdx)){ (oIdx-1):1 } else { matchLst.size:1 }

					for( iIdx in iIdxSpan ){

						if( oIdx == iIdx )
							next
						
						matMtx.iIdx <- pScanRst$matchLst[[iIdx]]$matMtx
						matMtx.iIdx[matMtx.oIdx.naF] <- NA
						matMtx.iIdx[matMtx.iIdx!=matMtx.oIdx] <- NA
						
						matMtx.f <- !is.na(matMtx.iIdx)
						matNum <- sum(matMtx.f) 
						if( 0 == matNum ){
							next
						}

						idxMtx <- matrix( 0, nrow=matNum, ncol=3 ) # row, col, val
						idxMtx.cur <- 1
						for( rIdx in 1:nrow(matMtx.f)){
							colArea <- which(matMtx.f[rIdx,])
							for(cIdx in colArea ){
								idxMtx[idxMtx.cur,1] <- rIdx
								idxMtx[idxMtx.cur,2] <- cIdx
								idxMtx[idxMtx.cur,3] <- matMtx.iIdx[rIdx,cIdx]
								idxMtx.cur <- idxMtx.cur + 1
							}
						}
						
						if( !is.null(pFiltF) && pFiltF(matMtx.iIdx,idxMtx) )
							next

						idStr <- apply(idxMtx,1,function(p){paste(p[1:2],collapse=".")})
						idStr <- paste( idStr ,collapse=" " )
						
						saveIdx <- which( idStrs==idStr )
						if( 0==length(saveIdx) ){
							matchObj <- list( idStr=idStr ,matMtx=matMtx.iIdx ,idxMtx=idxMtx ,matNum=matNum )
							matchLst[[(1+length(matchLst))]] <- matchObj
							idStrs <- c(idStrs,idStr)
							saveIdx <- length(idStrs)
						}

						compPairMtx.idx[compPairMtx.cursor] <- saveIdx
						compPairMtx.val[compPairMtx.cursor,1] <- iIdx
						compPairMtx.val[compPairMtx.cursor,2] <- oIdx
						compPairMtx.cursor <- compPairMtx.cursor+1
						if( buffSize < compPairMtx.cursor ){
							cpTStamp <- Sys.time()
							cpObj <- list( compPairMtx.val=compPairMtx.val )
							cpObj$compPairMtx.idx <- compPairMtx.idx

							# compPairLst[[(1+length(compPairLst))]] <- cpObj
							tempFile <- sprintf("%s_%d.rtmp",pTempFile,(length(compPairFileLst)+1))
							compPairFileLst[[(length(compPairFileLst)+1)]] <- tempFile
							save( cpObj ,file=tempFile )
							
							compPairMtx.val[,] <- -1;	compPairMtx.idx[] <- -1;	compPairMtx.cursor <- 1
							compPairLst.cost <- compPairLst.cost + (Sys.time()-cpTStamp)
							k.FLogStr(sprintf("  compPairMtx.val overflow (compPairFileLst:%d)",length(compPairFileLst)),pConsole=T)
						}

					} # for(iIdx)
					if( pDebug ){
						if( 0==(oIdx%%dLogTerm) )
							k.FLogStr(sprintf("       oIdx : %d  compPairMtx.cursor:%d(%d%%)",oIdx,compPairMtx.cursor,(compPairMtx.cursor*100)%/%buffSize))
					}
				} # for( oIdx )
			} # if
			
			tStamp <- Sys.time()
			compPairMtx.val <- compPairMtx.val[(0<compPairMtx.idx),,drop=F]
			compPairMtx.idx <- compPairMtx.idx[(0<compPairMtx.idx)]
			if( 0<length(matchLst) ){
				for( idx in 1:length(matchLst) ){
					matchLst[[idx]]$compPair <- compPairMtx.val[compPairMtx.idx==idx,,drop=F]
					if( 0 < length(compPairFileLst) ){
						for( lIdx in 1:length(compPairFileLst) ){
							myObj <- load( compPairFileLst[[lIdx]] ) # cpObj loading
							cpmL.val <- cpObj$compPairMtx.val
							cpmL.idx <- cpObj$compPairMtx.idx
							matchLst[[idx]]$compPair <- rbind(matchLst[[idx]]$compPair, cpmL.val[cpmL.idx==idx,,drop=F])
						}
					} # if
				} # for( idx )
			}
			if( pDebug ){
				k.FLogStr(sprintf("   time cost : %.1f (comPairLst:%d create cost:%.1f)",Sys.time()-tStamp,length(compPairFileLst),compPairLst.cost),pConsole=T)
			}

			rObj <- list( matchLst=matchLst )
			rObj$depth= ifelse( is.null(pScanRst$depth) ,1 ,pScanRst$depth+1 )
			rObj$scanIdx <- pScanIdx
			rObj$idStrs <- idStrs
			rObj$idStr.winDef <- pScanRst$idStr.winDef
			rObj$timeStamp <- Sys.time()

			return( rObj )
		} # findOverlap.tf


#	- pIndices : index matches to be scanned. (limits the scan area of matchLst)
findOverlap.within <- function( pScanRst ,pBuffSize=200 ,pIndices=NULL ,pExcFilm=NULL ,pFiltF=NULL ,pDebug=F ){

			if( is.null(pIndices) )
				pIndices <- if(0>=length(pScanRst$matchLst)){ integer(0) } else {1:length(pScanRst$matchLst)}

			dLogTerm <- 2000
			if( pDebug ){
				lterm <- length(pIndices) %/% 50
				lterm <- ifelse( 5>lterm ,5 ,lterm )
				dLogTerm <- ifelse( dLogTerm<lterm ,dLogTerm ,lterm )
				k.FLogStr(sprintf("    matchLst length : %d (Log term:%d)",length(pIndices),dLogTerm))
			}

			matDim <- dim(pScanRst$matchLst[[1]]$matMtx)
			matchLst.size <- length(pIndices)
			buffSize <- matchLst.size*pBuffSize
			idStrs <- character(0)
			matchLst <- list()
			compPairLst <- list();	compPairLst.cost <- 0
			compPairMtx.val <- matrix(-1,ncol=2,nrow=buffSize)
			compPairMtx.idx <- rep(-1,buffSize)	
			compPairMtx.cursor <- 1
			if( 0 < matchLst.size ){
				matMtx.oIdx <- matrix(0,nrow=matDim[1],ncol=matDim[2])

				for( oIdx in 1:(length(pIndices)-1) ){ # oIdx <- 1
					oIdx.idx <- pIndices[oIdx]
					matMtx.oIdx[,] <- pScanRst$matchLst[[oIdx.idx]]$matMtx[,]
					if( !is.null(pExcFilm) ){
						matMtx.oIdx[pExcFilm] <- NA
					}

					matMtx.oIdx.naF <- is.na(matMtx.oIdx)

					for( iIdx in (oIdx+1):length(pIndices) ){
						# k.FLogStr(sprintf(" oIdx:%d iIdx:%d",oIdx,iIdx))

						iIdx.idx <- pIndices[iIdx]

						matMtx.iIdx <- pScanRst$matchLst[[iIdx.idx]]$matMtx
						matMtx.iIdx[matMtx.oIdx.naF] <- NA
						matMtx.iIdx[matMtx.iIdx!=matMtx.oIdx] <- NA

						matMtx.f <- !is.na(matMtx.iIdx)
						matNum <- sum(matMtx.f) 
						if( 0 == matNum ){
							next
						}

						idxMtx <- matrix( 0, nrow=matNum, ncol=3 ) # row, col, val
						idxMtx.cur <- 1
						for( rIdx in 1:nrow(matMtx.f)){
							colArea <- which(matMtx.f[rIdx,])
							for(cIdx in colArea ){
								idxMtx[idxMtx.cur,1] <- rIdx
								idxMtx[idxMtx.cur,2] <- cIdx
								idxMtx[idxMtx.cur,3] <- matMtx.iIdx[rIdx,cIdx]
								idxMtx.cur <- idxMtx.cur + 1
							}
						}
						
						if( !is.null(pFiltF) && pFiltF(matMtx.iIdx,idxMtx) )
							next

						idStr <- apply(idxMtx,1,function(p){paste(p[1:2],collapse=".")})
						idStr <- paste( idStr ,collapse=" " )
						
						saveIdx <- which( idStrs==idStr )
						if( 0==length(saveIdx) ){
							matchObj <- list( idStr=idStr ,matMtx=matMtx.iIdx ,idxMtx=idxMtx ,matNum=matNum )
							matchLst[[(1+length(matchLst))]] <- matchObj
							idStrs <- c(idStrs,idStr)
							saveIdx <- length(idStrs)
						}

						compPairMtx.idx[compPairMtx.cursor] <- saveIdx
						compPairMtx.val[compPairMtx.cursor,1] <- iIdx.idx
						compPairMtx.val[compPairMtx.cursor,2] <- oIdx.idx
						compPairMtx.cursor <- compPairMtx.cursor+1
						if( buffSize < compPairMtx.cursor ){
							cpTStamp <- Sys.time()
							cpObj <- list( compPairMtx.val=compPairMtx.val )
							cpObj$compPairMtx.idx <- compPairMtx.idx
							compPairLst[[(1+length(compPairLst))]] <- cpObj
							compPairMtx.val[,] <- -1;	compPairMtx.idx[] <- -1;	compPairMtx.cursor <- 1
							compPairLst.cost <- compPairLst.cost + (Sys.time()-cpTStamp)
							k.FLogStr(sprintf("  compPairMtx.val overflow (comPairLst:%d)",length(compPairLst)),pConsole=T)
						}

					} # for(iIdx)
					if( pDebug ){
						if( 0==(oIdx%%dLogTerm) )
							k.FLogStr(sprintf("       %dth oIdx(real idx:%d)  compPairMtx.cursor:%d(%d%%)",oIdx,oIdx.idx,compPairMtx.cursor,(compPairMtx.cursor*100)%/%buffSize))
					}
				} # for( oIdx )
			} # if

			tStamp <- Sys.time()
			compPairMtx.val <- compPairMtx.val[(0<compPairMtx.idx),,drop=F]
			compPairMtx.idx <- compPairMtx.idx[(0<compPairMtx.idx)]
			if( 0<length(matchLst) ){
				for( idx in 1:length(matchLst) ){
					matchLst[[idx]]$compPair <- compPairMtx.val[compPairMtx.idx==idx,,drop=F]
					if( 0 < length(compPairLst) ){
						for( lIdx in 1:length(compPairLst) ){
							cpmL.val <- compPairLst[[lIdx]]$compPairMtx.val
							cpmL.idx <- compPairLst[[lIdx]]$compPairMtx.idx
							matchLst[[idx]]$compPair <- rbind(matchLst[[idx]]$compPair, cpmL.val[cpmL.idx==idx,,drop=F])
						}
					} # if
				} # for( idx )
			}
			if( pDebug ){
				k.FLogStr(sprintf("   time cost : %.1f (comPairLst:%d create cost:%.1f)",Sys.time()-tStamp,length(compPairLst),compPairLst.cost),pConsole=T)
			}

			rObj <- list( matchLst=matchLst )
			rObj$depth= ifelse( is.null(pScanRst$depth) ,1 ,pScanRst$depth+1 )
			rObj$indices <- pIndices
			rObj$idStrs <- idStrs
			rObj$idStr.winDef <- pScanRst$idStr.winDef
			rObj$timeStamp <- Sys.time()

			return( rObj )
		} # findOverlap.within( )



# pSaveFile="Obj_winObj.0001.0005.save";	pBuffSize=200;	pMP=T;	pDebug=T
# findOverlapFromFile( "Obj_winObj.0001.0005.save" ,pBuffSize=200 ,pMP=T ,pDebug=T )
findOverlapFromFile <- function( pSaveFile ,pBuffSize=200 ,pMP=T ,pDebug=F ){
		objName <- load(pSaveFile)
		if( "winRstUnit" != objName ){
			cat(sprintf("Fail to load winRstUnit from %s \n",pSaveFile))
			return( NULL )
		}
		fIdStr <- sprintf("fOFF[%s]",paste(winRstUnit$idx,collapse=","))
		
		tStamp <- Sys.time()
		overlapLst <- list()
		if( 0<length(winRstUnit$winRstLst) ){
			if( !pMP ){
				for( idx in 1:length(winRstUnit$winRstLst) ){
					# winRst <- winRstUnit$winRstLst[[1]]
					# k <- findOverlap( winRst ,pBuffSize=pBuffSize ,pDebug=pDebug )				
					if(pDebug){
						k.FLogStr(sprintf("%s Start %dth winRst precess",fIdStr,idx))
					}
					overlapLst[[(1+length(overlapLst))]] <- findOverlap( winRstUnit$winRstLst[[idx]] ,pBuffSize=pBuffSize ,pDebug=pDebug )
				}
			} else {
				sfExport("k.FLogStr");	sfExport("k.FLogOpt");
				sfExport("findOverlap");
				overlapLst <- sfLapply(winRstUnit$winRstLst,function( winRst ){
									gc()
									overlap <- findOverlap( winRst ,pBuffSize=pBuffSize ,pDebug=pDebug )
									return(overlap)
								})
			}
		}
		if( pDebug ){
			k.FLogStr(sprintf("%s cost:%.1f",fIdStr,Sys.time()-tStamp))
		}

		rObj <- list( idx=winRstUnit$idx, winRstUnitFile = pSaveFile )
		rObj$overlapLst <- overlapLst
		return(rObj)
	} # findOverlapFromFile

getWindow <- function( pFieldMtx=FB$zh ,pFilm=NULL ,pHeight=3 ,pInitPos=NULL ,pIdStr="windowObj" ){

		if(is.null(pInitPos)){
			pInitPos <- sample( pHeight:nrow(pFieldMtx) ,1 )
		}
		if( is.null(pFilm) ){
			pFilm <- getDefaultFilm( pFieldMtx )$filmMtx
		}

		rObj <- list( idStr=pIdStr )
		rObj$initPos <- pInitPos
		rObj$height <- pHeight
		rObj$width <- ncol(pFieldMtx)
		rObj$field <- pFieldMtx
		rObj$film <- pFilm
		rObj$fieldDim <- dim(pFieldMtx)
		names(rObj$fieldDim) <- c("nrow","ncol")
		
		return(rObj)
	}

getDefaultFilm <- function( pFieldMtx ,pFreqThreshold=0.5 ){

		valFreq <- table(as.vector(pFieldMtx),useNA="ifany")
		valFreq <- valFreq/sum(valFreq)

		filmMtx <- matrix( T ,nrow=nrow(pFieldMtx), ncol=ncol(pFieldMtx) )
		filmActivated <- F
		maxFreqVal <- sort(valFreq,decreasing=T)[1]
		if( 1<length(valFreq) && pFreqThreshold<=max(valFreq) && maxFreqVal!="NA" ){
			filmMtx <- pFieldMtx!=as.integer(names(maxFreqVal))
			filmActivated <- T
		}

		filmMtx[is.na(pFieldMtx)] <- F
		
		rObj <- list( freqThreshold=pFreqThreshold )
		rObj$filmMtx <- filmMtx
		rObj$valFreq <- valFreq
		rObj$filmActivated <- filmActivated

		return( rObj )
	}

#	- fnlCand : final candidate. if there is no candidate, no cmbMtx
#	pInvM = huLst.invM[[1]] ;pOlNum=2 ;pFnlCandMax=100 ;pCombinationMax=5000 ;pDebug=T
ana.invM <- function( pInvM ,pOlNum=2 ,pFnlCandMax=100 ,pDebug=T ){

				hu.maxCvr <- apply(pInvM$cvrMtx.hu,1,max)
				flag.huCvrAll <- hu.maxCvr > 70 

				flag.cand <- hu.maxCvr > 30
				flag.cand <- flag.cand & (pInvM$clueIdxNum>2)

				rObj <- list( errMsg=NULL ,combinationMax=pCombinationMax ,matchLst.idx=pInvM$matchLst.idx )
				rObj$cvrMtx			<- pInvM$cvrMtx
				rObj$cvrMtx.hu		<- pInvM$cvrMtx.hu
				rObj$cvrMtx.clue	<- pInvM$cvrMtx.clue
				rObj$clueIdxNum		<- pInvM$clueIdxNum

				rObj$fnlCand <- which( flag.huCvrAll | flag.cand )
				if( pDebug )
					k.FLogStr(sprintf("    ana.invM()  matchLst.idx:%3d  fnlCand:%d " ,pInvM$matchLst.idx,length(rObj$fnlCand)) )

				if( is.null(rObj$fnlCand) || 0==length(rObj$fnlCand) ){
					rObj$errMsg <- "no final candidate"
					return( rObj )
				} else if( pFnlCandMax < length(rObj$fnlCand) ){
					rObj$errMsg <- sprintf( "too much candidate (%d)" ,length(rObj$fnlCand) )
					return( rObj )
				}

				if( 1==length(rObj$fnlCand) ){
					cmbMtx <- matrix( rObj$fnlCand ,nrow=1 ,ncol=pOlNum )
				} else {
					cmbMtx <- combinations( length(rObj$fnlCand) ,pOlNum )
				}
				rObj$cmbMtx <- cmbMtx

				sdIdx <- if( 2==pOlNum ) 1 else 1:(pOlNum-1) # indices of Set difference
				sdIdxLst <- list()
				for( idx in sdIdx ){
					comb <- combinations( (pOlNum-1) ,idx )
					for( combRIdx in 1:nrow(comb) ){
						sdIdxLst[[(1+length(sdIdxLst))]] <- comb[combRIdx,]
					}
				} # ?????? ???? ??????

				cmbCvrLst <- list()
				for( rIdx in 1:nrow(cmbMtx) ){
					frObj <- list( )
					cmb <- cmbMtx[rIdx,]
					frObj[["union"]] <- Reduce(union ,pInvM$clueCompPair[cmb])
					if( 2<=length(unique(cmb)) ){ # final Candidate?? ?????? ?????? ????.
						frObj[["intersect"]] <- Reduce(intersect ,pInvM$clueCompPair[cmb])
						for(cIdx in 1:ncol(cmbMtx) ){
							rmCmb <- setdiff(cmb,cmb[cIdx])
							for( sdIdx in 1:length(sdIdxLst) ){
								curRmCmb <- rmCmb[ sdIdxLst[[sdIdx]] ]
								sdName <- sprintf("sd%d_%s",cmb[cIdx] ,paste( curRmCmb,collapse="a"))
								# ?????? ???
								frObj[[sdName]] <- setdiff( pInvM$clueCompPair[[ cmb[cIdx] ]] ,do.call( c ,pInvM$clueCompPair[curRmCmb] ) )
							}
						}
					} # if
					cmbCvrLst[[(1+length(cmbCvrLst))]] <- frObj
				}
				rObj$cmbCvrLst <- cmbCvrLst
				rObj$huCompPair <- pInvM$huCompPair

				return( rObj )

		} # ana.invM

#	pHuCompPair=pInvM.ana$huCompPair	;pCmbCvrLst=pInvM.ana$cmbCvrLst
ana.invM.getCvrMtx <- function( pHuCompPair ,pCmbCvrLst ){

									rCvrObj <- list()
									huSize <- sapply(pHuCompPair,length)

									typNameLst <- list()
									cvrMtxLst <- list()
									for( typIdx in 1:length(pCmbCvrLst[[1]]) ){
										# typIdx : ??????? ???? ?? ??? ?????? ???? ??????.
										#	u????? ??????, ???????? ??????,
										#	??????? ?????? ???? ?? ?????? ?????? ??????
										#	?? ?? ?????? u???? ????? ???? ??????, ?????? ????? ???? ??????, ?????? ????? ????...
										#	(cmbMtx?? u???? ????? ??????? ?????? ???? ????????.)
										cmbCvrMtx <- matrix( 0 ,ncol=length(huSize) ,nrow=length(pCmbCvrLst) )
										for( rIdx in 1:length(pCmbCvrLst) ){
											cmbCvrMtx[rIdx,] <- sapply( pHuCompPair ,function(p){ sum( p %in% pCmbCvrLst[[rIdx]][[typIdx]]) } )
										}

										rNames <- sapply( pCmbCvrLst ,function(p){attributes(p)$names[typIdx]} )
										typNameLst[[(1+length(typNameLst))]] <- rNames
										rownames(cmbCvrMtx) <- gsub( "^(sd[[:digit:]]*).*" ,"\\1.." ,rNames )
										# ??? ???? ?? ??????.. u????? ???????? ??????, ?????????? ??????? 
										cvrMtxLst[[typIdx]] <- cmbCvrMtx
									}
									rCvrObj$cvrMtxLst <- cvrMtxLst
									rCvrObj$typNameLst <- typNameLst

									cvrRateMtxLst.hnt	<- list()
									cvrRateMtxLst.clue	<- list()
									for( idx in 1:length(cvrMtxLst) ){
										cvrRateMtxLst.hnt[[sprintf("%d.hnt",idx)]] <- t(apply(cvrMtxLst[[idx]],1,function(p){
																								if(0==length(p)) rep(0,length(p))
																								else (p*100)%/%huSize 
																							}))
										cvrRateMtxLst.clue[[sprintf("%d.clue",idx)]] <- t(apply(cvrMtxLst[[idx]],1,function(p){
																								if(0==length(p)) rep(0,length(p)) 
																								else (p*100)%/%sum(p)
																							}))
									}
									rCvrObj$cvrRateMtxLst.hnt	<- cvrRateMtxLst.hnt
									rCvrObj$cvrRateMtxLst.clue	<- cvrRateMtxLst.clue

									return( rCvrObj )
						}


# pMtxClue=cvr$cvrRateMtxLst.clue[[1]]	;pMtxHnt=cvr$cvrRateMtxLst.hnt[[1]]
scanGoodRow.cvrRateMtx <- function( pMtxClue ,pMtxHnt ){
		flag <- rep( F ,nrow(pMtxClue) )
		for( rIdx in 1:nrow(pMtxClue) ){
			flag.hnt	<- pMtxHnt[rIdx,]>=60 # 50
			flag.clue	<- pMtxClue[rIdx,]>=90
			if( any(flag.clue&flag.hnt) ){
				flag[rIdx] <- T
				next
			}
			
			flag.hnt	<- pMtxHnt[rIdx,]>=90
			flag.clue	<- pMtxClue[rIdx,]>=40
			if( 2==sum(flag.hnt&flag.clue) ){
				flag[rIdx] <- T
				next
			}
		}
		return( flag )
	} # scanGoodRow.cvrRateMtx( )

# pMatchLst=rawPtn$matchLst ;pExcFilm=NULL ;pSearchArea=NULL ;pExpandMod=T	;pDebug=T
scanMatchLst <- function( pMatchLst ,pExcFilm=NULL ,pSearchArea=NULL ,pExpandMod=T ,pDebug=F ){

		matDim <- dim(pMatchLst[[1]]$matMtx)
		if( is.null(pSearchArea) ) {
			pSearchArea <- seq_along(pMatchLst)
		}
		if( is.null(pExcFilm) ){
			pExcFilm <- matrix( F ,nrow=matDim[1] ,ncol=matDim[2] )
			# pExcFilm[-nrow(pExcFilm),] <- T # for predict idx
		}

		ptnLst <- list() # predict Ptn
		for( idx in seq_along(pSearchArea) ){ # idx<-1

			curMtx <- pMatchLst[[ pSearchArea[idx] ]]$matMtx
			curMtx[pExcFilm] <- NA
			idxNum <- sum(!is.na(curMtx))
			if( 0==idxNum )
				next

			# create idxMtx
			idxMtx <- matrix( 0 ,nrow=idxNum ,ncol=3 ) # rIdx, cIdx, value
			ic <- 1 # input cursor
			for( rIdx in 1:matDim[1] ){
				fIndices <- which(!is.na(curMtx[rIdx,]))
				if( 0==length(fIndices) )
					next
				for( cIdx in fIndices ){
					idxMtx[ic,1] <- rIdx;	idxMtx[ic,2]<-cIdx; idxMtx[ic,3]<-curMtx[rIdx,cIdx]
					ic <- ic+1
				}
			}

			idxStr	<- paste( apply(idxMtx,1,function(p){paste(p[1:2],collapse=".")}) ,collapse="/" )	# just use row&col idx
			idxStrV	<- paste( apply(idxMtx,1,function(p){paste(p[1:3],collapse=".")}) ,collapse="/" )	# include value
			flag <- sapply( ptnLst ,function(p){return(p$idxStr==idxStr)} ) # flag is 0 length list() when ptnLst's length is 0

			if( 0==length(flag) || 0==sum(flag) ) { # need new ptn object

				vPtn <- list( idxStr=idxStrV );		vPtn$idxLst <- list( idx )	# should be (pSearchArea[idx]) ?
				ptn <- list( idxStr=idxStr )
				ptn$excFilm = is.na(pMatchLst[[(pSearchArea[idx])]]$matMtx)
				ptn$vPtnLst <- list()
				ptn$vPtnLst[[1]] <- vPtn
				ptnLst[[(1+length(ptnLst))]] <- ptn

			} else {
				# in most case, vPtnLst is just one.
				# cause matchLst is produced based on fixed map of which position defined as one wIdx
				idx.ptnLst <- which(flag)[1]
				ptnLst[[idx.ptnLst]]$excFilm[ !is.na(pMatchLst[[(pSearchArea[idx])]]$matMtx) ] <- F
				flag.v <- sapply( ptnLst[[idx.ptnLst]]$vPtnLst ,function(p){p$idxStr==idxStrV} )
				if( 0==sum(flag.v) ){ # need new vPtn object
					vPtn <- list( idxStr=idxStrV);	vPtn$idxLst <- list( idx )
					vPtn.l <- length(ptnLst[[idx.ptnLst]]$vPtnLst)
					ptnLst[[idx.ptnLst]]$vPtnLst[[(vPtn.l+1)]] <- vPtn
				} else {
					idx.vPtnLst <- which(flag.v)[1]
					idxLst.l <- length(ptnLst[[idx.ptnLst]]$vPtnLst[[idx.vPtnLst]]$idxLst)
					ptnLst[[idx.ptnLst]]$vPtnLst[[idx.vPtnLst]]$idxLst[[(idxLst.l+1)]] <- idx
				}

			} # if( 0==length(flag) || 0==sum(flag))

		} # for

		# compPair from idxLst
		if( 0 < length(ptnLst) ){
			for( idx1 in 1:length(ptnLst) ){
				for( idx2 in 1:length(ptnLst[[idx1]]$vPtnLst) ){
					compPair <- do.call( c ,ptnLst[[idx1]]$vPtnLst[[idx2]]$idxLst )
					ptnLst[[idx1]]$vPtnLst[[idx2]]$idxLst <- NULL
					ptnLst[[idx1]]$vPtnLst[[idx2]]$compPair <- compPair
				}
			}
		}

		return( ptnLst )

	} # scanMatchLst

#	- pCompPairNum : used for accumMtx.cp
# 		pMatchLst=ptnObjGrp$rawPtn$matchLst ;pExcFilm=NULL ;pSearchArea=NULL ;pCompPairNum=NULL	;pDebug=F
#		getMatMtxAccum( pMatchLst=ptnObjGrp$rawPtn$matchLst )
getMatMtxAccum <- function( pMatchLst ,pExcFilm=NULL ,pSearchArea=NULL ,pCompPairNum=NULL ,pDebug=F ){

		matDim <- dim( pMatchLst[[1]]$matMtx )
		if( is.null(pExcFilm) ){
			pExcFilm <- matrix( F ,nrow=matDim[1] ,ncol=matDim[2] )
		}
		if( is.null(pSearchArea) ){
			pSearchArea <- 1:length(pMatchLst)
		}
		if( is.null(pCompPairNum) ){
			pCompPairNum <- sapply( pMatchLst 
										,function(p){cpV<-as.vector(p$compPair);return(length(unique(cpV)))} 
								)
		}

		accumMtx	<- matrix( 0 ,nrow=matDim[1] ,ncol=matDim[2] )
		accumMtx.cp	<- matrix( 0 ,nrow=matDim[1] ,ncol=matDim[2] )
		for( idx in 1:length(pSearchArea) ){ # idx <- 1
			idx.r <- pSearchArea[idx]
			curMtx <- pMatchLst[[ idx.r ]]$matMtx
			curMtx[pExcFilm] <- NA
			curMtxF	<- !is.na(curMtx)
			accumMtx	<- accumMtx + curMtxF
			accumMtx.cp	<- accumMtx.cp + pCompPairNum[idx.r]*curMtxF
		} # for(idx)

		rObj <- list( accumMtx=accumMtx ,accumMtx.cp=accumMtx.cp )
		return( rObj )

	} # getMatMtxAccum( )

# -------------------------------------------------------------------------------------------
#	- pPredFlagMtx : which one is predict among pMatMtx idx
#	pWinDef= ;pMatMtx= ;pPredFlagMtx=NULL ;pScanArea=NULL
assessClue <- function( pWinDef ,pMatMtx ,pPredFlagMtx=NULL ,pScanArea=NULL ){

		if( is.null(pPredFlagMtx) ){
			pPredFlagMtx <- !is.na(pMatMtx)
			pPredFlagMtx[-pWinDef$height,] <- F
		}

		clueMtx <- pMatMtx
		clueMtx[pPredFlagMtx] <- NA
		clueMtx.num <- sum( !is.na(clueMtx) )

		predNum <- sum(pPredFlagMtx)
		cName <- c("row","col","val","hitNum")
		predIdxMtx <- matrix( 0 ,nrow=predNum ,ncol=length(cName) )
		colnames( predIdxMtx ) <- cName
		inputRow <- 1
		for( rIdx in seq_len(nrow(pPredFlagMtx)) ){
			for( cIdx in seq_len(ncol(pPredFlagMtx)) ){
				if( pPredFlagMtx[rIdx,cIdx] ){
					predIdxMtx[inputRow,"row"] <- rIdx	;predIdxMtx[inputRow,"col"] <- cIdx
					predIdxMtx[inputRow,"val"] <- pMatMtx[rIdx,cIdx]
					inputRow <- inputRow + 1
				}
			}
		}

		scanArea <- 1:( nrow(pWinDef$field) -pWinDef$height +1 )
		if( !is.null(pScanArea) )
			scanArea <- intersect(scanArea,pScanArea)

		clueFindLst <- list()
		predIdxAFndLst <- list() # predIdx found all (not limited by clue)
		windowMtx <- matrix( 0 ,nrow=pWinDef$height ,ncol=pWinDef$width )
		for( sIdx in seq_along(scanArea) ){ # sIdx <- seq_along(scanArea)[1]
			h <- scanArea[sIdx]
			windowMtx[,] <- pWinDef$field[h:(h+pWinDef$height-1),]

			predIdxAFndLst[(length(predIdxAFndLst)+1)] <- ( predNum == sum( windowMtx[pWinDef$height,]==pMatMtx[pWinDef$height,],na.rm=T) )

			if( clueMtx.num != sum( (windowMtx==clueMtx) ,na.rm=T) )
				next

			valInPredPos <- rep(0,predNum) 
			for( rIdx in seq_len(nrow(predIdxMtx)) ){
				valInPredPos[rIdx] <- windowMtx[ predIdxMtx[rIdx,"row"] ,predIdxMtx[rIdx,"col"] ]
			}

			matchFlag <- (valInPredPos==predIdxMtx[,"val"])
			matchFlag <- ifelse( is.na(matchFlag) ,F ,matchFlag )
			clueFind <- list( hIdx =h ,matchFlag =matchFlag )
			clueFindLst[[(1+length(clueFindLst))]] <- clueFind
		} # sIdx

		predHauntRate <- (do.call(sum,predIdxAFndLst)*100) %/% length(predIdxAFndLst)
		matchFlagMtx <- matrix( F ,nrow=predNum ,ncol=length(clueFindLst) )
		matchFlagMtx[,] <- sapply(clueFindLst,function(p){p$matchFlag})
		predIdxMtx[,"hitNum"] <- apply( matchFlagMtx ,1 ,sum )

		hitRate <- ( 100 * sum(sapply(clueFindLst,function(p){all(p$matchFlag)})) ) %/% length(clueFindLst)
			# pMatchPtn should be meet itself in zh, so length of clueFindLst must be more than 0

		return( list( hitRate=hitRate ,clueFindLst=clueFindLst ,predIdxMtx=predIdxMtx ,predHauntRate=predHauntRate ) )
		
	} # assessClue



getFieldSet.diff <- function( pFieldMtx=as.matrix(FB$zh) ,pDist=2 ,pAbs=T ){

		nColL <- ncol(pFieldMtx)
		nRowL <- nrow(pFieldMtx)
		vFieldMtx <- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=nColL*nColL )
		colnames(vFieldMtx) <- as.vector(outer(1:nColL,1:nColL,function(pA,pB){sprintf("%dF%d",pA,pB)}))
		diffMtx <- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=nColL )
		for( cIdx in 1:nColL ){
			for( rIdx in 1:nRowL ){
				if( rIdx<=pDist ){
					diffMtx[rIdx,] <- NA
					next
				}
				diffMtx[rIdx,] <- pFieldMtx[(rIdx-pDist),cIdx] - pFieldMtx[rIdx,]
				if( pAbs ){
					diffMtx[rIdx,] <- abs(diffMtx[rIdx,])
				}
			}
			idxBase <- nColL*(cIdx-1)
			vFieldMtx[,(1:nColL + idxBase)] <- diffMtx
		}

		filmObj <- getDefaultFilm(vFieldMtx)

		rObj <- list( idStr=sprintf("diff_%d%s",pDist,ifelse(pAbs,"T","F")) )
		rObj$vFieldMtx <- vFieldMtx
		rObj$film <- filmObj$filmMtx
		rObj$filmActivated <- filmObj$filmActivated
		return( rObj )

	}

getFieldSet.have <- function( pFieldMtx=as.matrix(FB$zh) ,pVal=c(1,2,3,5) ){

		vFieldMtx <- apply(pFieldMtx,2,function(p){ifelse(p%in%pVal,1,0)})
		filmObj <- getDefaultFilm(vFieldMtx)

		rObj <- list( idStr=sprintf("dev_%d",pBase) )
		rObj$vFieldMtx <- vFieldMtx
		rObj$film <- filmObj$filmMtx
		rObj$filmActivated <- filmObj$filmActivated
		return( rObj )

	}

getFieldSet.dev <- function( pFieldMtx=as.matrix(FB$zh) ,pBase=4 ){

		vFieldMtx <- pFieldMtx %/% pBase
		filmObj <- getDefaultFilm(vFieldMtx)

		rObj <- list( idStr=sprintf("dev_%d",pBase) )
		rObj$vFieldMtx <- vFieldMtx
		rObj$film <- filmObj$filmMtx
		rObj$filmActivated <- filmObj$filmActivated
		return( rObj )

	}

getFieldSet.left <- function( pFieldMtx=as.matrix(FB$zh) ,pBase=4 ){

		vFieldMtx <- pFieldMtx %% pBase
		filmObj <- getDefaultFilm(vFieldMtx)

		rObj <- list( idStr=sprintf("left_%d",pBase) )
		rObj$vFieldMtx <- vFieldMtx
		rObj$film <- filmObj$filmMtx
		rObj$filmActivated <- filmObj$filmActivated
		return( rObj )

	}

getFieldSet.have <- function( pFieldMtx=as.matrix(FB$zh) ,pHave=NULL ,pAsFlag=T ){

		if( is.null(pHave) ){
			pHave <- c(2,3,5,7,11,13,17,19,23,29,31,37,41,43)
		}

		vFieldMtx <- apply(pFieldMtx,1,function(p){	flag <- p %in% pHave
								if( pAsFlag ) ifelse(flag,1,0)
								else ifelse(flag,p,0)
							})
		vFieldMtx <- t( vFieldMtx )
		filmObj <- getDefaultFilm(vFieldMtx)
		
		rObj <- list( idStr=sprintf("have_%d",length(pHave)) )
		rObj$vFieldMtx <- vFieldMtx
		rObj$film <- filmObj$filmMtx
		rObj$filmActivated <- filmObj$filmActivated		

		return( rObj )
	}

getFieldSet.reb <- function( pFieldMtx=as.matrix(FB$zh) ,pDepth=2 ){

		nCol.field <- ncol(pFieldMtx)
		# Depth ?? ???? ??? ?? ????? ?????
		rebMtx.byVal <- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=nCol.field )
		for( rIdx in 1:nrow(pFieldMtx) ){
			if( rIdx <= pDepth ){
				rebMtx.byVal[rIdx,] <- rep(NA,nCol.field)
				next
			}

			for(cIdx in 1:nCol.field){
				chk <- pFieldMtx[(rIdx-1):(rIdx-pDepth),] == pFieldMtx[rIdx,cIdx]
				rebMtx.byVal[rIdx,cIdx] <- sum(chk)
			}
		} # for(rIdx)
		rebMtx.byVal.sum <- matrix( apply(rebMtx.byVal,1,sum), ncol=1 )
		rebMtx.byVal.F <- ifelse(rebMtx.byVal.sum>0 ,1 ,0 )

		rebMtx.rebF <- ifelse( rebMtx.byVal>0 ,1 ,0 ) # ????? ????????? ??u
		rebMtx.rebF.sum <- matrix( apply(rebMtx.rebF,1,sum) ,ncol=1 )

		# Depth ?? ???? ??? ???? ??????? ?? ????? ?????
		rebMtx.byValP <- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=nCol.field )
		for( rIdx in 1:nrow(pFieldMtx) ){
			if( rIdx <= pDepth ){
				rebMtx.byValP[rIdx,] <- rep(NA,nCol.field)
				next
			}

			for(cIdx in 1:nCol.field){
				chk <- pFieldMtx[(rIdx-1):(rIdx-pDepth),cIdx] == pFieldMtx[rIdx,cIdx]
				rebMtx.byValP[rIdx,cIdx] <- sum(chk)
			}
		} # for(rIdx)
		rebMtx.byValP.sum <- matrix( apply(rebMtx.byValP,1,sum), ncol=1 )
		rebMtx.byValP.F <- ifelse(rebMtx.byValP.sum>0 ,1 ,0 )
		rebMtx.rebFP <- ifelse( rebMtx.byValP>0 ,1 ,0 ) # ????? ????????? ??u
		rebMtx.rebFP.sum <- matrix( apply(rebMtx.rebF,1,sum) ,ncol=1 )


		vFieldMtx <- rebMtx.byVal
		filmMtx <- getDefaultFilm(vFieldMtx)$filmMtx
		vFieldMtx <- cbind( vFieldMtx, rebMtx.byVal.sum )
		filmMtx <- cbind( filmMtx, getDefaultFilm(rebMtx.byVal.sum)$filmMtx )
		vFieldMtx <- cbind( vFieldMtx, rebMtx.byVal.F)
		filmMtx <- cbind( filmMtx, getDefaultFilm(rebMtx.byVal.F)$filmMtx )
		vFieldMtx <- cbind( vFieldMtx, rebMtx.rebF)
		filmMtx <- cbind( filmMtx, getDefaultFilm(rebMtx.rebF)$filmMtx )
		vFieldMtx <- cbind( vFieldMtx, rebMtx.rebF.sum)
		filmMtx <- cbind( filmMtx, getDefaultFilm(rebMtx.rebF.sum)$filmMtx )

		vFieldMtx <- cbind( vFieldMtx, rebMtx.byValP)
		filmMtx <- cbind( filmMtx, getDefaultFilm(rebMtx.byValP)$filmMtx )
		vFieldMtx <- cbind( vFieldMtx, rebMtx.byValP.sum)
		filmMtx <- cbind( filmMtx, getDefaultFilm(rebMtx.byValP.sum)$filmMtx )
		vFieldMtx <- cbind( vFieldMtx, rebMtx.byValP.F )
		filmMtx <- cbind( filmMtx, getDefaultFilm(rebMtx.byValP.F)$filmMtx )
		vFieldMtx <- cbind( vFieldMtx, rebMtx.rebFP )
		filmMtx <- cbind( filmMtx, getDefaultFilm(rebMtx.rebFP)$filmMtx )
		vFieldMtx <- cbind( vFieldMtx, rebMtx.rebFP.sum )
		filmMtx <- cbind( filmMtx, getDefaultFilm(rebMtx.rebFP.sum)$filmMtx )

		rObj <- list( idStr=sprintf("reb_%d",pDepth) )
		rObj$vFieldMtx <- vFieldMtx
		rObj$film <- filmMtx
		rObj$filmActivated <- T # film?????? ?????????... ?? T ?? ????????.
		return( rObj )

	}

getFieldSet.dummy <- function( pFieldMtx=as.matrix(FB$zh) ){

		vFieldMtx <- pFieldMtx
		filmObj <- getDefaultFilm(vFieldMtx)

		rObj <- list( idStr=sprintf("dummy") )
		rObj$vFieldMtx <- vFieldMtx
		rObj$film <- filmObj$filmMtx
		rObj$filmActivated <- filmObj$filmActivated
		return( rObj )

	}

getFieldSet.dummy.ptn <- function( pFieldMtx=as.matrix(FB$zh) ){

		testLst <- list()
		# --------------------------------------------------
		ptn <- c(   1,  0,  0,  1,  1,  0
				 ,  0,  1,  1,  0,  0,  0
				 ,  1,  0,  0,  1,  0,  0
				)
		testObj <- list( hIdx=c( 1 ) )
		testObj$ptnMtx <- matrix( ptn ,nrow=3 ,ncol=6 ,byrow=T )
		testLst[[(length(testLst)+1)]] <- testObj


		ptn <- c(   1,  0,  0,  0,  0,  0
				 ,  0,  1,  1,  0,  0,  0
				 ,  0,  0,  0,  1,  0,  0
				)
		testObj <- list( hIdx=c( 10, 20, 30 ) )
		testObj$ptnMtx <- matrix( ptn ,nrow=3 ,ncol=6 ,byrow=T )
		testLst[[(length(testLst)+1)]] <- testObj

		ptn <- c(   0,  0,  0,  1,  1,  0
				 ,  0,  1,  1,  0,  0,  0
				 ,  1,  0,  0,  0,  0,  0
				)
		testObj <- list( hIdx=c( 110, 120, 130, 140 ) )
		testObj$ptnMtx <- matrix( ptn ,nrow=3 ,ncol=6 ,byrow=T )
		testLst[[(length(testLst)+1)]] <- testObj

		ptn <- c(   0,  0,  0,  1,  1,  0
				 ,  0,  0,  0,  0,  0,  0
				 ,  0,  0,  0,  0,  0,  0
				)
		testObj <- list( hIdx=c( 210, 220 ) )
		testObj$ptnMtx <- matrix( ptn ,nrow=3 ,ncol=6 ,byrow=T )
		testLst[[(length(testLst)+1)]] <- testObj

		ptn <- c(   0,  0,  0,  0,  0,  0
				 ,  0,  1,  1,  0,  0,  0
				 ,  0,  0,  0,  0,  0,  0
				)
		testObj <- list( hIdx=c( 310 ) )
		testObj$ptnMtx <- matrix( ptn ,nrow=3 ,ncol=6 ,byrow=T )
		testLst[[(length(testLst)+1)]] <- testObj
		# --------------------------------------------------

		vFieldMtx <- pFieldMtx
		vFieldMtx[,] <- 0
		for( idx in 1:length(testLst) ){
			testObj <- testLst[[idx]]
			for( hIdx in testObj$hIdx ){
				vFieldMtx[hIdx:(hIdx+2),] <- testObj$ptnMtx[,]
			}
		}

		filmObj <- getDefaultFilm(vFieldMtx)

		rObj <- list( idStr=sprintf("dummy.ptn") )
		rObj$vFieldMtx <- vFieldMtx
		rObj$film <- filmObj$filmMtx
		rObj$filmActivated <- filmObj$filmActivated
		return( rObj )

	} # getFieldSet.dummy.ptn()

	

buildField <- function( pFieldMtx=as.matrix(FB$zh) ){

		colN <- NULL
		vFieldMtx	<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )
		filmMtx		<- matrix( F ,nrow=nrow(pFieldMtx) ,ncol=0 )

		# getFieldSet.dummy( ) -------------------------------------------------
		fObj <- getFieldSet.dummy( pFieldMtx=pFieldMtx )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		
		# getFieldSet.left( ) -------------------------------------------------
		fObj <- getFieldSet.left( pFieldMtx=pFieldMtx ,pBase=2 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		fObj <- getFieldSet.left( pFieldMtx=pFieldMtx ,pBase=4 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )

		# getFieldSet.dev( ) -------------------------------------------------
		fObj <- getFieldSet.dev( pFieldMtx=pFieldMtx ,pBase=3 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		fObj <- getFieldSet.dev( pFieldMtx=pFieldMtx ,pBase=5 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		fObj <- getFieldSet.dev( pFieldMtx=pFieldMtx ,pBase=10 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )


		# getFieldSet.reb( ) -------------------------------------------------
		fObj <- getFieldSet.reb( pFieldMtx=pFieldMtx ,pDepth=1 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		fObj <- getFieldSet.reb( pFieldMtx=pFieldMtx ,pDepth=2 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		fObj <- getFieldSet.reb( pFieldMtx=pFieldMtx ,pDepth=3 )
		vFieldMtx	<- cbind( vFieldMtx	,fObj$vFieldMtx	)
		filmMtx		<- cbind( filmMtx	,fObj$film	)
		colN <- c( colN ,sprintf("%s_%02d",fObj$idStr,1:ncol(fObj$vFieldMtx)) )
		
		# getFieldSet.left( ) -------------------------------------------------


		rObj <- list( rawFieldMtx = vFieldMtx )
		rObj$vFieldMtx <- vFieldMtx
		rObj$vFieldMtx[!filmMtx] <- NA
		rObj$filmMtx <- filmMtx
		rObj$colN <- colN
		return( rObj )
	}
