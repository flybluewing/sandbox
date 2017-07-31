#-[Filters]-------------------------------------------------
filtF.oneIdx <- function( pMatPtn ,pIdxMtx ){ 1==nrow(pIdxMtx) }
filtF.oneRow <- function( pMatPtn ,pIdxMtx ){ rIdx <-unique(idxMtx[,1]); return(1>=length(rIdx)) }
filtF.lastRowMiss <- function( pMatPtn ,pIdxMtx ){
		rIdx <- unique(pIdxMtx[,1])
		return( (1>=length(rIdx)) || !(nrow(pMatPtn) %in% rIdx) )
	}

#-----------------------------------------------------------

#-----------------------------------------------------------
FB <- getFlagBank()
winDef <- tSample.winDef06()

win.height <- 4;	wIdx <- 42
fbObj <- buildField( pFieldMtx=as.matrix(FB$zh) );	dim(fbObj$vFieldMtx)
winDef <- getWindow( pField=fbObj$vFieldMtx	,pFilm=fbObj$filmMtx
						,pHeight=win.height ,pInitPos=wIdx ,pIdStr=sprintf("wObj%03d",wIdx)
					)



rawPtn <- scanRawPattern( winDef ,pFiltF=filtF.lastRowMiss ,pBuffSize=1 ,pDebug=T )
overlap1 <- findOverlap( pScanRst=rawPtn ,pBuffSize=200 ,pDebug=T
				,pFiltF=filtF.lastRowMiss
			)
overlap2 <- findOverlap( pScanRst=overlap1 ,pBuffSize=200 ,pDebug=T )
ptnObj <- list( winDef=winDefwinDef ,overlap1=overlap1 ,overlap2=overlap2 )
save( ptnObj ,file="Obj_ptnObj.test0411.save" )


rSize <- sapply(overlap1$matchLst,function(p){ length(unique(p$idxMtx[,1])) })
sample.idx <- which(rSize>2)
ptnObj <- createPtnObj( overlap1$matchLst[[(sample.idx[1])]]$matMtx ,pSearchDepth=1 )

sample2.idx <- which(rSize==2)
ptnObj <- createPtnObj( overlap1$matchLst[[(sample.idx[1])]]$matMtx ,pSearchDepth=1 )


pIndices <- sort(unique(as.vector(overlap1$matchLst[[1]]$compPair[1:9,])))
ptnObj <- createPtnObj( overlap1$matchLst[[1]]$matMtx )
pScanRst=rawPtn ,pBuffSize=200 ,pIndices=pIndices ,pExcFilm=(!ptnObj$film) ,pFiltF=NULL ,pDebug=T
myOl <- findOverlap.within( pScanRst=rawPtn ,pBuffSize=200 ,pIndices=pIndices ,pExcFilm=(!ptnObj$film) ,pFiltF=NULL ,pDebug=T )
excFilm <- (!ptnObj$film); excFilm[nrow(excFilm),]<-T
myOl2 <- findOverlap.within( pScanRst=myOl ,pBuffSize=200 ,pExcFilm=excFilm ,pFiltF=NULL ,pDebug=T )


# scanRawPattern.within( pWin=winDef ,pIndices=pIndices , pFiltF=NULL ,pExcFilm=excFilm ,pDebug=T )
# pWin=winDef ;pIndices=pIndices ;pFiltF=NULL ;pExcFilm=excFilm ;pDebug=T
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
			compPairMtx.val <- matrix(-1,ncol=2,nrow=buffSize)	# compPair 생성 속도향상을 위해.
				# oIdx, iIdx 쌍을 저장.(hIdx가 아닌 pScanRst$matchLst의 인덱스임을 유의)
			compPairMtx.idx <- rep(-1,buffSize)	
				# matchLst의 인덱스.
				# default -1 사용 : 값이 입력된 곳은 자연히 정수일 것이므로.
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


