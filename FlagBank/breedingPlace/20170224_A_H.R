# 2017.04.02 구조 재설계를 위해 Phase B로 넘어간다.


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

		idxStr <- apply(mfIdxMtx,1,function(p){paste(sprintf("%04d",p),collapse="")})
		rMObj <- list( idStr = paste(idxStr,collapse="") )
		rMObj$mfIdxMtx <- mfIdxMtx
		rMObj$mfMtx <- mfMtx
		rMObj$compPair <- sort(c(pIdxA,pIdxB))
		
		return( rMObj )
	}

# compareFilm() 함수의 Logical Flag 처리버전.
#	pMatchLst에는 matMtx 대신 T/F 값이 담긴 mfMtx가 들어옴.
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

		idxStr <- apply(mfIdxMtx,1,function(p){paste(sprintf("%04d",p),collapse="")})
		rMObj <- list( idStr = paste(idxStr,collapse="") )
		rMObj$mfIdxMtx <- mfIdxMtx
		rMObj$mfMtx <- mfMtx
		rMObj$compPair <- sort(c(pIdxA,pIdxB))

		return( rMObj )
	}
	
# QQE : check
reportWinDef <- function( pWinRst ,pIdx ,pRptFile="./report/winDef" ){

		winDef <- pWinRst$winDefLst[[pIdx]]
		stageLst <- pWinRst$stageLst[[pIdx]]

		log.txt <- sprintf("%s.txt",pRptFile)
		log.pdf <- sprintf("%s.pdf",pRptFile)		
		FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
				k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
		FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
				k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}

		FLogStr(sprintf("ID:%s",winDef$idStr),pConsole=T,pTime=T,pAppend=F)
		FLogStr(sprintf("    initPos:%d height:%d width:%d",winDef$initPos,winDef$height,winDef$width))
		FLogStr(sprintf("    Field Dim : nrow %d  ncol %d",winDef$fieldDim[1],winDef$fieldDim[2]))

		FLogStr("    \n Film")
		colStr <- paste(sprintf(" %04d",1:ncol(winDef$film)),collapse="")
		FLogStr(sprintf("    %s",colStr))
		for( rIdx in 1:nrow(winDef$film) ){
			FLogStr(sprintf("    %s",paste(ifelse(winDef$film[rIdx,],"   1","   ."),collapse=" ")))
		}

		for( sIdx in 1:length(stageLst) ){
			# sIdx <- 1
			rptFileName <- sprintf("%s_Stage%03d",pRptFile,sIdx)
			FLogStr(sprintf("<Stge %d of %d>",stageLst[[sIdx]]$depth,length(stageLst)))
			FLogStr(sprintf("    time stamp : %s    match set num : %d",stageLst[[sIdx]]$timeStamp,length(stageLst[[sIdx]]$matchLst)))
			FLogStr(sprintf("    report file : %s",rptFileName))
			reportStage( pStage=stageLst[[sIdx]] ,pRptFile=rptFileName )
		}

		FLogStr("End of Report",pTime=T)

	}

# QQE : check
reportStage <- function( pStage ,pRptFile="./report/stage" ){

		log.txt <- sprintf("%s.txt",pRptFile)
		log.pdf <- sprintf("%s.pdf",pRptFile)		
		FLog <- function( pObj ,pTime=F ,pAppend=T ,pConsole=F ){
				k.FLog( pObj ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}
		FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
				k.FLogStr( pMsg ,pFile=log.txt ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
			}

		FLogStr(sprintf("Stage Report(idStr.winDef:%s depth:%d)",pStage$idStr.winDef,pStage$depth),pConsole=T,pTime=T,pAppend=F)
		FLogStr(sprintf("    Match List size : %d",length(pStage$matchLst)))

		if( 0<length(pStage$matchLst) ){
		for( idx in 1:length(pStage$matchLst) ){
			# idx <- 1
			mtch <- pStage$matchLst[[idx]]
			FLogStr(sprintf("<%d th match info>",idx))
			FLogStr(sprintf("    idstr:%s",mtch$idStr))
			colStr <- paste(sprintf(" %4d",1:ncol(mtch$mfMtx)),collapse="")
			FLogStr(sprintf("    %s",colStr))
			for( rIdx in 1:nrow(mtch$mfMtx) ){
				vStr <- paste(ifelse(mtch$mfMtx[rIdx,],"    1","    ."),collapse="")
				FLogStr(sprintf("    %s",vStr))
			}
			
			FLogStr(sprintf("    point:%s",length(as.vector(mtch$compPair))))
			FLogStr(sprintf("        %s",paste(sort(unique(as.vector(mtch$compPair))),collapse=" ")))
		} # for
		} # if

		FLogStr("End of Report",pTime=T)

	}

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

getFieldSet.reb <- function( pFieldMtx=as.matrix(FB$zh) ,pDepth=2 ){

		nCol.field <- ncol(pFieldMtx)
		# Depth 내 동일 코드가 몇 번이나 재발생?
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

		rebMtx.rebF <- ifelse( rebMtx.byVal>0 ,1 ,0 ) # 중복이 존재하는지 자체
		rebMtx.rebF.sum <- matrix( apply(rebMtx.rebF,1,sum) ,ncol=1 )

		# Depth 내 동일 코드가 동일 위치에서 몇 번이나 재발생?
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
		rebMtx.rebFP <- ifelse( rebMtx.byValP>0 ,1 ,0 ) # 중복이 존재하는지 자체
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
		rObj$filmActivated <- T # film생성이 여러개이니... 걍 T 로 박아버리자.
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

buildField <- function( pFieldMtx=as.matrix(FB$zh) ){

		colN <- NULL
		vFieldMtx	<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )
		filmMtx		<- matrix( 0 ,nrow=nrow(pFieldMtx) ,ncol=0 )

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


		rObj <- list( vFieldMtx = vFieldMtx )
		rObj$filmMtx <- filmMtx
		rObj$colN <- colN
		return( rObj )
	}
