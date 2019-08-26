#	fMtx 박스 생성

getScoreMtx.grp.4H <- function( aZoid ,filter.grp ){
	#	aZoidMtx <- matrix( c( 8,22,35,38,39,41) ,nrow=1 )

	aZoidMtx <- matrix( aZoid ,nrow=1 )
	return( getScoreMtx.grp(aZoidMtx,filter.grp,makeInfoStr=T) )

} # getScoreMtx.grp.4H()

getScoreMtx.grp <- function( aZoidMtx ,filter.grp ,makeInfoStr=F ,cutter.grp=NULL ,tgt.scMtx=NULL ){

	rObj <- list( basic=list() ,bDup=list() ,mf=list() )

	for( nIdx in names(filter.grp$basic) ){
		scoreMtxLst <- list()
		for( nIdx.s in names(filter.grp$basic[[nIdx]]) ){
			filterObj <- filter.grp$basic[[nIdx]][[nIdx.s]]
			scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=makeInfoStr )

			#	QQE:todo cutter.grp 적용이 가능하도록 기능 추가할 것.
			#		- scoreMtx 계산 즉시 cutting..
			scoreMtxLst[[nIdx.s]] <- scoreMtxObj
		}
		rObj$basic[[nIdx]] <- scoreMtxLst
	}

	for( nIdx.s in names(filter.grp$bDup) ){
		filterObj <- filter.grp$bDup[[nIdx.s]]
		scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=is.null(cutter.grp) )

		#	QQE:todo cutter.grp 적용이 가능하도록 기능 추가할 것.
		rObj$bDup[[nIdx.s]] <- scoreMtxObj
	}

	for( nIdx.s in names(filter.grp$mf) ){
		filterObj <- filter.grp$mf[[nIdx.s]]
		scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=is.null(cutter.grp) )

		#	QQE:todo cutter.grp 적용이 가능하도록 기능 추가할 것.
		rObj$mf[[nIdx.s]] <- scoreMtxObj
	}

	return(rObj)

} # getScoreMtx.grp()



getScoreMtx.grp_byFCol <- function( scoreMtx.grp ){
	#	scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,] ,filter.grp )
	#	scoreMtx의 각 fCol 별 Mtx. column은 phase.
	#		(Column이 phase 이므로 기본 phase만 가능하다.)
	phaseName <- names(scoreMtx.grp$basic)
	mtxInfoLst <- lapply(scoreMtx.grp$basic$basic ,function( scoreObj ){ colnames(scoreObj$scoreMtx) })
	rowSize <- nrow(scoreMtx.grp$basic[["basic"]][[1]]$scoreMtx)

	# hMtx_byFCol[["sfcLate"]][["score2"]][["rebV.r"]]
	mLst <- list()
	for( mName in names(mtxInfoLst) ){	# mName <- names(mtxInfoLst)[1]
		fColLst <- list()
		for( fcName in mtxInfoLst[[mName]] ){	# fcName <- mtxInfoLst[[mName]][1]
			mtx <- matrix( 0, nrow=rowSize, ncol=length(phaseName) )
			colnames(mtx) <- phaseName
			for( pName in phaseName ){	# pName <- phaseName[1]
				mtx[,pName] <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx[,fcName]
			}
			fColLst[[fcName]] <- mtx
		}
		mLst[[mName]] <- fColLst
	}

	return( mLst )

} # getScoreMtx.grp_byFCol( )

#	byHIdx이긴 하지만, 사실은 각 scoreMtxN 에 대한 [col,phase] 테이블이다.
#	즉 allIdx 단위별로 List가 만들어짐.
getScoreMtx.grp_byHIdx <- function( scoreMtx.grp ){
	#	scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,] ,filter.grp )
	#	aZoid, scoreMtx별로 [fCol,phName] 구조.
	#		(Column이 phase 이므로 기본 phase만 가능하다.)
	phaseName <- names(scoreMtx.grp$basic)
	mtxInfoLst <- lapply(scoreMtx.grp$basic$basic ,function( scoreObj ){ colnames(scoreObj$scoreMtx) })
	rowSize <- nrow(scoreMtx.grp$basic[["basic"]][[1]]$scoreMtx)

	# hMtx_byHIdx[["sfcLate"]][["score2"]][["820"]]
	mLst <- list()
	for( mName in names(mtxInfoLst) ){	# mName <- names(mtxInfoLst)[1]
		aZoidLst <- list()
		for( aIdx in seq_len(rowSize) ){ # aIdx <- 1
			mtx <- matrix( 0, nrow=length(mtxInfoLst[[mName]]), ncol=length(phaseName) )
			colnames(mtx) <- phaseName	;rownames(mtx) <- mtxInfoLst[[mName]]
			for( pName in phaseName ){	# pName <- phaseName[1]
				mtx[,pName] <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx[aIdx,]
			}
			aZoidLst[[aIdx]] <- mtx
		}
		mLst[[mName]] <- aZoidLst
	}

	return( mLst )

} # getScoreMtx.grp_byHIdx( )



getFilter.grp <- function( stdMI.grp ,tgt.scMtx=NULL ){

	getMtxObjLst <- function( stdMIObj ){
		mtxObjLst <- list()
		if( is.null(tgt.scMtx) || ("score2" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score2( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score3" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score3( stdMIObj )
		}
		names(mtxObjLst) <- sapply(mtxObjLst,function(p){p$idStr})
		return( mtxObjLst )
	}

	rObj <- list()
	rObj$basic <- lapply( stdMI.grp$basic ,getMtxObjLst )
	rObj$bDup <- lapply( stdMI.grp$bDup ,getMtxObjLst )
	rObj$mf <- lapply( stdMI.grp$mf ,getMtxObjLst )

	return( rObj )
}



bFMtx.score2 <- function( stdMIObj ){
	# stdMIObj	: stdMI ,zMtx
	#		stdMIObj <- stdMI.grp$basic[[1]]

	#	...ab. <-- left slide ( k, a, b 패턴에 대한 V 값)
	#   ..k...
	#   .V....

	getSlideReb <- function( pZMtx ){

		hSize <- nrow(pZMtx)
		if( 3>hSize ){ return( NULL ) }

		hSpan <- (hSize-1):2		;hWidth <- ncol(pZMtx)

		rObj <- list()	;dbgObj <- list()

		if( TRUE ){	# left slide
			colSpan <- 1:(hWidth-2)
			rName <- c("col","val","ref1","ref2")
			lMtx <- matrix( NA, nrow=length(rName), ncol=length(colSpan) )
			rownames( lMtx ) <- rName	;colnames( lMtx ) <- paste( "c" ,colSpan,sep="")
			lMtx["col",] <- colSpan

			for( idx in 1:ncol(lMtx) ){
				colIdx <- lMtx["col",idx]
				lMtx[c("ref1","ref2"),idx] <- c( pZMtx[hSize,colIdx+1] ,pZMtx[hSize-1,colIdx+2] )
			}

			dbgInfo <- list()
			for( hIdx in hSize:3 ){
				sObj <- getSlideReb.ptnLst( pZMtx ,hIdx ,"left" )
				for( idx in 1:ncol(lMtx) ){
					if( !is.na(lMtx["val",idx]) ){
						next
					}
					for( lIdx in 1:length(sObj) ){
						matFlag <- lMtx[c("ref1","ref2"),idx]==sObj[[lIdx]]$val[c("ref-1","ref-2")]
						if( all(matFlag) ){
							lMtx["val",idx] <- sObj[[lIdx]]$val["tgtV"]
							dbgInfo[[1+length(dbgInfo)]] <- c( hIdx ,idx ,lIdx ,sObj[[lIdx]]$val[c("ref-1","ref-2")] )
							names(dbgInfo[[length(dbgInfo)]]) <- c("hIdx","idx","lIdx",c("ref-1","ref-2"))
							break
						}
					}
				}

				if( all(!is.na(lMtx["val",])) ) {
					break
				}
			}
			#	hIdx<-719    ;pZMtx[(hIdx-4):hIdx,]
			rObj$lMtx <- lMtx
		}

		if( TRUE ){ # right slide
			colSpan <- 3:hWidth
			rName <- c("col","val","ref1","ref2")
			rMtx <- matrix( NA, nrow=length(rName), ncol=length(colSpan) )
			rownames( rMtx ) <- rName	;colnames( rMtx ) <- paste( "c" ,colSpan,sep="")
			rMtx["col",] <- colSpan

			for( idx in 1:ncol(rMtx) ){
				colIdx <- rMtx["col",idx]
				rMtx[c("ref1","ref2"),idx] <- c( pZMtx[hSize,colIdx-1] ,pZMtx[hSize-1,colIdx-2] )
			}

			dbgInfo <- list()
			for( hIdx in hSize:3 ){
				sObj <- getSlideReb.ptnLst( pZMtx ,hIdx ,"right" )
				for( idx in 1:ncol(rMtx) ){
					if( !is.na(rMtx["val",idx]) ){
						next
					}
					for( lIdx in 1:length(sObj) ){
						matFlag <- rMtx[c("ref1","ref2"),idx]==sObj[[lIdx]]$val[c("ref-1","ref-2")]
						if( all(matFlag) ){
							rMtx["val",idx] <- sObj[[lIdx]]$val["tgtV"]
							dbgInfo[[1+length(dbgInfo)]] <- c( hIdx ,idx ,lIdx ,sObj[[lIdx]]$val[c("ref-1","ref-2")] )
							names(dbgInfo[[length(dbgInfo)]]) <- c("hIdx","idx","lIdx",c("ref-1","ref-2"))
							break
						}
					}
				}

				if( all(!is.na(rMtx["val",])) ) {
					break
				}
			}
			#	hIdx<-333    ;pZMtx[(hIdx-4):hIdx,]

			rObj$rMtx <- rMtx
		}

		return( rObj )
	}	# getSlideReb()
	getSlideReb.ptnLst <- function( pZMtx ,curHIdx ,direc="left" ){
		rObj <- list()

		hWidth <- ncol(pZMtx)
		if( "left"==direc ){
			cSpan <- 1:(hWidth-2)
			for( cIdx in cSpan ){
				uObj <- list( col=cIdx )
				uObj$val <- c( pZMtx[curHIdx,cIdx] ,pZMtx[curHIdx-1,cIdx+1] ,pZMtx[curHIdx-2,cIdx+2] )
				names( uObj$val ) <- c("tgtV","ref-1","ref-2")
				rObj[[1+length(rObj)]] <- uObj
			}
		} else {
			cSpan <- 3:hWidth
			for( cIdx in cSpan ){
				uObj <- list( col=cIdx )
				uObj$val <- c( pZMtx[curHIdx,cIdx] ,pZMtx[curHIdx-1,cIdx-1] ,pZMtx[curHIdx-2,cIdx-2] )
				names( uObj$val ) <- c("tgtV","ref-1","ref-2")
				rObj[[1+length(rObj)]] <- uObj
			}
		}
		return( rObj )
	}	# getSlideReb.ptnLst()

	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	zMtx.size <- nrow(zMtx)
	rObj <- list( idStr="score2" ,lastZoid=stdMI$lastZoid ,lastCStep=stdMI$cStep ,lastFStep=stdMI$fStep ,zMtx.size=zMtx.size )

	if( TRUE ){
		rObj$lastZoid.H2	<-	if( 2>stdMI$mtxLen ) NULL	 else stdMI$rawTail[nrow(stdMI$rawTail)-1,]
		rObj$lastCStep.H2	<-	if( 2>stdMI$mtxLen ) NULL	 else stdMI$cStepTail[nrow(stdMI$cStepTail)-1,]
		rObj$lastFStep.H2	<-	if( 3>stdMI$mtxLen ) NULL	 else stdMI$fStepTail[nrow(stdMI$fStepTail)-1,]
									# fStepTail[1,] 은 NA

		inc.stdRaw		<- if( 2>stdMI$mtxLen ){ NULL 
							} else {	vDiff <- stdMI$lastZoid - zMtx[stdMI$mtxLen-1,]
										stdMI$lastZoid+vDiff
							}
		inc.stdRaw2		<- if( 4>stdMI$mtxLen ){ NULL 
							} else {	vDiff <- zMtx[stdMI$mtxLen-1,] - zMtx[stdMI$mtxLen-3,]
										zMtx[stdMI$mtxLen-1,]+vDiff
							}
		inc.stdRaw3		<- if( 6>stdMI$mtxLen ){ NULL 
							} else {	vDiff <- zMtx[stdMI$mtxLen-2,] - zMtx[stdMI$mtxLen-5,]
										zMtx[stdMI$mtxLen-2,]+vDiff
							}
		inc.stdCStep	<- if( 2>stdMI$mtxLen ){ NULL 
							} else {	h2Zoid <- zMtx[stdMI$mtxLen-1,]
										vDiff <- stdMI$cStep - (h2Zoid[2:6]-h2Zoid[1:5])
										stdMI$cStep+vDiff
							}
		inc.stdCStep2	<- if( 4>stdMI$mtxLen ){ NULL 
							} else {	cStep <- zMtx[,2:6] - zMtx[,1:5]
										vDiff <- cStep[stdMI$mtxLen-1,] - cStep[stdMI$mtxLen-3,]
										cStep[stdMI$mtxLen-1,]+vDiff
							}
		inc.stdCStep3	<- if( 6>stdMI$mtxLen ){ NULL 
							} else {	cStep <- zMtx[,2:6] - zMtx[,1:5]
										vDiff <- cStep[stdMI$mtxLen-2,] - cStep[stdMI$mtxLen-5,]
										cStep[stdMI$mtxLen-2,]+vDiff
							}

		fStepLen <- ifelse( is.null(stdMI$fStepTail) ,0 ,nrow(stdMI$fStepTail)-1 )	# # fStepTail[1,] 은 NA
		inc.stdFStep	<- if( 2>fStepLen ){ NULL 
							} else {	vDiff <- stdMI$fStep - stdMI$fStepTail[fStepLen,]
										stdMI$fStep+vDiff
							}
		inc.stdFStep2	<- if( 4>fStepLen ){ NULL 
							} else {	vDiff <- stdMI$fStepTail[fStepLen,] - stdMI$fStepTail[fStepLen,]
										stdMI$fStepTail[fStepLen-1,]+vDiff
							}

		rObj$inc.stdRaw <- inc.stdRaw	;rObj$inc.stdRaw2 <- inc.stdRaw2	;rObj$inc.stdRaw3 <- inc.stdRaw3
		rObj$inc.stdCStep <- inc.stdCStep	;rObj$inc.stdCStep2 <- inc.stdCStep2	;rObj$inc.stdCStep3 <- inc.stdCStep3
		rObj$inc.stdFStep <- inc.stdFStep	;rObj$inc.stdFStep2 <- inc.stdFStep2

		rObj$slideObj <- getSlideReb( zMtx )
		
	}

	#	colNames :	"rebV.r","rebL","rebR"											--> 동일 값 발생, 사선 방향 동일 패턴 발생.
	#				,"rebC.r","rebC.c","rebC.f","rebC2,r","rebC2,c","rebC2,f"		--> 동일컬럼 값 재발생.
	#				,"inc.r","inc.c","inc.f","inc.r2","inc.c2","inc.f2","inc.r3","inc.c3","inc.f3"	--> 증감 패턴이 다음에도 유지.
	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	fMtxObj <- list( scoreMtx ,infoMtx )
		aLen <- nrow(aZoidMtx)
		cName <- c("rebV.r","rebL","rebR","rebC.r","rebC.c","rebC.f","rebC2.r","rebC2.c","rebC2.f")
		cName <- c( cName, c("inc.r","inc.c","inc.f","inc.r2","inc.c2","inc.f2","inc.r3","inc.c3") )
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c("rebSlide","zMtx.size")
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}

		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			aCStep <- aZoid[2:6] - aZoid[1:5]
			aFStep <- aZoid - rObj$lastZoid
			#	working
			scoreMtx[aIdx,"rebV.r"] <- length( intersect(rObj$lastZoid, aZoid) )
			if( !is.null(rObj$slideObj) ){
				scoreMtx[aIdx,"rebL"] <- sum(aZoid[rObj$slideObj$lMtx["col",]]==rObj$slideObj$lMtx["val",] ,na.rm=T )
				scoreMtx[aIdx,"rebR"] <- sum(aZoid[rObj$slideObj$rMtx["col",]]==rObj$slideObj$rMtx["val",] ,na.rm=T )
			}

			scoreMtx[aIdx,"rebC.r"] <- sum( rObj$lastZoid==aZoid )
			scoreMtx[aIdx,"rebC.c"] <- sum( rObj$lastCStep==aCStep )
			scoreMtx[aIdx,"rebC.f"] <- sum( rObj$lastFStep==aFStep )
			scoreMtx[aIdx,"rebC2.r"] <- sum( rObj$lastZoid.H2==aZoid )
			scoreMtx[aIdx,"rebC2.c"] <- sum( rObj$lastCStep.H2==aCStep )
			scoreMtx[aIdx,"rebC2.f"] <- sum( rObj$lastFStep.H2==aFStep )

			scoreMtx[aIdx,"inc.r"] <- sum( rObj$inc.stdRaw==aZoid )
			scoreMtx[aIdx,"inc.c"] <- sum( rObj$inc.stdCStep==aCStep )
			scoreMtx[aIdx,"inc.f"] <- sum( rObj$inc.stdFStep==aFStep )
			scoreMtx[aIdx,"inc.r2"] <- sum( rObj$inc.stdRaw2==aZoid )
			scoreMtx[aIdx,"inc.c2"] <- sum( rObj$inc.stdCStep2==aCStep )
			scoreMtx[aIdx,"inc.f2"] <- sum( rObj$inc.stdFStep2==aFStep )
			scoreMtx[aIdx,"inc.r3"] <- sum( rObj$inc.stdRaw3==aZoid )
			scoreMtx[aIdx,"inc.c3"] <- sum( rObj$inc.stdCStep3==aCStep )

			if( makeInfoStr ){
				if( is.null(rObj$slideObj) ){ infoMtx[aIdx,"rebSlide"] <- "N/A"
				} else {
					infoMtx[aIdx,"rebSlide"] <- sprintf("cnt rebL:%d rebR:%d"
													,sum(!is.na(rObj$slideObj$lMtx["val",]))
													,sum(!is.na(rObj$slideObj$rMtx["val",]))
												)
				}
			}
		}

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}
	return( rObj )
} # bFMtx.score2( )


bFMtx.score3 <- function( stdMIObj ){

	# zMtx : 각 ph에서의 히스토리.
	#	zMtx <- gEnv$zhF
	getRebPtn.1 <- function( stdMI ){
		rObj <- list( matInfo=matrix(0,nrow=0,ncol=4) )
		rowLen <- nrow( stdMI$rawTail )
		if( 2>rowLen ) return( rObj )

		matLst <- list()
		for( rIdx in rowLen:2 ){
			cVal <- intersect(stdMI$rawTail[rIdx,] ,stdMI$rawTail[rIdx-1,])
			if( 1!=length(cVal) ) next

			matLst[[1+length(matLst)]] <- c( rIdx, cVal
												, which(stdMI$rawTail[rIdx-1,]==cVal) 
												, which(stdMI$rawTail[rIdx  ,]==cVal)
											)
		}

		if( 0<length(matLst) ){
			matInfo <- do.call( rbind ,matLst )
			colnames( matInfo ) <- c("row","val","fromC","toC")
			rObj$matInfo <- matInfo
		}
		return( rObj )
	} # getRebPtn.1()
	getRebPtn.n <- function( stdMI ){
		rObj <- list( )
		rowLen <- nrow( stdMI$rawTail )
		if( 2>rowLen ) return( rObj )

		matLst <- list()	;matInfo <- NULL
		for( rIdx in rowLen:2 ){
			cVal <- intersect(stdMI$rawTail[rIdx,] ,stdMI$rawTail[rIdx-1,])
			if( 2>length(cVal) ) next

			matMtx <- matrix( NA, nrow=2, ncol=length(cVal) )
			rownames(matMtx) <- c("from","to")	;colnames(matMtx) <- paste("val",cVal)
			for( idx in seq_len(length(cVal)) ){
				val <- cVal[idx]
				matMtx["from",idx] <- which(stdMI$rawTail[rIdx-1,]==val)
				matMtx["to"  ,idx] <- which(stdMI$rawTail[rIdx  ,]==val)
			}
			matInfo <- c( matInfo ,sprintf("%d:%s",rIdx,paste(cVal,collapse=",")) )
			matLst[[1+length(matLst)]] <- matMtx
		}
		names(matLst) <- matInfo

		return( matLst )
	} # getRebPtn.n()
	getSeqPtn <- function( mtx ){
		rObj <- list( )
		rowLen <- ifelse( is.null(mtx) ,0 ,nrow( mtx ) )
		colLen <- ifelse( is.null(mtx) ,0 ,ncol( mtx ) )
		if( 2>rowLen ){
			rObj$filt <- function( aCode ){ return( list( matCnt=0 ) ) }
			return( rObj )
		}

		banLst <- list()
		for( cIdx in 1:colLen ){	# lastCode
			lc <- mtx[rowLen,cIdx]
			fColIdx <- integer(0)
			fRowIdx <- integer(0)
			dbgStr <- ""
			for( rIdx in (rowLen-1):1 ){
				fColIdx <- which(mtx[rIdx,]==lc)
				if( 0<length(fColIdx) ){
					fRowIdx <- rIdx
					dbgStr <- sprintf("col:%d(val:%d)  found in row:%d col:%s",cIdx,lc,fRowIdx,paste(fColIdx,collapse=","))
					break
				}
			}

			dbgStr <- ""
			for( fcIdx in fColIdx ){
				olSpan <- fCutU.overlapSpan( colLen ,colIdx.pre=fcIdx ,colIdx.post=cIdx )
				if( 1>sum(olSpan$info[c("lMargin","rMargin")]) )	next

				valInc <- mtx[fRowIdx+1,olSpan$span.pre]-mtx[fRowIdx,olSpan$span.pre]
				banVal <- mtx[rowLen,olSpan$span.post]+valInc
				fixPoint <- banVal	;fixPoint[-(olSpan$info["lMargin"]+1)] <- NA
				dbgStr <- sprintf("colIdx:%d(val:%d) from (%d,%d)  %s/%s --> %s/%s..?",cIdx,lc,fRowIdx,fcIdx
									,paste(mtx[fRowIdx  ,olSpan$span.pre],collapse=",")
									,paste(mtx[fRowIdx+1,olSpan$span.pre],collapse=",")
									,paste(mtx[rowLen,olSpan$span.post],collapse=",")
									,paste(banVal,collapse=",")
								)
				banObj <- list( banVal=banVal ,banSpan=olSpan$span.post ,fixPoint=fixPoint ,dbgStr=dbgStr )
				banLst[[1+length(banLst)]] <- banObj
			}
			
		}

		rObj$banLst <- banLst

		rObj$filt <- function( aCode ){
			rstObj <- list( matCnt=0 )
			if( 0==length(rObj$banLst) ) return( rstObj )

			matCnt <- sapply( rObj$banLst ,function( banInfo ){
				cnt <- sum( aCode[banInfo$banSpan] == banInfo$banVal )
				flagFixPoint <- all(aCode[banInfo$banSpan]==banInfo$fixPoint,na.rm=T)
				if( flagFixPoint ){
					return( cnt )
				} else {
					return( 0 )
				}
			})

			rstObj$matCnt = matCnt
			return( rstObj )
		}

		return( rObj )
	} # getSeqPtn()

	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="score3"	,zMtx.size=nrow(zMtx)
					,lastZoid=stdMI$lastZoid
					,rebPtn.1=getRebPtn.1(stdMI)	,rebPtn.n=getRebPtn.n( stdMI )
					,seqNextPtn.raw=getSeqPtn( stdMI$rawTail )	,seqNextPtn.cStep=getSeqPtn( stdMI$cStepTail )
				)

	#	cName <- c("rebPtn.1","rebPtn.n","snMax.r" ,"snFCnt.r" ,"snMax.c" ,"snFCnt.c")
	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		aLen <- nrow(aZoidMtx)
		cName <- c("rebPtn.1","rebPtn.n","snMax.r" ,"snFCnt.r" ,"snMax.c" ,"snFCnt.c")
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c("rebPtn.1","rebPtn.n","snXXX.r","snXXX.c","zMtx.size")
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}
		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			aCStep <- aZoid[2:6] - aZoid[1:5]
			aFStep <- aZoid - rObj$lastZoid

			# rebPtn.1
			infoStr.rebPtn.1 <- ""
			if( 0<nrow(rObj$rebPtn.1$matInfo) ){
				reb.lastZoid <- rObj$lastZoid[rObj$rebPtn.1$matInfo[,"fromC"]]
				reb.aZoid <- aZoid[rObj$rebPtn.1$matInfo[,"toC"]]
				scoreMtx[aIdx,"rebPtn.1"] <- sum( reb.aZoid==reb.lastZoid )
				if( 0<scoreMtx[aIdx,"rebPtn.1"] ){
					fromCol <- rObj$rebPtn.1$matInfo[,"fromC"][reb.aZoid==reb.lastZoid]
					toCol <- rObj$rebPtn.1$matInfo[,"toC"][reb.aZoid==reb.lastZoid]
					infoStr.rebPtn.1 <- sprintf("reb col: (%s)->(%s)",paste(fromCol,collapse=","),paste(toCol,collapse=","))
				}
			}

			# rebPtn.n
			infoStr.rebPtn.n <- ""
			if( length(rObj$rebPtn.n)>0 ){
				flag <- sapply( rObj$rebPtn.n ,function( matMtx ){
								fromVal <- rObj$lastZoid[matMtx["from",]]
								toVal <- aZoid[matMtx["to",]]
								return( all(fromVal==toVal) )
							})
				scoreMtx[aIdx,"rebPtn.n"] <- sum( flag )
				if( 0<scoreMtx[aIdx,"rebPtn.n"] ){
					infoStr.rebPtn.n <- sprintf("%s",paste(names(flag)[flag],collapse=" "))
				}
			}

			#	"sncMax.raw" ,"sncFCnt.raw" 
			snMatCnt.raw <- rObj$seqNextPtn.raw$filt( aZoid )$matCnt
			scoreMtx[aIdx,"snMax.r"] <- max( snMatCnt.raw )
			scoreMtx[aIdx,"snFCnt.r"] <- sum( snMatCnt.raw>=2 )

			#	"sncMax.cStep" ,"sncFCnt.cStep"
			snMatCnt.cStep <- rObj$seqNextPtn.cStep$filt( aZoid )$matCnt
			scoreMtx[aIdx,"snMax.c"] <- max( snMatCnt.cStep )
			scoreMtx[aIdx,"snFCnt.c"] <- sum( snMatCnt.cStep>=2 )

			if( makeInfoStr ){
				infoMtx[aIdx,"rebPtn.1"] <- if( nrow(rObj$rebPtn.1$matInfo) >0 ) infoStr.rebPtn.1 else "N/A"
				infoMtx[aIdx,"rebPtn.n"] <- if( length(rObj$rebPtn.n) >0 ) infoStr.rebPtn.n else "N/A"
				infoMtx[aIdx,"snXXX.r"] <- if( 0==sum(snMatCnt.raw) ) "N/A" else sprintf("matCnt:%s",paste(snMatCnt.raw,collapse=" "))
				infoMtx[aIdx,"snXXX.c"] <- if( 0==sum(snMatCnt.cStep) ) "N/A" else sprintf("matCnt:%s",paste(snMatCnt.cStep,collapse=" "))
			}

		}
		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}
	return( rObj )

} # bFMtx.score3( )

