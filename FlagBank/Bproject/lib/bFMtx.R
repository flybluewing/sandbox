#	fMtx 박스 생성
#		- scoreN 을 추가할 때 마다 getFilter.grp() 에서도 적용해 줄 것.

getScoreMtx.grp.4H <- function( aZoid ,filter.grp ){
	#	aZoidMtx <- matrix( c( 8,22,35,38,39,41) ,nrow=1 )

	aZoidMtx <- matrix( aZoid ,nrow=1 )
	return( getScoreMtx.grp(aZoidMtx,filter.grp,makeInfoStr=T) )

} # getScoreMtx.grp.4H()

getScoreMtx.grp <- function( aZoidMtx ,filter.grp ,makeInfoStr=F ,cutter.grp=NULL ,tgt.scMtx=NULL ){
	#  ,cutter.grp=NULL ,tgt.scMtx=NULL

	rObj <- list( basic=list() ,bDup=list() ,mf=list() )
	
	for( nIdx in names(filter.grp$basic) ){
		scoreMtxLst <- list()
		scMtxName <- names(filter.grp$basic[[nIdx]])
		if( !is.null(tgt.scMtx) ){
			scMtxName <- intersect( tgt.scMtx ,scMtxName )
		}

		for( nIdx.s in scMtxName ){
			filterObj <- filter.grp$basic[[nIdx]][[nIdx.s]]
			scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=makeInfoStr )

			scoreMtxLst[[nIdx.s]] <- scoreMtxObj
		}
		rObj$basic[[nIdx]] <- scoreMtxLst
	}

	scMtxName <- names(filter.grp$bDup)
	if( !is.null(tgt.scMtx) ){
		scMtxName <- intersect( tgt.scMtx ,scMtxName )
	}
	for( nIdx.s in scMtxName ){
		filterObj <- filter.grp$bDup[[nIdx.s]]
		scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=is.null(cutter.grp) )

		rObj$bDup[[nIdx.s]] <- scoreMtxObj
	}

	scMtxName <- names(filter.grp$mf)
	if( !is.null(tgt.scMtx) ){
		scMtxName <- intersect( tgt.scMtx ,scMtxName )
	}
	for( nIdx.s in scMtxName ){
		filterObj <- filter.grp$mf[[nIdx.s]]
		scoreMtxObj <- filterObj$fMtxObj( aZoidMtx ,makeInfoStr=is.null(cutter.grp) )
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
		if( is.null(tgt.scMtx) || ("score1" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score1( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score2" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score2( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score3" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score3( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score4" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score4( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score5" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score5( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score6" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score6( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score7" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score7( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score8" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score8( stdMIObj )
		}
		if( is.null(tgt.scMtx) || ("score9" %in%tgt.scMtx ) ){
			mtxObjLst[[1+length(mtxObjLst)]] <- bFMtx.score9( stdMIObj )
		}
		names(mtxObjLst) <- sapply(mtxObjLst,function(p){p$idStr})
		return( mtxObjLst )
	}

	rObj <- list()
	rObj$basic <- lapply( stdMI.grp$basic ,getMtxObjLst )
	rObj$bDup <- lapply( stdMI.grp$bDup ,getMtxObjLst )


	rObj$mf <- list()
	bScrNames <- names(bFMtxB.BScrLst)
	if( !is.null(tgt.scMtx) ){	
		bScrNames <- intersect( tgt.scMtx ,bScrNames )
	}
	if( 0<length(bScrNames) ){
		stdMIObj <- stdMI.grp$basic$basic
		rObj$mf <- lapply( bFMtxB.BScrLst[bScrNames] ,function(bScr){ return(bScr(stdMIObj)) })
	}

	return( rObj )
}


#	fCutU.getFiltObjPair( stdMI$fStepTail )
bFMtx.score1 <- function( stdMIObj ){
	#	stdMIObj <- stdMI.grp$basic$basic
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="score1"	,zMtx.size=nrow(zMtx)
					,lastZoid=stdMI$lastZoid
				)

	findSeg <- function( aCode ){

		segLst <- list()

		tbl <- table(aCode)
		if( all(tbl<2) ){
			return( segLst )
		} else {
			tbl <- tbl[tbl>=2]
		}

		for( val in as.integer(names(tbl)) ){
			segObj <- list( val=val ,idx=which(aCode==val) )
			segLst[[1+length(segLst)]] <- segObj
		}
		names(segLst) <- paste("val",names(tbl),sep="")

		return( segLst )
	}

	rObj$lastSeg0 <- NULL	;rObj$lastSeg1 <- NULL		# rem
	rObj$lastSeg0.c <- NULL	;rObj$lastSeg1.c <- NULL	# cStep
	rObj$lastSeg0.f <- NULL	;rObj$lastSeg1.f <- NULL	# fStep
	if( 0<nrow(stdMI$rawTail) ){
		#	rObj$lastSeg0 ------------------------------------------
		segLst <- findSeg( stdMI$lastZoid %% 10 )
		val = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){obj$val})
		segLen = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){length(obj$idx)})
		rObj$lastSeg0 <- list( infoMtx=rbind(val,segLen)	,segLst=segLst	)

		#	rObj$lastSeg0.c ----------------------------------------
		segLst <- findSeg( stdMI$lastZoid[2:6] - stdMI$lastZoid[1:5] )
		val = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){obj$val})
		segLen = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){length(obj$idx)})
		rObj$lastSeg0.c <- list( infoMtx=rbind(val,segLen)	,segLst=segLst	)
	}
	if( 1<nrow(stdMI$rawTail) ){
		workZoid0 <- stdMI$lastZoid
		workZoid1 <- stdMI$rawTail[nrow(stdMI$rawTail)-1,]
		#	rObj$lastSeg1 ------------------------------------------		
		segLst <- findSeg( workZoid1 %% 10 )
		val = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){obj$val})
		segLen = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){length(obj$idx)})
		rObj$lastSeg1 <- list( infoMtx=rbind(val,segLen)	,segLst=segLst	)

		#	rObj$lastSeg1.c ----------------------------------------
		segLst <- findSeg( workZoid1[2:6] - workZoid1[1:5] )
		val = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){obj$val})
		segLen = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){length(obj$idx)})
		rObj$lastSeg1.c <- list( infoMtx=rbind(val,segLen)	,segLst=segLst	)

		#	rObj$lastSeg0.f ----------------------------------------
		segLst <- findSeg( workZoid0 - workZoid1 )
		val = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){obj$val})
		segLen = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){length(obj$idx)})
		rObj$lastSeg0.f <- list( infoMtx=rbind(val,segLen)	,segLst=segLst	)
	}
	if( 2<nrow(stdMI$rawTail) ){
		workZoid1 <- stdMI$rawTail[nrow(stdMI$rawTail)-1,]
		workZoid2 <- stdMI$rawTail[nrow(stdMI$rawTail)-2,]

		#	rObj$lastSeg1.f ----------------------------------------
		segLst <- findSeg( workZoid1 - workZoid2 )
		val = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){obj$val})
		segLen = if( 0==length(segLst) ) integer(0) else sapply( segLst ,function(obj){length(obj$idx)})
		rObj$lastSeg1.f <- list( infoMtx=rbind(val,segLen)	,segLst=segLst	)
	}
	rObj$checkSegMatch <- function( aCode ,segObj ){

		rVal <- c( num=0 ,len.tot=0 ,len.val=0 )
			#	num : 매치가 일어난 세그먼트 수
			#	len.tot : 매치가 일어난 세그먼트들의 길이를 합친 값.
			#	len.val : rem 값까지 일치한 세그먼트들의 길이를 합친 값.

		if( 0==length(segObj$segLst) ) return( rVal )

		for( lIdx in 1:length(segObj$segLst) ){
			seg <- segObj$segLst[[lIdx]]
			if( !bUtil.allSame(aCode[seg$idx]) )	next

			rVal["num"] <- 1 + rVal["num"]
			rVal["len.tot"] <- length(seg$idx) + rVal["len.tot"]

			if( seg$val==aCode[seg$idx[1]] ){
				rVal["len.val"] <- length(seg$idx) + rVal["len.val"]
			}
		}

		return( rVal )

	}

	rObj$zwBanMtx <- NULL
	if( 1<nrow(stdMI$rawTail) ){
		banLst <- list()
		zw <- zMtx[,6]-zMtx[,1]	;names(zw) <- rownames(zMtx)
		zw.len <- length(zw)

		#	a ... a!
		banLst[["H0"]] <- c(zw=zw[zw.len] ,c1=zMtx[zw.len,1] )

		if( 2<=zw.len ){	#	a, a ... a!
			banLst[["H1"]] <- c(zw=zw[zw.len-1] ,c1=zMtx[zw.len-1,1] )
		}

		if( 3<=zw.len ){	cur <- list( zw=zw[zw.len-2] ,c1=zMtx[zw.len-2,1] )

			#	b, a, a ... b!
			if( (cur$zw!=zw[zw.len]) && (zw[zw.len]==zw[zw.len-1]) ){
				banLst[["H2.a"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
			#	b, a, b ... a!
			if( (cur$zw!=zw[zw.len]) && (zw[zw.len]==zw[zw.len-1]) ){
				banLst[["H2.b"]] <- banLst[["H0"]]
			}
			#	a, a, a ... a!
			if( (cur$zw==zw[zw.len]) && (cur$zw==zw[zw.len-1]) ){
				banLst[["H2.c"]] <- banLst[["H0"]]
			}
			#	a, a, b ... b!
			if( (zw[zw.len-2]==zw[zw.len-1]) ){
				banLst[["H2.d"]] <- banLst[["H0"]]
			}
		}

		if( 4<=zw.len ){	cur <- list( zw=zw[zw.len-3] ,c1=zMtx[zw.len-3,1] )
			#	b, a, *, a ... b!
			if( (zw[zw.len]==zw[zw.len-2]) ){
				banLst[["H3.a"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
			#	b, *, b, * ... b!
			if( (zw[zw.len-1]==zw[zw.len-3]) ){
				banLst[["H3.b"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
			#	a, a, a, b ... b!
			if( all(zw[zw.len-1:2]==zw[zw.len-2:3]) ){
				banLst[["H3.c"]] <- banLst[["H0"]]
			}
		}

		if( 5<=zw.len ){	cur <- list( zw=zw[zw.len-4] ,c1=zMtx[zw.len-4,1] )
			#	b, a, c, c, a ... b!
			if( (zw[zw.len]==zw[zw.len-3]) && (zw[zw.len-1]==zw[zw.len-2]) ){
				banLst[["H4.a"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
			#	a, c, b, a, c ... b!
			if( (zw[zw.len-2]==zw[zw.len-4]) && (zw[zw.len-1]==zw[zw.len-3]) ){
				banLst[["H4.b"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
		}

		if( 6<=zw.len ){	cur <- list( zw=zw[zw.len-5] ,c1=zMtx[zw.len-5,1] )
			#	b, a, c, *, c, a ... b!
			if( all(zw[zw.len-c(0,1)]==zw[zw.len-c(4,3)]) ){
				banLst[["H5.a"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
			#	b, *, a, *, a, * ... b!
			if( (zw[zw.len-1]==zw[zw.len-3]) ){
				banLst[["H5.b"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
			#	a, *, a, *, b, * ... b!
			if( (zw[zw.len-3]==zw[zw.len-5]) ){
				banLst[["H5.b"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
		}

		if( 7<=zw.len ){	cur <- list( zw=zw[zw.len-6] ,c1=zMtx[zw.len-6,1] )
			#	b, a, c, d, d, c, a ... b!
			if( all(zw[zw.len-0:2]==zw[zw.len-5:3]) ){
				banLst[["H6.a"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
			#	a, c, b, *, *, a, c ... b!
			if( all(zw[zw.len-1:0]==zw[zw.len-6:5]) ){
				banLst[["H6.b"]] <- c(zw=cur$zw ,c1=cur$c1 )
			}
		}

		# -------------------------------------------------------------------------
		zwMtx <- do.call( rbind ,banLst )
		colnames(zwMtx) <- c("zw","c1")
		fndPtn <- rownames( zwMtx )

		zwBanLst <- list()
		for( zwIdx in unique(zwMtx[,"zw"]) ){
			zwFlag <- zwMtx[,"zw"]==zwIdx
			for( c1Idx in unique(zwMtx[zwFlag,"c1"]) ){
				c1Flag <- zwMtx[,"c1"]==c1Idx
				zwBanLst[[1+length(zwBanLst)]] <- c( zw=zwIdx ,c1=c1Idx 
														,zwCnt=sum(zwFlag) ,c1Cnt=sum(zwFlag&c1Flag) 
													)
			}
		}

		rObj$zwBanMtx <- do.call(rbind,zwBanLst)
	}


	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	aZoidMtx <- gEnv$allZoidMtx[c(stdIdx,sample(10:nrow(gEnv$allZoidMtx),19)) ,] ;makeInfoStr=T

		aLen <- nrow(aZoidMtx)
		cName <- c(	"rem0.num" ,"rem0.len.tot" ,"rem0.len.val"	# 이전에 값이 동일했던 컬럼이, 다음에도 동일 값이 된 경우.
					,"rem1.num" ,"rem1.len.tot" ,"rem1.len.val"
					,"c0.num" ,"c0.len.tot" ,"c0.len.val" ,"c1.num" ,"c1.len.tot" ,"c1.len.val"
					,"f0.num" ,"f0.len.tot" ,"f0.len.val" ,"f1.num" ,"f1.len.tot" ,"f1.len.val"
					,"zwNum","zwC1Num"
				)
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}
		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		if( is.null(rObj$lastSeg0) ){ # stdMIObj$zMtx 데이터가 부족한 상태
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			aRem <- aZoid %% 10
			aCStep <- aZoid[2:6] - aZoid[1:5]	;aFStep <- aZoid - rObj$lastZoid

			if( !is.null(rObj$lastSeg0) ){
				rVal <- rObj$checkSegMatch( aRem ,rObj$lastSeg0 )
				scoreMtx[aIdx,"rem0.num"] 	<- rVal["num"]
				scoreMtx[aIdx,"rem0.len.tot"] 	<- rVal["len.tot"]
				scoreMtx[aIdx,"rem0.len.val"] 	<- rVal["len.val"]
			}
			if( !is.null(rObj$lastSeg1) ){
				rVal <- rObj$checkSegMatch( aRem ,rObj$lastSeg1 )
				scoreMtx[aIdx,"rem1.num"] 	<- rVal["num"]
				scoreMtx[aIdx,"rem1.len.tot"] 	<- rVal["len.tot"]
				scoreMtx[aIdx,"rem1.len.val"] 	<- rVal["len.val"]
			}

			if( !is.null(rObj$lastSeg0.c) ){
				rVal <- rObj$checkSegMatch( aCStep ,rObj$lastSeg0.c )
				scoreMtx[aIdx,"c0.num"] 	<- rVal["num"]
				scoreMtx[aIdx,"c0.len.tot"] 	<- rVal["len.tot"]
				scoreMtx[aIdx,"c0.len.val"] 	<- rVal["len.val"]
			}
			if( !is.null(rObj$lastSeg1.c) ){
				rVal <- rObj$checkSegMatch( aCStep ,rObj$lastSeg1.c )
				scoreMtx[aIdx,"c1.num"] 	<- rVal["num"]
				scoreMtx[aIdx,"c1.len.tot"] 	<- rVal["len.tot"]
				scoreMtx[aIdx,"c1.len.val"] 	<- rVal["len.val"]
			}

			if( !is.null(rObj$lastSeg0.f) ){
				rVal <- rObj$checkSegMatch( aFStep ,rObj$lastSeg0.f )
				scoreMtx[aIdx,"f0.num"] 	<- rVal["num"]
				scoreMtx[aIdx,"f0.len.tot"] 	<- rVal["len.tot"]
				scoreMtx[aIdx,"f0.len.val"] 	<- rVal["len.val"]
			}
			if( !is.null(rObj$lastSeg1.f) ){
				rVal <- rObj$checkSegMatch( aFStep ,rObj$lastSeg1.f )
				scoreMtx[aIdx,"f1.num"] 	<- rVal["num"]
				scoreMtx[aIdx,"f1.len.tot"] 	<- rVal["len.tot"]
				scoreMtx[aIdx,"f1.len.val"] 	<- rVal["len.val"]
			}

			zw <- aZoid[6] - aZoid[1]
			flag <- rObj$zwBanMtx[,"zw"]==zw
			if( any(flag) ){
				fIdx <- which(flag)
				scoreMtx[aIdx,"zwNum"] 	<- rObj$zwBanMtx[fIdx,"zwCnt"][1]

				if( aZoid[1] %in% rObj$zwBanMtx[fIdx,"c1"] ){
					flag <- rObj$zwBanMtx[fIdx,"c1"] %in% aZoid[1]
					scoreMtx[aIdx,"zwC1Num"] 	<- rObj$zwBanMtx[fIdx[flag],"c1Cnt"]
				}
			}

			# if( makeInfoStr ){ }
		}

		# for( idx in 1:4 ){	# c3.x
		# 	logId <- sprintf("c3%d",idx)
		# 	scoreMtx[,logId] <- rObj$cInfo$mat3Lst[[logId]]$match( aZoidMtx[,0:2+idx,drop=F] )
		# }

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}

	return( rObj )

} # bFMtx.score1( )


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
					# dbgStr <- sprintf("col:%d(val:%d)  found in row:%d col:%s",cIdx,lc,fRowIdx,paste(fColIdx,collapse=","))
					break
				}
			}

			dbgStr <- ""
			for( fcIdx in fColIdx ){
				olSpan <- fCutU.overlapSpan( colLen ,colIdx.pre=fcIdx ,colIdx.post=cIdx )
				if( 1>sum(olSpan$info[c("lMargin","rMargin")]) )	next

				# valInc <- mtx[fRowIdx+1,olSpan$span.pre]-mtx[fRowIdx,olSpan$span.pre] <- bug?
				valInc <- mtx[rowLen,olSpan$span.post]-mtx[fRowIdx,olSpan$span.pre]
				banVal <- mtx[rowLen,olSpan$span.post]+valInc
				fixPoint <- banVal	;fixPoint[-(olSpan$info["lMargin"]+1)] <- NA
				dbgStr <- sprintf("colIdx:%d(val:%d) from (%d,%d)  %s/%s --> %s/%s..?",cIdx,lc,fRowIdx,fcIdx
									,paste(mtx[fRowIdx  ,olSpan$span.pre],collapse=",")
									,paste(mtx[rowLen	,olSpan$span.post],collapse=",")
									,paste(mtx[rowLen	,olSpan$span.post],collapse=",")
									,paste(banVal,collapse=",")
								)
				banObj <- list( banVal=banVal ,banSpan=olSpan$span.post ,fixPoint=fixPoint ,dbgStr=dbgStr )

				fixIdx <- which( !is.na(fixPoint) )
				banObj$info <- c( length(banVal) ,fixIdx )
				names(banObj$info) <- c("len","fixIdx")

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
					,seqNextPtn.raw=getSeqPtn( stdMI$rawTail )
					,seqNextPtn.cStep=getSeqPtn( stdMI$cStepTail )
					,seqNextPtn.fStep=getSeqPtn( stdMI$fStepTail )
				)

	#	cName <- c("rebPtn.1","rebPtn.n","snR3"  ,"snMax.r","snFCnt.r"  ,"snMax.c","snFCnt.c"  ,"snMax.f","snFCnt.f")
	#				snMax.r ,snFCnt.r 은 기준 값이 동일 칼럼에 반복될 때의 좌우 증감량이 유지되는 지 여부
	#				snR3는 기준값이 다른 컬럼이지만 좌우 증감량이 2개 이상인 경우.( Flag )
	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		aLen <- nrow(aZoidMtx)
		cName <- c("rebPtn.1","rebPtn.n","snR3" ,"snMax.r","snFCnt.r"  ,"snMax.c","snFCnt.c"  ,"snMax.f","snFCnt.f")
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c("rebPtn.1","rebPtn.n","snXXX.r","snXXX.c","snXXX.f","zMtx.size")
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

			#	"snR3"	- 재발값을 기준으로 증감이 동일하게 반복되는 것이 3개 이상.
			banLst <- rObj$seqNextPtn.raw$banLst
			for( idx in seq_len(length(banLst)) ){
				if( 3>banLst[[idx]]$info["len"] ) next

				flag <- fCutU.hasPtn( banLst[[idx]]$banVal, aZoid ,thld=3 ,fixIdx=banLst[[idx]]$info["fixIdx"] )
				if( flag ){
					scoreMtx[aIdx,"snR3"] <- TRUE
					break
				}
			}
			#	fCutU.hasPtn( src, aZoid ,thld=3 ,fixIdx=c(2,4) )
			#	scoreMtx[aIdx,"snR3"] <- 

			#	"sncMax.raw" ,"sncFCnt.raw" 
			snMatCnt.raw <- rObj$seqNextPtn.raw$filt( aZoid )$matCnt
			scoreMtx[aIdx,"snMax.r"] <- max( snMatCnt.raw )
			if( 1==scoreMtx[aIdx,"snMax.r"] ) scoreMtx[aIdx,"snMax.r"] <- 0
			scoreMtx[aIdx,"snFCnt.r"] <- sum( snMatCnt.raw>=2 )

			#	"sncMax.cStep" ,"sncFCnt.cStep"
			snMatCnt.cStep <- rObj$seqNextPtn.cStep$filt( aCStep )$matCnt
			scoreMtx[aIdx,"snMax.c"] <- max( snMatCnt.cStep )
			if( 1==scoreMtx[aIdx,"snMax.c"] ) scoreMtx[aIdx,"snMax.c"] <- 0
			scoreMtx[aIdx,"snFCnt.c"] <- sum( snMatCnt.cStep>=2 )

			#	"sncMax.fStep" ,"sncFCnt.fStep"
			snMatCnt.fStep <- rObj$seqNextPtn.fStep$filt( aFStep )$matCnt
			scoreMtx[aIdx,"snMax.f"] <- max( snMatCnt.fStep )
			if( 1==scoreMtx[aIdx,"snMax.f"] ) scoreMtx[aIdx,"snMax.f"] <- 0
			scoreMtx[aIdx,"snFCnt.f"] <- sum( snMatCnt.fStep>=2 )

			if( makeInfoStr ){
				infoMtx[aIdx,"rebPtn.1"] <- if( nrow(rObj$rebPtn.1$matInfo) >0 ) infoStr.rebPtn.1 else "N/A"
				infoMtx[aIdx,"rebPtn.n"] <- if( length(rObj$rebPtn.n) >0 ) infoStr.rebPtn.n else "N/A"
				infoMtx[aIdx,"snXXX.r"] <- if( 0==sum(snMatCnt.raw) ) "N/A" else sprintf("matCnt:%s",paste(snMatCnt.raw,collapse=" "))
				infoMtx[aIdx,"snXXX.c"] <- if( 0==sum(snMatCnt.cStep) ) "N/A" else sprintf("matCnt:%s",paste(snMatCnt.cStep,collapse=" "))
				infoMtx[aIdx,"snXXX.f"] <- if( 0==sum(snMatCnt.fStep) ) "N/A" else sprintf("matCnt:%s",paste(snMatCnt.fStep,collapse=" "))
			}

		}
		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}
	return( rObj )

} # bFMtx.score3( )

#	fCutU.getFiltObjPair( stdMI$rawTail )
bFMtx.score4 <- function( stdMIObj ){
	#	stdMIObj <- stdMI.grp$basic$basic
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="score4"	,zMtx.size=nrow(zMtx)	,lastZoid=stdMI$lastZoid )

	rObj$fInfo <- NULL
	if( 1<nrow(stdMI$rawTail) ){
		rObj$fInfo <- fCutU.getFiltObjPair( stdMI$rawTail )	# rObj$fInfo$explain( )
	}

	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	aZoidMtx <- gEnv$allZoidMtx[c(stdIdx,sample(10:nrow(gEnv$allZoidMtx),19)) ,] ;makeInfoStr=T

		aLen <- nrow(aZoidMtx)
		cName <- c(	"pBanN.r","pBanN.n"				# found num of rebound ptn ( ptn itself, next ptn in right column )
					,"pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum"
					,"iBanN"							# found num of inc.ptn
					,"iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum"	#  rebLastCol     extMat3     extMat4 multiHpn.TF    foundNum 
					,"FVa.m","FVa.c"	# FVa.max FVa.hpnCnt
					# ,"aFV.m","aFV.c"	# aFV.max aFV.hpnCnt 사실상 중복되는 경우가 많아 폐지.
					,"m4"								# match4
				)
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pBanLst","iBanLst", "pairHpn", "match4" ,"zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}
		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		if( is.null(rObj$fInfo) ){ # stdMIObj$zMtx 데이터가 부족한 상태
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			# aCStep <- aZoid[2:6] - aZoid[1:5]	;aFStep <- aZoid - rObj$lastZoid	;aRem <- aZoid %% 10

			rstObj <- bFMtx.util.fMtxObj.score4567( aZoid ,rObj$fInfo ,makeInfoStr )

			# pairBanLst
			pBan.cutInfo <- rstObj$F_pBanLst( makeInfoStr )
			scoreMtx[aIdx ,"pBanN.r"] <- pBan.cutInfo$cutHpn["rebPtn"]
			scoreMtx[aIdx ,"pBanN.n"] <- pBan.cutInfo$cutHpn["nextPtn"]
			workCol <- c("pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum")
			scoreMtx[aIdx ,workCol] <- pBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# iBanLst
			iBan.cutInfo <- rstObj$F_iBanLst( makeInfoStr )
			scoreMtx[aIdx ,"iBanN"] <- length(rstObj$iBanLst)
			workCol <- c("iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum")
			scoreMtx[aIdx ,workCol] <- iBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# pairHpn
			if( 0<length(rstObj$pairHpn) ){
				workCol <- c("FVa.m","FVa.c")	#	,"aFV.m","aFV.c" 폐지
				scoreMtx[aIdx ,workCol ] <- rstObj$pairHpn$foundInfo[c("FVa.max","FVa.hpnCnt")]	#	,"aFV.max","aFV.hpnCnt" 폐지
				
				#	"FVa.m","aFV.m" 에서 1은 너무 흔한 듯.
				if( 1==scoreMtx[aIdx ,"FVa.m" ] )	scoreMtx[aIdx ,"FVa.m" ] <- 0
				# if( 1==scoreMtx[aIdx ,"aFV.m" ] )	scoreMtx[aIdx ,"aFV.m" ] <- 0
			}

			# match4
			if( 0<length(rstObj$match4) ){
				scoreMtx[aIdx, "m4"] <- rstObj$match4$foundInfo["matCnt"]
			}

			if( makeInfoStr ){

				infoMtx[aIdx,"pBanLst"] <- pBan.cutInfo$infoStr
				infoMtx[aIdx,"iBanLst"] <- iBan.cutInfo$infoStr
				
				if( 0<length(rstObj$pairHpn) )	infoMtx[aIdx,"pairHpn"] <- rstObj$pairHpn$infoStr

				if( 0<scoreMtx[aIdx, "m4"] )	infoMtx[aIdx,"match4"] <- rstObj$match4$infoStr

			}

		}

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}

	return( rObj )

} # bFMtx.score4( )

#	fCutU.getFiltObjPair( stdMI$rawTail %% 10 )
bFMtx.score5 <- function( stdMIObj ){
	#	stdMIObj <- stdMI.grp$basic$basic
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="score5"	,zMtx.size=nrow(zMtx)	,lastZoid=stdMI$lastZoid )

	rObj$fInfo <- NULL
	if( 1<nrow(stdMI$rawTail) ){
		rObj$fInfo <- fCutU.getFiltObjPair( stdMI$rawTail %% 10 )	# rObj$fInfo$explain( )
	}

	#	cName <- c("rebPtn.1")
	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	aZoidMtx <- gEnv$allZoidMtx[c(stdIdx,sample(10:nrow(gEnv$allZoidMtx),19)) ,] ;makeInfoStr=T

		aLen <- nrow(aZoidMtx)
		cName <- c(	"pBanN.r","pBanN.n"				# found num of rebound ptn ( ptn itself, next ptn in right column )
					,"pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum"
					,"iBanN"							# found num of inc.ptn
					,"iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum"	#  rebLastCol     extMat3     extMat4 multiHpn.TF    foundNum 
					,"FVa.m","FVa.c"
					# ,"aFV.m","aFV.c"	# FVa.max FVa.hpnCnt  aFV.max aFV.hpnCnt 
					,"m4"								# match4
				)
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pBanLst","iBanLst", "pairHpn", "match4" ,"zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}
		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		if( is.null(rObj$fInfo) ){ # stdMIObj$zMtx 데이터가 부족한 상태
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			# aCStep <- aZoid[2:6] - aZoid[1:5]	;aFStep <- aZoid - rObj$lastZoid	;aRem <- aZoid %% 10

			rstObj <- bFMtx.util.fMtxObj.score4567( aZoid%%10 ,rObj$fInfo ,makeInfoStr )

			# pairBanLst
			pBan.cutInfo <- rstObj$F_pBanLst( makeInfoStr )
			scoreMtx[aIdx ,"pBanN.r"] <- pBan.cutInfo$cutHpn["rebPtn"]
			scoreMtx[aIdx ,"pBanN.n"] <- pBan.cutInfo$cutHpn["nextPtn"]
			workCol <- c("pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum")
			scoreMtx[aIdx ,workCol] <- pBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# iBanLst
			iBan.cutInfo <- rstObj$F_iBanLst( makeInfoStr )
			scoreMtx[aIdx ,"iBanN"] <- length(rstObj$iBanLst)
			workCol <- c("iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum")
			scoreMtx[aIdx ,workCol] <- iBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# pairHpn
			if( 0<length(rstObj$pairHpn) ){
				workCol <- c("FVa.m","FVa.c")	#	,"aFV.m","aFV.c" 폐지
				scoreMtx[aIdx ,workCol ] <- rstObj$pairHpn$foundInfo[c("FVa.max","FVa.hpnCnt")]	# ,"aFV.max","aFV.hpnCnt" 폐지

				#	"FVa.m","aFV.m" 에서 1은 너무 흔한 듯.
				if( 1==scoreMtx[aIdx ,"FVa.m" ] )	scoreMtx[aIdx ,"FVa.m" ] <- 0
				# if( 1==scoreMtx[aIdx ,"aFV.m" ] )	scoreMtx[aIdx ,"aFV.m" ] <- 0
			}

			# match4
			if( 0<length(rstObj$match4) ){
				scoreMtx[aIdx, "m4"] <- rstObj$match4$foundInfo["matCnt"]
			}

			if( makeInfoStr ){

				infoMtx[aIdx,"pBanLst"] <- pBan.cutInfo$infoStr
				infoMtx[aIdx,"iBanLst"] <- iBan.cutInfo$infoStr
				
				if( 0<length(rstObj$pairHpn) )	infoMtx[aIdx,"pairHpn"] <- rstObj$pairHpn$infoStr

				if( 0<scoreMtx[aIdx, "m4"] )	infoMtx[aIdx,"match4"] <- rstObj$match4$infoStr

			}

		}

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}

	return( rObj )

} # bFMtx.score5( )

#	fCutU.getFiltObjPair( stdMI$cStepTail )
bFMtx.score6 <- function( stdMIObj ){
	#	stdMIObj <- stdMI.grp$basic$basic
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="score6"	,zMtx.size=nrow(zMtx)	,lastZoid=stdMI$lastZoid )

	rObj$fInfo <- NULL
	if( 1<nrow(stdMI$rawTail) ){
		rObj$fInfo <- fCutU.getFiltObjPair( stdMI$cStepTail )	# rObj$fInfo$explain( )
	}

	#	cName <- c("rebPtn.1")
	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	aZoidMtx <- gEnv$allZoidMtx[c(stdIdx,sample(10:nrow(gEnv$allZoidMtx),19)) ,] ;makeInfoStr=T

		aLen <- nrow(aZoidMtx)
		cName <- c(	"pBanN.r","pBanN.n"				# found num of rebound ptn ( ptn itself, next ptn in right column )
					,"pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum"
					,"iBanN"							# found num of inc.ptn
					,"iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum"	#  rebLastCol     extMat3     extMat4 multiHpn.TF    foundNum 
					,"FVa.m","FVa.c"
					# ,"aFV.m","aFV.c"	# FVa.max FVa.hpnCnt  aFV.max aFV.hpnCnt 
					,"m4"								# match4
				)
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pBanLst","iBanLst", "pairHpn", "match4" ,"zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}
		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		if( is.null(rObj$fInfo) ){ # stdMIObj$zMtx 데이터가 부족한 상태
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			# aCStep <- aZoid[2:6] - aZoid[1:5]	;aFStep <- aZoid - rObj$lastZoid	;aRem <- aZoid %% 10

			rstObj <- bFMtx.util.fMtxObj.score4567( aZoid[2:6] - aZoid[1:5] ,rObj$fInfo ,makeInfoStr )

			# pairBanLst
			pBan.cutInfo <- rstObj$F_pBanLst( makeInfoStr )
			scoreMtx[aIdx ,"pBanN.r"] <- pBan.cutInfo$cutHpn["rebPtn"]
			if( 0<scoreMtx[aIdx,"pBanN.r"] ){	# 1 개 발생은 흔한 듯.
				scoreMtx[aIdx,"pBanN.r"] <- scoreMtx[aIdx,"pBanN.r"] - 1
			}
			scoreMtx[aIdx ,"pBanN.n"] <- pBan.cutInfo$cutHpn["nextPtn"]
			workCol <- c("pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum")
			scoreMtx[aIdx ,workCol] <- pBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# iBanLst
			iBan.cutInfo <- rstObj$F_iBanLst( makeInfoStr )
			scoreMtx[aIdx,"iBanN"] <- length(rstObj$iBanLst)
			if( 0<scoreMtx[aIdx,"iBanN"] ){	# 1 개 발생은 흔한 듯.
				scoreMtx[aIdx,"iBanN"] <- scoreMtx[aIdx,"iBanN"] - 1
			}
			workCol <- c("iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum")
			scoreMtx[aIdx ,workCol] <- iBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# pairHpn
			if( 0<length(rstObj$pairHpn) ){
				workCol <- c("FVa.m","FVa.c")	# ,"aFV.m","aFV.c"
				scoreMtx[aIdx ,workCol ] <- rstObj$pairHpn$foundInfo[c("FVa.max","FVa.hpnCnt")]	# ,"aFV.max","aFV.hpnCnt"

				#	"FVa.m","aFV.m" 에서 1은 너무 흔한 듯.
				if( 1==scoreMtx[aIdx ,"FVa.m" ] )	scoreMtx[aIdx ,"FVa.m" ] <- 0
				# if( 1==scoreMtx[aIdx ,"aFV.m" ] )	scoreMtx[aIdx ,"aFV.m" ] <- 0
			}

			# match4
			if( 0<length(rstObj$match4) ){
				scoreMtx[aIdx, "m4"] <- rstObj$match4$foundInfo["matCnt"]
			}

			if( makeInfoStr ){

				infoMtx[aIdx,"pBanLst"] <- pBan.cutInfo$infoStr
				infoMtx[aIdx,"iBanLst"] <- iBan.cutInfo$infoStr
				
				if( 0<length(rstObj$pairHpn) )	infoMtx[aIdx,"pairHpn"] <- rstObj$pairHpn$infoStr

				if( 0<scoreMtx[aIdx, "m4"] )	infoMtx[aIdx,"match4"] <- rstObj$match4$infoStr

			}

		}

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}

	return( rObj )

} # bFMtx.score5( )

#	fCutU.getFiltObjPair( stdMI$fStepTail )
bFMtx.score7 <- function( stdMIObj ){
	#	stdMIObj <- stdMI.grp$basic$basic
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="score7"	,zMtx.size=nrow(zMtx)	,lastZoid=stdMI$lastZoid )

	rObj$fInfo <- NULL
	if( 2<nrow(stdMI$rawTail) ){
		rObj$fInfo <- fCutU.getFiltObjPair( stdMI$fStepTail )	# rObj$fInfo$explain( )
	}

	#	cName <- c("rebPtn.1")
	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	aZoidMtx <- gEnv$allZoidMtx[c(stdIdx,sample(10:nrow(gEnv$allZoidMtx),19)) ,] ;makeInfoStr=T

		aLen <- nrow(aZoidMtx)
		cName <- c(	"pBanN.r","pBanN.n"				# found num of rebound ptn ( ptn itself, next ptn in right column )
					,"pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum"
					,"iBanN"							# found num of inc.ptn
					,"iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum"	#  rebLastCol     extMat3     extMat4 multiHpn.TF    foundNum 
					,"FVa.m","FVa.c"
					# ,"aFV.m","aFV.c"	# FVa.max FVa.hpnCnt  aFV.max aFV.hpnCnt 
					,"m4"								# match4
				)
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "pBanLst","iBanLst", "pairHpn", "match4" ,"zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}
		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		if( is.null(rObj$fInfo) ){ # stdMIObj$zMtx 데이터가 부족한 상태
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			# aCStep <- aZoid[2:6] - aZoid[1:5]	;aFStep <- aZoid - rObj$lastZoid	;aRem <- aZoid %% 10

			rstObj <- bFMtx.util.fMtxObj.score4567( aZoid - rObj$lastZoid ,rObj$fInfo ,makeInfoStr )

			# pairBanLst
			pBan.cutInfo <- rstObj$F_pBanLst( makeInfoStr )
			scoreMtx[aIdx ,"pBanN.r"] <- pBan.cutInfo$cutHpn["rebPtn"]
			if( 0<scoreMtx[aIdx,"pBanN.r"] ){	# 1 개 발생은 흔한 듯.
				scoreMtx[aIdx,"pBanN.r"] <- scoreMtx[aIdx,"pBanN.r"] - 1
			}
			scoreMtx[aIdx ,"pBanN.n"] <- pBan.cutInfo$cutHpn["nextPtn"]
			workCol <- c("pLCol" ,"pE3" ,"pE4"	,"pMH" ,"pfNum")
			scoreMtx[aIdx ,workCol] <- pBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# iBanLst
			iBan.cutInfo <- rstObj$F_iBanLst( makeInfoStr )
			scoreMtx[aIdx ,"iBanN"] <- length(rstObj$iBanLst)
			if( 0<scoreMtx[aIdx,"iBanN"] ){	# 1 개 발생은 흔한 듯.
				scoreMtx[aIdx,"iBanN"] <- scoreMtx[aIdx,"iBanN"] - 1
			}
			workCol <- c("iLCol" ,"iE3" ,"iE4"	,"iMH" ,"ifNum")
			scoreMtx[aIdx ,workCol] <- iBan.cutInfo$cutHpn[c("rebLastCol","extMat3","extMat4","multiHpn","foundNum")]

			# pairHpn
			if( 0<length(rstObj$pairHpn) ){
				workCol <- c("FVa.m","FVa.c")	# ,"aFV.m","aFV.c"
				scoreMtx[aIdx ,workCol ] <- rstObj$pairHpn$foundInfo[c("FVa.max","FVa.hpnCnt")]	# ,"aFV.max","aFV.hpnCnt"

				#	"FVa.m","aFV.m" 에서 1은 너무 흔한 듯.
				if( 1==scoreMtx[aIdx ,"FVa.m" ] )	scoreMtx[aIdx ,"FVa.m" ] <- 0
				# if( 1==scoreMtx[aIdx ,"aFV.m" ] )	scoreMtx[aIdx ,"aFV.m" ] <- 0
			}

			# match4
			if( 0<length(rstObj$match4) ){
				scoreMtx[aIdx, "m4"] <- rstObj$match4$foundInfo["matCnt"]
			}

			if( makeInfoStr ){

				infoMtx[aIdx,"pBanLst"] <- pBan.cutInfo$infoStr
				infoMtx[aIdx,"iBanLst"] <- iBan.cutInfo$infoStr
				
				if( 0<length(rstObj$pairHpn) )	infoMtx[aIdx,"pairHpn"] <- rstObj$pairHpn$infoStr

				if( 0<scoreMtx[aIdx, "m4"] )	infoMtx[aIdx,"match4"] <- rstObj$match4$infoStr

			}

		}

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}

	return( rObj )

} # bFMtx.score5( )


#	score4,score5,score6,score7 의 공통 fMtxObj( )
bFMtx.util.fMtxObj.score4567 <- function( aCode ,fInfo ,makeInfoStr=F ){
	#	fInfo <- rObj$fInfo ;makeInfoStr<-T	;aCode <- aZoid
	rstObj <- list()

	# pairBanLst ==========================================
	pBanLst <- list()
	if( 0<length(fInfo$pBanInfoLst) ){
		for( lIdx in 1:length(fInfo$pBanInfoLst) ){
			pBI <- fInfo$pBanInfoLst[[lIdx]]	# banInfo
			pI	<- pBI$pairInfo

			# typ="rebPair" --------------------------------------
			rebPair <- NULL
			foundIdxMtx <- fInfo$findPtn( aCode ,pI[c("v1","v2")] )
			if( 0<nrow(foundIdxMtx) ){
				foundInfo <- c( rebLastCol=0 ,extMatNum=0
								,hpnCnt=pI["hpn"] ,foundNum=nrow(foundIdxMtx) 
				)
				rebLastCol.F <- apply( foundIdxMtx ,1 ,function(fIdx){all(fIdx==pI[c("cf1","cf2")])} )
				foundInfo["rebLastCol"] <- any( rebLastCol.F )

				flag <- fCutU.hasPtn(pBI$incPtn.banVal,aCode,thld=3,fixIdx=pBI$incPtn.fixIdx)
				if( flag )	foundInfo["extMatNum"] <- 1	# 초과 매치 수 3 - 2

				flag <- fCutU.hasPtn(pBI$incPtn.banVal,aCode,thld=4,fixIdx=pBI$incPtn.fixIdx)
				if( flag )	foundInfo["extMatNum"] <- 2	# 초과 매치 수 4 - 2

				rebPair <- list( foundInfo=foundInfo )
				if( makeInfoStr ){
					infoStr <- character(0)
					if( any(rebLastCol.F) ){
						str <- apply( foundIdxMtx[rebLastCol.F,,drop=F] ,1 ,function(cord){sprintf("(%d,%d)",cord[1],cord[2])} )
						infoStr <- c(infoStr ,rebLastCol=paste(str,collapse="") )
					}

					infoStr <- c( pairVal=sprintf("%d,%d",pI["v1"],pI["v2"]) ,infoStr )

					rebPair$infoStr <- paste( paste(names(infoStr),infoStr,sep=":") ,collapse="  " )
				}
			}

			# typ="pairNextPtn" ----------------------------------
			pairNextPtn <- NULL
			if( all(pBI$rebPtn.banVal[pBI$rebPtn.fixIdx]==aCode[pBI$rebPtn.fixIdx]) ){
				foundInfo <- c( extMatNum = 0 )

				if( 2<length(pBI$rebPtn.banCol) ){
					banCol <- setdiff( pBI$rebPtn.banCol ,pBI$rebPtn.fixIdx )
					foundInfo["extMatNum"] <- sum( pBI$rebPtn.banVal[banCol]==aCode[banCol] ) - 2
				}

				pairNextPtn <- list( foundInfo=foundInfo )
				if( makeInfoStr ){
					infoStr <- character(0)

					nextPtn.val <- pBI$rebPtn.banVal[pBI$rebPtn.fixIdx]
					infoStr <- c( nextPtn=sprintf("%d,%d(from %d,%d)",nextPtn.val[1],nextPtn.val[2],pI["v1"],pI["v2"]) 
								,infoStr )

					pairNextPtn$infoStr <- paste( paste(names(infoStr),infoStr,sep=":") ,collapse="  " )
				}
			}

			if( !is.null(rebPair) || !is.null(pairNextPtn) ){
				lId <- sprintf("pBan%d",lIdx)
				pBanLst[[lId]] <- list( rebPair=rebPair ,pairNextPtn=pairNextPtn )
			}
		}
	}
	rstObj$pBanLst <- pBanLst

	# iBanLst =============================================
	iBanLst <- list()
	if( 0<length(fInfo$iBanInfoLst) ){
		for( lIdx in 1:length(fInfo$iBanInfoLst) ){
			iBanInfo <- fInfo$iBanInfoLst[[lIdx]]
			infoDf <- iBanInfo$incInfoDf
			foundIdxMtx <- fInfo$findPtn( aCode ,infoDf[1,c("v1","v2")] )
			if( 0==nrow(foundIdxMtx) ) next

			foundInfo <- c( rebLastCol=0 ,extMatNum=0
							,multiHpn.TF=as.integer(iBanInfo$multiHpn)	,foundNum=nrow(foundIdxMtx)
						)
			
			rebLastCol <- apply( foundIdxMtx ,1 ,function(fIdx){all(fIdx==infoDf[c("cIdx1.f","cIdx2.f")])} )
			foundInfo["rebLastCol"] <- any(rebLastCol)

			if( 2<length(iBanInfo$incPtn.banVal) ){
				flag=fCutU.hasPtn(iBanInfo$incPtn.banVal,aCode,thld=3,fixIdx=iBanInfo$incPtn.fixIdx)
				if( flag )	foundInfo["extMatNum"] <- 1	# 초과매치 수 (3-2)

				flag=fCutU.hasPtn(iBanInfo$incPtn.banVal,aCode,thld=4,fixIdx=iBanInfo$incPtn.fixIdx)
				if( flag )	foundInfo["extMatNum"] <- 2	# 초과매치 수 (4-2)
			}

			iBan <- list( foundInfo=foundInfo )
			if( makeInfoStr ){
				infoStr <- character(0)

				infoStr <- c( valStr=as.character(infoDf[1,"valStr"]) 
							,infoStr )
				iBan$infoStr <- paste( paste(names(infoStr),infoStr,sep=":") ,collapse="  " )
			}

			iBanLst[[sprintf("iBan%d",lIdx)]] <- iBan
		}
	}
	rstObj$iBanLst <- iBanLst

	# pairHpn( pairPtnLst )=========================================
	pairHpn <- list()
	if( 0<length(fInfo$pairPtnLst) ){
		matCntLst <- lapply( fInfo$pairPtnLst ,function( ptnLst ){
			fCnt <- rep( 0, length(ptnLst) )
			for( lIdx in seq_len(length(ptnLst)) ){
				mtx <- ptnLst[[lIdx]]
				for( rIdx in seq_len(nrow(mtx)) ){
					if( fCutU.hasPtn( mtx[rIdx,], aCode ) ) fCnt[lIdx] <- fCnt[lIdx] + 1
				}
			}
			return( fCnt )
		})		# match count List : Gen 별로 매치가 몇 개 발견되었는지?
		names(matCntLst) <- c("FVa","aFV")
		foundInfo <- c( FVa.max=0 ,FVa.hpnCnt=0 ,aFV.max=0 ,aFV.hpnCnt=0 )
		if( 0<length(matCntLst[["FVa"]]) ){
			foundInfo["FVa.max"] <- max(matCntLst[["FVa"]])	# max가 1개인 것은 별 의미 없을 듯.
			foundInfo["FVa.hpnCnt"] <- sum(matCntLst[["FVa"]]>1)
		}
		if( 0<length(matCntLst[["aFV"]]) ){
			foundInfo["aFV.max"] <- max(matCntLst[["aFV"]])	# max가 1개인 것은 별 의미 없을 듯.
			foundInfo["aFV.hpnCnt"] <- sum(matCntLst[["aFV"]]>1)			
		}
		pairHpn <- list( foundInfo=foundInfo )
		if( makeInfoStr ){
			infoStr <- character(0)
			if( any(foundInfo[c("FVa.max","FVa.hpnCnt")]>=c(2,2)) ){
				infoStr <- c( infoStr
							,"(pFV,*)"= paste( matCntLst[["FVa"]] ,collapse=" " ) 
				)
			}
			if( any(foundInfo[c("aFV.max","aFV.hpnCnt")]>=c(2,2)) ){
				infoStr <- c( infoStr
							,"(*,pFV)"= paste( matCntLst[["aFV"]] ,collapse=" " ) 
				)
			}
			pairHpn$infoStr <- paste( paste(names(infoStr),infoStr,sep=":") ,collapse="  " )
		}
	}
	rstObj$pairHpn <- pairHpn

	# match4 (ptn4Str) =============================================
	match4 <- list()
	if( 0 < length(fInfo$ptn4Lst) ){
		flag <- rep( F ,length(fInfo$ptn4Lst) )
		for( lIdx in seq_len(length(fInfo$ptn4Lst)) ){
			ptn4 <- fInfo$ptn4Lst[[lIdx]]
			if( !all(ptn4$cVal==aCode[ptn4$cIdx]) ) next

			if( all(ptn4$cSpanVal==aCode[ptn4$cSpan]) ){
				flag[lIdx] <- TRUE
			}
		}
		foundInfo <- c( matCnt=sum(flag) )
		match4 <- list( foundInfo=foundInfo )
		if( makeInfoStr ){
			infoStr <- character(0)
			if( any(flag) ){
				matInfo <- sapply( fInfo$ptn4Lst[flag] ,function(p){p$infoStr} )
				infoStr <- c( infoStr ,matPtn=paste(matInfo,collapse="   ") )
			}
			match4$infoStr <- paste( paste(names(infoStr),infoStr,sep=":") ,collapse="  " )
		}
	}
	rstObj$match4 <- match4


	# --------------------------------------------------------------
	rstObj$F_pBanLst <- function( makeInfoStr=F ){
		infoStr <- character(0)
		cutHpn <- c( rebPtn=0 ,nextPtn=0 ,rebLastCol=0 ,extMat3=0 ,extMat4=0 ,multiHpn=0 ,foundNum=0 )
		for( lIdx in seq_len(length(rstObj$pBanLst)) ){
			pBan <- rstObj$pBanLst[[lIdx]]
			if( !is.null(pBan$rebPair) ){
				cutHpn["rebPtn"] <- 1 + cutHpn["rebPtn"]

				foundInfo <- pBan$rebPair$foundInfo
				cutHpn["rebLastCol"] <- cutHpn["rebLastCol"] + foundInfo["rebLastCol"]
				cutHpn["multiHpn"] <- cutHpn["multiHpn"] + ifelse( 2<foundInfo["hpnCnt.hpn"] ,1,0 )
				if( 1 < foundInfo["foundNum"] ){
						cutHpn["foundNum"] <- cutHpn["foundNum"] + foundInfo["foundNum"]
				}

				if( 1==foundInfo["extMatNum"] ) cutHpn["extMat3"] <- 1 + cutHpn["extMat3"]
				if( 2==foundInfo["extMatNum"] ) cutHpn["extMat4"] <- 1 + cutHpn["extMat4"]

			}
			if( !is.null(pBan$pairNextPtn) ){
				cutHpn["nextPtn"] <- 1 + cutHpn["nextPtn"]

				foundInfo <- pBan$pairNextPtn$foundInfo
				
				if( 1==foundInfo["extMatNum"] ) cutHpn["extMat3"] <- 1 + cutHpn["extMat3"]
				if( 2==foundInfo["extMatNum"] ) cutHpn["extMat4"] <- 1 + cutHpn["extMat4"]

			}

			#	if( makeInfoStr ) infoStr 작업은 나중에.. (지금은 뭘 적어야 할 지도 모르겠다.)
		}
		return( list(cutHpn=cutHpn,infoStr=ifelse(0<length(infoStr),infoStr,"" ) ) )
	} # rstObj$F_pBanLst( )

	rstObj$F_iBanLst <- function( makeInfoStr=F ){
		infoStr <- character(0)
		cutHpn <- c(rebLastCol=0 ,extMat3=0 ,extMat4=0 ,multiHpn=0 ,foundNum=0 )
		iBanLst <- rstObj$iBanLst
		for( lIdx in seq_len(length(iBanLst)) ){
			cutHpn["rebLastCol"] <- cutHpn["rebLastCol"] + iBanLst[[lIdx]]$foundInfo["rebLastCol"]
			cutHpn["multiHpn"] <- cutHpn["multiHpn"] + iBanLst[[lIdx]]$foundInfo["multiHpn.TF"]
			
			if( 1== iBanLst[[lIdx]]$foundInfo["extMatNum"] ){
				cutHpn["extMat3"] <- 1 + cutHpn["extMat3"]
			}
			if( 2== iBanLst[[lIdx]]$foundInfo["extMatNum"] ){
				cutHpn["extMat4"] <- 1 + cutHpn["extMat4"]
			}

			if( 1 < iBanLst[[lIdx]]$foundInfo["foundNum"] ){
					cutHpn["foundNum"] <- cutHpn["foundNum"] + iBanLst[[lIdx]]$foundInfo["foundNum"]
			}
			#	if( makeInfoStr ) infoStr 작업은 나중에.. (지금은 뭘 적어야 할 지도 모르겠다.)
		}
		return( list(cutHpn=cutHpn,infoStr=ifelse(0<length(infoStr),infoStr,"" ) ) )
	} # rstObj$F_iBanLst( )

	return( rstObj )
} # bFMtx.util.fMtxObj.score4567( )



#	fCutU.getFiltObjPair( stdMI$fStepTail )
bFMtx.score8 <- function( stdMIObj ){
	#	stdMIObj <- stdMI.grp$basic$basic
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="score8"	,zMtx.size=nrow(zMtx)
					,lastZoid=stdMI$lastZoid
				)

	rObj$cInfo <- NULL	# cStep Info
	if( 0<nrow(stdMI$rawTail) ){
		cInfo <- list()

		cStep.srt <- sort(unique(stdMI$cStep))
		cLen <- length(cStep.srt)
		if( 3<=cLen ){
			cInfo$max3 <- cStep.srt[cLen-0:2]	;cInfo$min3 <- cStep.srt[1:3]
		}
		if( 2<=cLen ){
			cInfo$max2 <- cStep.srt[cLen-0:1]	;cInfo$min2 <- cStep.srt[1:2]
		}

		#	c5.x ,c4.x 폐지. 시간이 너무 오래걸리는 듯.
		# cInfo$mat5Lst <- list()
		# for( idx in 1:2 ){	# c5.x
		# 	logId <- sprintf("c5%d",idx)
		# 	cInfo$mat5Lst[[logId]] <- fCutU.getChkCStepValReb( zMtx[,0:4+idx,drop=F] )
		# }
		# cInfo$mat4Lst <- list()
		# for( idx in 1:3 ){	# c4.x
		# 	logId <- sprintf("c4%d",idx)
		# 	cInfo$mat4Lst[[logId]] <- fCutU.getChkCStepValReb( zMtx[,0:3+idx,drop=F] )
		# }
		cInfo$mat3Lst <- list()
		for( idx in 1:4 ){	# c3.x
			logId <- sprintf("c3%d",idx)
			cInfo$mat3Lst[[logId]] <- fCutU.getChkCStepValReb( zMtx[,0:2+idx,drop=F] )
		}
		cInfo$mat2Lst <- list()
		for( idx in 1:5 ){	# c2.x
			logId <- sprintf("c2%d",idx)
			cInfo$mat2Lst[[logId]] <- fCutU.getChkCStepValReb( zMtx[,0:1+idx,drop=F] )
		}

		cInfo$cTbl <- table(stdMI$cStep)

		rObj$cInfo <- cInfo

		rObj$fTbl <- NULL
		if( 1<nrow(stdMI$rawTail) ){
			rObj$fTbl <- table(stdMI$fStep)
		}
	}

	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	aZoidMtx <- gEnv$allZoidMtx[c(stdIdx,sample(10:nrow(gEnv$allZoidMtx),19)) ,] ;makeInfoStr=T

		aLen <- nrow(aZoidMtx)
		cName <- c(	# "c51","c52","c41","c42","c43"		# 폐지. 시간이 너무 오래걸리는 듯 하다.
					"c31","c32","c33","c34"
					,"c21","c22","c23","c24","c25"
					,"max3","min3","max2","min2"
					,"cTbl","fTbl"
				)
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}
		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		if( is.null(rObj$cInfo) ){ # stdMIObj$zMtx 데이터가 부족한 상태
			# cInfo가 없으면 aFStep 관련 제약조건들도 없겠지.
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			aCStep <- aZoid[2:6] - aZoid[1:5]	# ;aFStep <- aZoid - rObj$lastZoid	;aRem <- aZoid %% 10
			aFStep <- aZoid - rObj$lastZoid

			# minN, maxN
			aCStep.srt <- sort(unique(aCStep))
			cLen <- length(aCStep.srt)
			if( 3<=cLen ){
				scoreMtx[aIdx ,"max3"] <- all( aCStep.srt[cLen-0:2]==rObj$cInfo$max3 )
				scoreMtx[aIdx ,"min3"] <- all( aCStep.srt[1:3]==rObj$cInfo$min3 )
			}
			if( 2<=cLen ){
				scoreMtx[aIdx ,"max2"] <- all( aCStep.srt[cLen-0:1]==rObj$cInfo$max2 )
				scoreMtx[aIdx ,"min2"] <- all( aCStep.srt[1:2]==rObj$cInfo$min2 )
			}

			cTbl <- table(aCStep)
			if( length(rObj$cInfo$cTbl)==length(cTbl) ){
				scoreMtx[aIdx ,"cTbl"] <- all(names(cTbl)==names(rObj$cInfo$cTbl)) && all(cTbl==rObj$cInfo$cTbl)
			}

			if( !is.null(rObj$fTbl) ){
				fTbl <- table(aFStep)
				if( length(rObj$fTbl)==length(fTbl) ){
					scoreMtx[aIdx ,"fTbl"] <- all(names(fTbl)==names(rObj$fTbl)) && all(fTbl==rObj$fTbl)
				}
			}
			# if( makeInfoStr ){ }
		}

		#	matNLst --------------------------------------------------------
		# for( idx in 1:2 ){	# c5.x
		# 	logId <- sprintf("c5%d",idx)
		# 	scoreMtx[,logId] <- rObj$cInfo$mat5Lst[[logId]]$match( aZoidMtx[,0:4+idx,drop=F] )
		# }
		# for( idx in 1:3 ){	# c4.x
		# 	logId <- sprintf("c4%d",idx)
		# 	scoreMtx[,logId] <- rObj$cInfo$mat4Lst[[logId]]$match( aZoidMtx[,0:3+idx,drop=F] )
		# }
		for( idx in 1:4 ){	# c3.x
			logId <- sprintf("c3%d",idx)
			scoreMtx[,logId] <- rObj$cInfo$mat3Lst[[logId]]$match( aZoidMtx[,0:2+idx,drop=F] )
		}
		for( idx in 1:5 ){	# c2.x
			logId <- sprintf("c2%d",idx)
			scoreMtx[,logId] <- rObj$cInfo$mat2Lst[[logId]]$match( aZoidMtx[,0:1+idx,drop=F] )
		}



		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}

	return( rObj )

	if( FALSE ){	# rObj$cInfo$mat3Lst ,rObj$cInfo$mat2Lst 살펴보기 용도.

		mat3MtxLst <- list()
		for( matName in names(fltObj$cInfo$mat3Lst) ){
			matObj <- fltObj$cInfo$mat3Lst[[matName]]
			hpnMtx <- sapply( matObj$hpnLst ,function( p ){ 
						names(p$cStep) <- paste( "cStep",names(p$cStep),sep="_" )
						names(p$raw) <- paste( "raw",names(p$raw),sep="_" )
						return( c(p$cStep,p$raw) )
			})
			hpnMtx <- t( hpnMtx )
			rName <- rownames(hpnMtx)
			mat3MtxLst[[matName]] <- hpnMtx[order(rName),]
		}

		mat2MtxLst <- list()
		for( matName in names(fltObj$cInfo$mat2Lst) ){
			matObj <- fltObj$cInfo$mat2Lst[[matName]]
			hpnMtx <- sapply( matObj$hpnLst ,function( p ){ 
						names(p$cStep) <- paste( "cStep",names(p$cStep),sep="_" )
						names(p$raw) <- paste( "raw",names(p$raw),sep="_" )
						return( c(p$cStep,p$raw) )
			})
			hpnMtx <- t( hpnMtx )
			rName <- rownames(hpnMtx)
			mat2MtxLst[[matName]] <- hpnMtx[order(rName),]
		}

	}


} # bFMtx.score8( )


#	fCutU.getFiltObjPair( stdMI$fStepTail )
bFMtx.score9 <- function( stdMIObj ){
	#	stdMIObj <- stdMI.grp$basic$basic
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="score9"	,zMtx.size=nrow(zMtx)
					,lastZoid=stdMI$lastZoid
				)

	rObj$rawBan <- NULL	;rObj$remBan <- NULL	;rObj$cBan <- NULL	;rObj$fBan <- NULL
	if( 0<nrow(stdMI$rawTail) ){

		# rawBan ------------------------------------------
		df <- u0.zoidMtx_ana(stdMI$rawTail)
		if( 0<nrow(df) ){
			df <- df[ 0< df[,"banVal"] ,]
			df <- df[45>=df[,"banVal"] ,]
		}
		chkCol <- sort(unique(df[,"tgt.col"]))
		dfLst <- vector("list",6)
		for( cIdx in chkCol ){	dfLst[[cIdx]]<-df[df[,"tgt.col"]==cIdx ,] }
		rObj$rawBan <- list( chkCol=chkCol, dfLst=dfLst ,df=df )

		# remBan ------------------------------------------
		df <- u0.zoidMtx_ana(stdMI$rawTail%%10)
		if( 0<nrow(df) ){
			df <- df[ 0<=df[,"banVal"] ,]
			df[ 10==df[,"banVal"] ,"banVal"] <- 0
			df <- df[ 10> df[,"banVal"] ,]
		}
		chkCol <- sort(unique(df[,"tgt.col"]))
		dfLst <- vector("list",6)
		for( cIdx in chkCol ){	dfLst[[cIdx]]<-df[df[,"tgt.col"]==cIdx ,] }
		rObj$remBan <- list( chkCol=chkCol, dfLst=dfLst ,df=df )

		# cBan ------------------------------------------
		df <- u0.zoidCMtx_ana( stdMI$rawTail )
		if( 0<nrow(df) ){
			df <- df[ 0< df[,"banVal"] ,]
		}
		chkCol <- sort(unique(df[,"tgt.col"]))
		dfLst <- vector("list",6)
		for( cIdx in chkCol ){	dfLst[[cIdx]]<-df[df[,"tgt.col"]==cIdx ,] }
		rObj$cBan <- list( chkCol=chkCol, dfLst=dfLst ,df=df )

		# fBan ------------------------------------------
		df <- u0.zoidFMtx_ana( stdMI$rawTail )
		chkCol <- sort(unique(df[,"tgt.col"]))
		dfLst <- vector("list",6)
		for( cIdx in chkCol ){	dfLst[[cIdx]]<-df[df[,"tgt.col"]==cIdx ,] }
		rObj$fBan <- list( chkCol=chkCol, dfLst=dfLst ,df=df )

	}

	rObj$checkBan <- function( srcVal ,banObj ){
		#	srcVal <- c( 8,23,32,33,34,40)	;banObj <- rObj$rawBan
		fLst <- list()
		for( cIdx in banObj$chkCol ){
			if( !(srcVal[cIdx] %in% banObj$dfLst[[cIdx]][,"banVal"]) ) next	# 처리 속도를 위해

			flag <- srcVal[cIdx] == banObj$dfLst[[cIdx]][,"banVal"]
			fObj <- list( fInfo=c(cIdx=cIdx ,val=srcVal[cIdx] ,dupLen=sum(flag))
						,typ=sort(as.character(banObj$dfLst[[cIdx]][flag,"tgt.dir"]))
					)
			fLst[[sprintf("C%d",cIdx)]] <- fObj
		}

		bDupCnt <- sapply( fLst ,function(obj){obj$fInfo["dupLen"]})
		names(bDupCnt) <- names(fLst)

		typ <- do.call( c ,lapply(fLst,function(obj){obj$typ}))
		rFObj <- list( cnt=length(fLst) ,bDupCnt=bDupCnt ,typCnt=table(typ) )

		return( rFObj )
	} # rObj$checkBan( )


	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	aZoidMtx <- gEnv$allZoidMtx[c(stdIdx,sample(10:nrow(gEnv$allZoidMtx),19)) ,] ;makeInfoStr=T

		aLen <- nrow(aZoidMtx)
		cName <- c(	"rCnt","rD2","rDn","rLr","rRl"	,"eCnt","eD2","eDn","eLr","eRl"
					,"cCnt","cD2","cDn","cLr","cRl"	,"fCnt","fD2","fDn","fLr","fRl"
				)
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}
		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		if( is.null(rObj$rawBan) ){ # stdMIObj$zMtx 데이터가 부족한 상태
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			aRem <- aZoid %% 10
			aCStep <- aZoid[2:6] - aZoid[1:5]	
			aFStep <- aZoid - rObj$lastZoid

			# 		rObj$rawBan[,c("tgt.col","banVal","tgt.dir")]	# 		rObj$checkBan( aZoid ,rObj$rawBan )
			# 							tgt.col banVal tgt.dir		# 				$cnt		[1] 4
			# 						824        1      8     col		# 				$bDupCnt	dupLen dupLen dupLen dupLen 
			# 						8241       2      0     col		# 								1      1      1      1 
			# 						843        5     34     col		# 				$typCnt		col Slide\\ 
			# 						809        5     23     col		# 							3       1 
			# 						8242       5     30     col
			# 						1          2     31  Slide/
			# 						11         3     32  Slide/

			banR <- rObj$checkBan( aZoid ,rObj$rawBan )
			scoreMtx[aIdx,"rCnt"] 	<- banR$cnt
			scoreMtx[aIdx,"rD2"]	<- sum(banR$bDupCnt==2)
			scoreMtx[aIdx,"rDn"]	<- sum(banR$bDupCnt >2)
			scoreMtx[aIdx,"rLr"]	<- ifelse(is.na(banR$typCnt["Slide\\"]),0,banR$typCnt["Slide\\"])
			scoreMtx[aIdx,"rRl"]	<- ifelse(is.na(banR$typCnt["Slide/"]),0,banR$typCnt["Slide/"])

			banE <- rObj$checkBan( aRem ,rObj$remBan )
			scoreMtx[aIdx,"eCnt"] 	<- banE$cnt
			scoreMtx[aIdx,"eD2"]	<- sum(banE$bDupCnt==2)
			scoreMtx[aIdx,"eDn"]	<- sum(banE$bDupCnt >2)
			scoreMtx[aIdx,"eLr"]	<- ifelse(is.na(banE$typCnt["Slide\\"]),0,banE$typCnt["Slide\\"])
			scoreMtx[aIdx,"eRl"]	<- ifelse(is.na(banE$typCnt["Slide/"]),0,banE$typCnt["Slide/"])

			banC <- rObj$checkBan( aCStep ,rObj$cBan )
			scoreMtx[aIdx,"cCnt"] 	<- banC$cnt
			scoreMtx[aIdx,"cD2"]	<- sum(banC$bDupCnt==2)
			scoreMtx[aIdx,"cDn"]	<- sum(banC$bDupCnt >2)
			scoreMtx[aIdx,"cLr"]	<- ifelse(is.na(banC$typCnt["Slide\\"]),0,banC$typCnt["Slide\\"])
			scoreMtx[aIdx,"cRl"]	<- ifelse(is.na(banC$typCnt["Slide/"]),0,banC$typCnt["Slide/"])

			banF <- rObj$checkBan( aFStep ,rObj$fBan )
			scoreMtx[aIdx,"fCnt"] 	<- banF$cnt
			scoreMtx[aIdx,"fD2"]	<- sum(banF$bDupCnt==2)
			scoreMtx[aIdx,"fDn"]	<- sum(banF$bDupCnt >2)
			scoreMtx[aIdx,"fLr"]	<- ifelse(is.na(banF$typCnt["Slide\\"]),0,banF$typCnt["Slide\\"])
			scoreMtx[aIdx,"fRl"]	<- ifelse(is.na(banF$typCnt["Slide/"]),0,banF$typCnt["Slide/"])

			# if( makeInfoStr ){ }
		}

		# for( idx in 1:4 ){	# c3.x
		# 	logId <- sprintf("c3%d",idx)
		# 	scoreMtx[,logId] <- rObj$cInfo$mat3Lst[[logId]]$match( aZoidMtx[,0:2+idx,drop=F] )
		# }

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
	}

	return( rObj )

} # bFMtx.score9( )


bFMtx.scoreA <- function( stdMIObj ){

	#	stdMIObj <- stdMI.grp$basic[[pName]]
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="scoreA"	,zMtx.size=nrow(zMtx)	,lastZoid=stdMI$lastZoid	)
	rObj$mtxPtnObj	<- NULL		;rObj$mtxColPtnObj <- NULL

	hSize <- nrow(stdMI$rawTail)
	if( 0 <= hSize ){

		# quoTail  -------------------------------------------------------------
		rObj$quoTail <- stdMI$quoTail
		rObj$quoLst <- list()	#	$size		,$qValLst
		for( hIdx in seq_len(hSize) ){
			rObj$quoLst[[hIdx]] <- fCutU.getQuoObj( stdMI$rawTail[hIdx,] ,valSet=T )
		}

		rObj$mtxPtnObj <- bUtil.mtxPtn( rObj$quoTail )
		rObj$mtxColPtnObj <- bUtil.mtxColPtn( rObj$quoTail )


		# remTblMtx  -----------------------------------------------------------
		remTbl <- table(stdMI$lastZoid %% 10)
		remTbl <- remTbl[ order(names(remTbl)) ]
		rObj$remMtx <- matrix( c( as.integer(names(remTbl)) ,remTbl ) ,byrow=T
					,nrow=2 ,ncol=length(remTbl) ,dimnames=list(c("rem","cnt")) 
		)

		cName <- c(	"xaAVLen","axAVLen"
			        ,"aMHpn","aMHpnVLen","aNHpnF","aNHpnVLen","aSHpnVLen_abbA","aSHpnVLen_abxbA"
					,"paaAH1","paaAH1VLen","paaAH2","paaAH2VLen","paaAH3","paaAH3VLen","paaAHn","paaAHnVLen"
					,"pabbAH1","pabbAH1VLen","pabbAH2","pabbAH2VLen","pabbAH3","pabbAH3VLen","pabbAHn","pabbAHnVLen"
					,"pbbaA" ,"pbbaAVLen" ,"pbabA" ,"pbabAVLen" ,"pabxbA" ,"pabxbAVLen"
					,"remTblF" 
				)
			# xaAVLen, axAVLen : quo size가 모두 일치하는 h, h-1가 있는 경우, quo 값 까지 일치하는 길이.
			# aMHpn		: all match - multi hpn 갯수 (multi hpn이 2개 이상이더라도 어차피 매치는 한쪽만 된다는 걸 참고.)
			# aMHpnVLen	: all match - multi hpn 에서 quo 블럭의 값이 일치한 것들의 총 길이(발생이 없으면 0)
			# aNHpnF	: all match - multi Hpn에 대한 next hpn과 일치하는지 여부.
			# aNHpnVLen	: all match - next hpn 일치 상태에서, 값이 동일한 quo 블럭의 총 길이(발생이 없으면 0)
			# aSHpnVLen_abbA	: all match - Symm 형태(abbA) 매치 발생 상태에서 동일 값 Quo 블럭 총 길이(발생이 없으면 0)
			# aSHpnVLen_abxbA	: all match - Symm 형태(abxbA) 매치 발생 상태에서 동일 값 Quo 블럭 총 길이(발생이 없으면 0)
			# paaAH1	:   aaA 형태 발생이 일어난 컬럼 수(전체 컬럼이 아닌 부분매치)
			# paaAH1VLen:   aaA 형태 발생이 일어난 컬럼들에서 동일 값 Quo 블럭의 총 길이.(발생이 없으면 0)
			# paaAH2	:  aaaA 형태 발생이 일어난  수. paaAHn은 4이상의 H 연속발생을 다룬다. (aaa..aaA)
			# pabbAH1,pabbAH1VLen	:   aaA 형태 발생에 대한 바로 이전 H, 즉 Symm 패턴을 다룬다.

	}

	rObj$quoMatch <- function( aQuo ,hQuo ){
		#	aQuo <- fCutU.getQuoObj( aZoid ,valSet=T )
		matRst <- list( lenMatFlag=rep(F,length(aQuo$qValLst)) )
		names( matRst$lenMatFlag ) <- names( hQuo$size )
		matRst$info <- c( "allMatF"=F ,"matColCnt"=0 ,"matValLen"=0 )

		qValMat <- matRst$lenMatFlag
		for( qIdx in 1:length(aQuo$qValLst) ){
			matRst$lenMatFlag[qIdx] <- aQuo$size[qIdx]==hQuo$size[qIdx]
			if( matRst$lenMatFlag[qIdx] ){
				qValMat[qIdx] <- all( aQuo$qValLst[[qIdx]]==hQuo$qValLst[[qIdx]] )
			}
		}

		matRst$info["allMatF"]		<- all(matRst$lenMatFlag)
		matRst$info["matColCnt"]	<- sum(matRst$lenMatFlag)
		matRst$info["matValLen"]	<- sum(aQuo$size[qValMat])

		return( matRst )
	}

	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){
		#	aZoidMtx <- gEnv$allZoidMtx[c(stdIdx,sample(10:nrow(gEnv$allZoidMtx),19)) ,] ;makeInfoStr=T

		aLen <- nrow(aZoidMtx)
		cName <- c(	"xaAVLen","axAVLen"
					,"aMHpn","aMHpnVLen","aNHpnF","aNHpnVLen","aSHpnVLen_abbA","aSHpnVLen_abxbA"
					,"paaAH1","paaAH1VLen","paaAH2","paaAH2VLen","paaAH3","paaAH3VLen","paaAHn","paaAHnVLen"
					,"pabbAH1","pabbAH1VLen","pabbAH2","pabbAH2VLen","pabbAH3","pabbAH3VLen","pabbAHn","pabbAHnVLen"
					,"pbbaA" ,"pbbaAVLen" ,"pbabA" ,"pbabAVLen" ,"pabxbA" ,"pabxbAVLen"
					,"remTblF" 
				)
			# xaAVLen, axAVLen : quo size가 모두 일치하는 h, h-1가 있는 경우, quo 값 까지 일치하는 길이.
			# aMHpn		: all match - multi hpn 갯수 (multi hpn이 2개 이상이더라도 어차피 매치는 한쪽만 된다는 걸 참고.)
			# aMHpnVLen	: all match - multi hpn 에서 quo 블럭의 값이 일치한 것들의 총 길이(발생이 없으면 0)
			# aNHpnF	: all match - multi Hpn에 대한 next hpn과 일치하는지 여부.
			# aNHpnVLen	: all match - next hpn 일치 상태에서, 값이 동일한 quo 블럭의 총 길이(발생이 없으면 0)
			# aSHpnVLen_abbA	: all match - Symm 형태(abbA) 매치 발생 상태에서 동일 값 Quo 블럭 총 길이(발생이 없으면 0)
			# aSHpnVLen_abxbA	: all match - Symm 형태(abxbA) 매치 발생 상태에서 동일 값 Quo 블럭 총 길이(발생이 없으면 0)
			# paaAH1	:   aaA 형태 발생이 일어난 컬럼 수(전체 컬럼이 아닌 부분매치)
			# paaAH1VLen:   aaA 형태 발생이 일어난 컬럼들에서 동일 값 Quo 블럭의 총 길이.(발생이 없으면 0)
			# paaAH2	:  aaaA 형태 발생이 일어난  수. paaAHn은 4이상의 H 연속발생을 다룬다. (aaa..aaA)
			# pabbAH1,pabbAH1VLen	:   aaA 형태 발생에 대한 바로 이전 H, 즉 Symm 패턴을 다룬다.
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}
		if( 0==rObj$zMtx.size ){
			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

		hSize <- length(rObj$quoLst)
		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
			aRem <- aZoid %% 10
			# aCStep <- aZoid[2:6] - aZoid[1:5]		;aFStep <- aZoid - rObj$lastZoid

			aQuo <- fCutU.getQuoObj( aZoid ,valSet=T )

			if( 1<=hSize ){
				quoMat <- rObj$quoMatch( aQuo ,rObj$quoLst[[hSize]] )
				scoreMtx[aIdx,"xaAVLen"] 	<- ifelse( 0==quoMat$info["allMatF"] ,0 ,quoMat$info["matValLen"] )
			}
			if( 2<=hSize ){
				quoMat <- rObj$quoMatch( aQuo ,rObj$quoLst[[hSize-1]] )
				scoreMtx[aIdx,"axAVLen"] 	<- ifelse( 0==quoMat$info["allMatF"] ,0 ,quoMat$info["matValLen"] )


				# ,"aMHpn","aMHpnVLen","aNHpnF","aNHpnVLen","aSHpnVLen_abbA","aSHpnVLen_abxbA"

				# > rObj$mtxPtnObj
				# 	$hpnCode	[1] 1 0 0 1 0 0
				# 	$mHpnLst	$mHpnLst[[1]]	[1] 1 4
				# 	$nHpnLst	$nHpnLst[[1]]	[1] 4
				# 	$symmHpn		abbA abxbA 
				# 						0     0 


			}

			return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )
		}

	}

	return( rObj )

}

