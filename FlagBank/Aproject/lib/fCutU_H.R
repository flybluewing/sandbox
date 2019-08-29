# fCutU_H.R 최종접근
fCutU.remFilt <- function( srcVal ,banRem ,excVal=NULL ){
	# srcVal<-12	;banRem=c( 2, 3, 4) ;excVal<-NULL
	if( !is.null(excVal) && (srcVal%in%excVal) )	return( FALSE )

	return( (srcVal%%10) %in% banRem )

} # fCutU.remFilt( )

fCutU.spanMatch <- function( src ,tgt ,posDiff ,size=3 ){
	# tgt : aZoid
	# src : 검색값 (src와 tgt의 길이는 동일)
	# posDiff : src와 tgt의 어긋남 정도. tgt에 비해 src가 왼쪽으로 이동하면 -1
	#	( 0이면 src[1]과 tgt[1]이 동일하며, -1이면 src[2]가 tgt[1]과 동일하다. )
	#	sample : tgt<-c(1,3,4,5,5,6)	;src<-1:6	;posDiff<--1	;size<-3
	#		return value : 4
	idx.src <- 1:length(src)
	idx.tgt <- 1:length(tgt)
	if( posDiff>0 ){
		if( length(src) < (posDiff+size) ) return( 0 )

		idx.src <- idx.src[ 1:(length(idx.src)-posDiff) ]
		idx.tgt <- idx.tgt[ (posDiff+1):length(idx.tgt)     ]
	} else if( posDiff<0 ) {
		if( (length(src)+posDiff) < size ) return( 0 )

		idx.src <- idx.src[ (-posDiff+1):length(idx.src)     ]
		idx.tgt <- idx.tgt[ 1:(length(idx.tgt)+ posDiff) ]
	}

	score <- 0
	for( idx in 1:(length(idx.src)-size+1) ){
		cnt <- sum( src[ idx.src[idx:(idx+size-1)] ] == tgt[ idx.tgt[idx:(idx+size-1)] ] )
		score <- score + ifelse( cnt>1 ,cnt-1 ,0 )
	}
	return( score )
} # fCutU.spanMatch()

fCutU.neighborObj <- function( pMtx ){

	getSpanLst <- function( idx ){
		spanLst <- list()
		for( dIdx in 2:0 ){
			lEnd <- idx-dIdx
			rEnd <- idx-dIdx+2
			if( lEnd>0 && rEnd<=6 ){
				spanLst[[1+length(spanLst)]] <- lEnd:rEnd
			}
		}
		return( spanLst )
	} #getSpanLst()

	if( 2>nrow(pMtx) ){
		rObj <- list()
		rObj$matchCnt <- function( aZoid ){ return(0) }
		return( rObj )
	}

	vals <- sort(unique(as.vector(pMtx)))
	freqVals <- vals[ table(pMtx)>1 ]
	names( freqVals ) <- freqVals

	neighborLst <- list()
	for( fVal in freqVals ){
		fndLst <- list()
		for( rIdx in 1:(nrow(pMtx)-1)){
			f.idx <- which(pMtx[rIdx,]==fVal)
			if( 0==length(f.idx) )	next

			spanLst <- getSpanLst( f.idx )
			for( lIdx in 1:length(spanLst) ){
				fndLst[[1+length(fndLst)]] <- pMtx[ rIdx, spanLst[[lIdx]] ]
			}
		}
		if( 0<length(fndLst) ){
			neighborLst[[as.character(fVal)]] <- fndLst
		}
	}

	rObj <- list( neighborLst=neighborLst ,freqVals=freqVals )
	rObj$matchCnt <- function( aZoid ){
		srchValIdx <- which( rObj$freqVals%in%aZoid )
		if( 0==length(srchValIdx) ) return( 0 )

		score <- 0
		for( sIdx in srchValIdx ){
			banPtnLst <- rObj$neighborLst[[sIdx]]
			for( idx in 1:length(banPtnLst) ){
				banVal <- banPtnLst[[idx]]
				for( cIdx in 1:4 ){
					if( all(banVal==aZoid[0:2+cIdx]) ){
						# cat(sprintf("sIdx:%d idx:%d %s \n",sIdx,idx,paste(banVal,collapse=",") ))
						score <- score + 1
					}
				} # cIdx
			} # idx
		} # sIdx

		return( score )
	} # rObj$matchCnt( )

	return( rObj )

} # fCutU.neighborLst()

fCutU.hasPtn <- function( src ,tgt ,thld=NULL ,fixIdx=NULL ){ # < official >
	# thld : 이거 이상 매치되어야 함.
	# fixIdx : src[fixIdx] 는 반드시 포함되어야 함.


	# if( is.null(thld) ){	# 기능 확인 후 삭제.
	# 	thld <- sum(!is.na(src))
	# } else if( thld>sum(!is.na(src)) ){
	# 	return( FALSE )
	# }

	# src.len <- length(src)	;tgt.len <- length(tgt)
	# colSpan <- 1:src.len - 1
	# for( cIdx in 1:(tgt.len-src.len+1) ){
	# 	matFlag <- tgt[cIdx+colSpan]==src
	# 	if( thld<=sum(matFlag,na.rm=T) ){
	# 		if( !is.null(fixIdx) && !matFlag[fixIdx] ){
	# 			next	
	# 		}
	# 		return( TRUE )
	# 	}
	# }
	
	# ------------------------------------------
	if( is.null(thld) ){
		thld <- sum(!is.na(src))
	}

	src.len <- length(src)	;tgt.len <- length(tgt)
	if( thld>src.len || thld>tgt.len )	return( FALSE )

	matFlag <- rep( NA, src.len )
	towSpan <- (src.len-thld+1):(-tgt.len + thld + 1)	# slide를 끄는 기준점.
	for( towIdx in towSpan ){
		# src.span ---------------------------------------------------------
		sIdx <- ifelse(towIdx<1,1,towIdx)	# start Idx
		eIdx <- if(	towIdx<1 ){ 		# end Idx
					tgtLeft.len <- tgt.len + towIdx -1	# 여기서 towIdx값은 -상태
					ifelse( tgtLeft.len>src.len ,src.len ,tgtLeft.len )
				} else {
					overlap.len <- src.len - towIdx +1
					ifelse( overlap.len>tgt.len ,towIdx+tgt.len-1 ,src.len )
				}
		src.span <- sIdx:eIdx
		# tgt.span ---------------------------------------------------------
		sIdx <- ifelse( towIdx<1 , -towIdx+2, 1 )
		eIdx <- if( towIdx<1 ){
					tgtLeft.len <- tgt.len + towIdx -1	# towIdx 값은 -상태
					ifelse( tgtLeft.len>src.len ,(-towIdx+1+src.len) ,tgt.len )
				} else {
					overlap.len <- src.len - towIdx +1
					ifelse( overlap.len<tgt.len ,overlap.len ,tgt.len )
				}
		tgt.span <- sIdx:eIdx
		# for debug
		# 		src.span.str <- paste( sprintf("%2d",src.span) ,collapse="")
		# 		cat( sprintf("%2d src.span : %s \n",towIdx,src.span.str ) )
		# 		tgt.span.str <- paste( sprintf("%2d",tgt.span) ,collapse="")
		# 		cat( sprintf("%2d tgt.span : %s \n",towIdx,tgt.span.str ) )

		matFlag[] <- NA
		matFlag[src.span] <- src[src.span]==tgt[tgt.span]
		#		cat( sprintf("%2d : %s \n",towIdx,paste( matFlag ,collapse=" ") ) )

		if( thld<=sum(matFlag,na.rm=T) ){
			if( !is.null(fixIdx) ){
				if( any(is.na(matFlag[fixIdx])) || any(!matFlag[fixIdx]) ){ 
					next 
				}
			}
			# cat("       found\n")
			return( TRUE )
		}
	}

	# debug test
	# src <- c( 4, 2, 2, 1, 1, 1)	;tgt <- c( 1, 4, 2)
	# thld <- 3		;fixIdx <- c(1,2)

	return( FALSE )

} # fCut.hasPtn()


fCutU.hasRow <- function( val ,mtx ){ # < official >
	for( rIdx in 1:nrow(mtx) ){
		if( all(val==mtx[rIdx,]) ){
			return( TRUE )
		}
	}
	return( FALSE )
} # fCut.hasRow()

fCutU.seqRebCnt <- function( pZh ,pZoid ,pRowLen=10 ,pLen=2 ){ # < official >

	colLen <- ncol(pZh)
	zhRowLen <- nrow(pZh)

	rObj <- list( cnt=0 ,dbgLst=list() )
	for( cIdx in 1:(colLen-pLen+1) ){
		for( hIdx in (zhRowLen-0:(pRowLen-1)) ){
			fnd <- fCutU.hasPtn( pZoid[cIdx+0:(pLen-1)] ,pZh[hIdx,] )
			if( fnd ){
				rObj$dbgLst[[ 1+length(rObj$dbgLst) ]] <- c( cIdx ,hIdx )
				rObj$cnt <- 1 + rObj$cnt
			}
		}
	}
	return( rObj )

} # fCut.seqRebCnt()

fCutU.getQuoTblLst <- function( zhF ){ # < official >

	hLen <- nrow( zhF )

	quoLst <- apply( zhF%/%10 ,1 ,function(zCode){
					obj <- list( tbl=table(zCode) )
					obj$valStr <- paste(obj$tbl,collapse=" ")
					obj$quoStr <- paste(names(obj$tbl),collapse=" ")
					obj$idStr <- sprintf("V:%s Q:%s",obj$valStr,obj$quoStr)
					return(obj)
				})

	return( quoLst )

} # fCutU.getQuoTblLst()

fCutU.getNextZW <- function( gEnv ){ # < official >

	hLen <- nrow( gEnv$zhF )

	hZW <- gEnv$zhF[,6] - gEnv$zhF[,1]
	nextZW.idx <- which( hZW[1:(hLen-1)]==hZW[hLen] )+1
	
	rObj <- list( zMtx=gEnv$zhF[nextZW.idx,] ,hZW=hZW )
	return( rObj )

} # fCutU.getNextZW()

fCutU.getNextQuo10 <- function( gEnv ){ # < official >

	hLen <- nrow( gEnv$zhF )
	lastQuoPtn <- table( gEnv$zhF[hLen,]%/%10 )
	lastQuoPtn.len <- length(lastQuoPtn)
	flag <- apply( gEnv$zhF[1:(hLen-1),]%/%10 ,1 ,function( aCode ){
					tbl <- table(aCode)
					if( length(tbl)!=lastQuoPtn.len ) return( FALSE )

					return( all(tbl==lastQuoPtn) )
				})
	indices <- which( flag )+1

	rObj <- list( zMtx=gEnv$zhF[indices,] ,indices=indices )
	return( rObj )

} # fCutU.getNextQuo10()

fCutU.getNextBin <- function( gEnv ){ # < official >
	hLen <- nrow( gEnv$zhF )
	lastBin <- gEnv$zhF[hLen,]%%2

	flag <- sapply( 1:(hLen-1) ,function( hIdx ){ all((gEnv$zhF[hIdx,]%%2)==lastBin) })
	flag.idx <- which( flag )+1

	rObj <- list( zMtx=gEnv$zhF[flag.idx,] ,lastBin=lastBin )
	return( rObj )
} # fCutU.getNextBin()

fCutU.getNextCStepBin <- function( gEnv ){ # < official >
	hLen <- nrow( gEnv$zhF )
	cStepMtx <- t( apply(gEnv$zhF ,1 ,function(zoid){(zoid[2:6]-zoid[1:5])}) )
	lastBin <- cStepMtx[hLen,] %% 2

	flag <- sapply( 1:(hLen-1) ,function( hIdx ){ all((cStepMtx[hIdx,]%%2)==lastBin) })
	flag.idx <- which( flag )+1

	rObj <- list( zMtx=gEnv$zhF[flag.idx,] ,lastBin=lastBin )
	return( rObj )
} # fCutU.getNextCStepBin()

fCutU.getNextFStepBin <- function( gEnv ){ # < official >
	hLen <- nrow( gEnv$zhF )
	fStepMtx <- do.call( rbind ,lapply( 2:hLen ,function(hIdx){ gEnv$zhF[hIdx,]-gEnv$zhF[(hIdx-1),] }) )
	fStepBinMtx <- rbind( c(0,0,0,0,0,0) ,abs(fStepMtx)%%2 )
	rownames( fStepBinMtx ) <- 1:hLen
	lastBin <- fStepBinMtx[hLen,] %% 2

	flag <- sapply( 1:(hLen-1) ,function( hIdx ){ all((fStepBinMtx[hIdx,]%%2)==lastBin) })
	flag.idx <- which( flag )+1

	rObj <- list( zMtx=gEnv$zhF[flag.idx,] ,lastBin=lastBin )
	return( rObj )
} # fCutU.getNextFStepBin()

fCutU.getRebNum <- function( gEnv ,rebNum=0 ){ # < official >

	hLen <- nrow( gEnv$zhF )

	rebCnt <- sapply( 2:hLen ,function( hIdx ){ sum(gEnv$zhF[(hIdx-1),] %in% gEnv$zhF[hIdx,]) })
	rebCnt <- c( 0 ,rebCnt )
	flag.idx <- which( rebCnt==rebNum )

	rObj <- list( zMtx=gEnv$zhF[flag.idx,] ,rebCnt=rebCnt )
	return( rObj )

} # fCutU.getNextZW()

fCutU.getNextRebNumPtn <- function( gEnv ,numPtn=NULL ){	# <official>
	rebNum <- sapply( 2:nrow(gEnv$zhF) ,function(hIdx){ sum(gEnv$zhF[(hIdx-1),]%in%gEnv$zhF[hIdx,]) } )
	rebNum <- c( 0 ,rebNum )	;names(rebNum) <- 1:length(rebNum)
	rebNumLen <- length(rebNum)
	if( is.null(numPtn) ){
		numPtn <- rebNum[(rebNumLen-3):rebNumLen]
	}
	numPtnLen <- length(numPtn)

	span <- (numPtnLen-1):0
	flag <- rep( FALSE ,rebNumLen )
	for( hIdx in numPtnLen:(rebNumLen-1) ){
		flag[hIdx] <- all( rebNum[hIdx-span]==numPtn )
	}

	indices <- which(flag)+1	;indices.last <- indices[length(indices)]
	lastRebMtx <- gEnv$zhF[indices.last-6:0,]	# 마지막 재발정보를 비교할 수 있도록...
	rObj <- list( zMtx=gEnv$zhF[indices,,drop=F] ,rebNum=rebNum ,lastRebMtx=lastRebMtx  )
	return( rObj )
}	# fCutU.getNextRebNumPtn()

fCutU.getNextColVal <- function( gEnv ,colIdx ){	# <official>
	hLen <- nrow( gEnv$zhF )
	lastVal <- gEnv$zhF[hLen,colIdx]

	flag <- sapply( 1:(hLen-1) ,function( hIdx ){ all(gEnv$zhF[hIdx,colIdx]==lastVal) })
	flag.idx <- which( flag )+1

	rObj <- list( zMtx=gEnv$zhF[flag.idx,,drop=F] ,lastVal=lastVal ,colIdx=colIdx )
	return( rObj )
}	# fCutU.getNextColVal()


fCutU.hist.banValScan.grp <- function( gEnv ){ # < official >

	testSpan <- 400:nrow(gEnv$zhF)

	tEnv <- gEnv
	rstLst <- list()
	for( tIdx in testSpan ){
		tEnv$zhF <- gEnv$zhF[1:(tIdx-1),]
		tEnv$allZoidMtx <- rbind( gEnv$zhF[tIdx,,drop=F] ,c(1,11,21,31,41,42) )
		tAllIdxF <- 1:2
		uAnaLstGrp <- getUAnaLstGrp( tEnv ,tAllIdxF ,pDefaultCut=FALSE ,pReport=F )

		uAnaCutDataLst.c <- list()	# uAnaCutDataLst custom
		for( nIdx in attributes(uAnaLstGrp)$names ){	# nIdx <- "uAnaLst.rebCnt"
			uAnaLst <- uAnaLstGrp[[nIdx]]
			cutDataLst <- list()
			for( uIdx in seq_len(length(uAnaLst)) ){
				cutData <- list( )
				cutData$colVal		<- uAnaLst[[uIdx]]$uAnaCutData$colVal
				cutData$colVal.f	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.f
				cutData$colVal.c	<- uAnaLst[[uIdx]]$uAnaCutData$colVal.c
				cutDataLst[[uAnaLst[[uIdx]]$idStr]] <- cutData
			} # uIdx
			uAnaCutDataLst.c[[nIdx]] <- cutDataLst
		} # nIdx
		# uAnaCutDataLst.c <- customizeCutData( uAnaCutDataLst.c )

		k.FLogStr(sprintf("=[tIdx:%d  %s]=====================================" 
							,tIdx ,paste(tEnv$allZoidMtx[1,],collapse=",") 
				))

		rObj <- banValScan.grp( pAllIdxF=tAllIdxF ,pBanLst=NULL ,grpIdx=1 ,pPhase="colVal" ,pLog=T ,gEnv=tEnv )
		rstLst[[1+length(rstLst)]] <- rObj
	}

	fltPos <- sapply( rstLst ,function(p){p$fltPos[1]})

	return( list(fltPos=fltPos ,rstLst=rstLst) )
} # hist.banValScan.grp()

fCutU.overlapSpan <- function( spanLen ,colIdx.pre ,colIdx.post ){
	# spanLen <- 5		;colIdx.pre<-4		;colIdx.post<-2
	rObj <- list()

	lMargin <- min( colIdx.pre ,colIdx.post ) -1
	rMargin <- min( spanLen-colIdx.pre ,spanLen-colIdx.post )
	
	rObj$span.pre <- (colIdx.pre-lMargin):(colIdx.pre+rMargin)
	rObj$span.post <- (colIdx.post-lMargin):(colIdx.post+rMargin)

	rObj$info <- c( spanLen ,colIdx.pre ,colIdx.post ,lMargin ,rMargin )
	names( rObj$info ) <- c( "spanLen" ,"colIdx.pre" ,"colIdx.post" ,"lMargin" ,"rMargin" )

	return( rObj )

}	# fCutU.overlapSpan()



fCutU.checkOverlap4Mtx <- function( mtx ,val ,maxDepth=5 ){

	cName <- c("valCnt","lstCnt","lstMtch","lstMtch.hpn","maxMtch")
		# valCnt : val에서 존재하는 값의 갯수.
		# lstCnt : 가장 최근의 hpn 갯수.(hpn 0가 드문 경우의 체크를 위해.)
		# lstMtch : mtx 마지막 행과 val과의 일치 수
		# lstMtch.hpn : mtx에서 마지막 발생이 나타난 행과 val과의 일치
		#		(mtx 마지막 행에 hpn이 있다면 lstMtch와 lstMtch.hpn은 같다.)
		# maxMtch	: 최대 일치 수
	matInfo <- rep( 0 ,length(cName) )		;names(matInfo) <- cName

	valCnt <- sum(!is.na(val))
	matCnt <- rep( 0 ,length(srchSpan) )
	srchDepth <- ifelse( maxDepth<nrow(mtx) ,maxDepth ,nrow(mtx) )
	for( idx in 1:srchDepth ){
		if( valCnt==sum(!is.na(mtx[nrow(mtx)-idx+1,])) ){
			matCnt[idx] <- sum(mtx[nrow(mtx)-idx+1, ]==val,na.rm=T)
		}
	}

	matInfo["valCnt"] <- valCnt
	matInfo["lstCnt"] <- sum(!is.na(mtx[nrow(mtx), ]))
	matInfo["lstMtch"] <- matCnt[1]
	hpnIdx <- which(matCnt>0)
	matInfo["lstMtch.hpn"] <- ifelse( 0<length(hpnIdx) ,matCnt[hpnIdx] ,matCnt[1] )
	matInfo["maxMtch"] <- max(matCnt)

	return( matInfo )

} # fCutU.checkOverlap4Mtx()


# zoidMtx <- gEnv$allZoidMtx[allIdxF,]	;logId="allZoid.idx1"
fCutU.logAllZoidMtx <- function( zoidMtx ,logId ){

	fileName <- sprintf("./report/%s.txt",logId)
    FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
                k.FLogStr( pMsg ,pFile=fileName ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
    }
	FLogStr( sprintf("allZoidMtx :%s",logId),pAppend=F)
	for( rIdx in 1:nrow(zoidMtx) ){
		dnaStr <- sprintf("%2d",zoidMtx[rIdx,])
		dnaStr <- paste( dnaStr ,collapse=" " )
		FLogStr(sprintf("%3d  %s",rIdx,dnaStr))
		if( 0==(rIdx%%5) ){
			FLogStr(sprintf("      "))
		}
	}
	return( fileName )
} # fCutU.logAllZoidMtx()

fCutU.getQuoObj <- function( zoid ){
	rObj <- list( tbl=table(zoid%/%10) )
	rObj$size <- rep(0,5)	;names(rObj$size) <- 0:(length(rObj$size)-1)
	rObj$size[names(rObj$tbl)] <- rObj$tbl
	rObj$valStr <- paste( rObj$tbl ,collapse=" " )
	rObj$idStr <- sprintf("V:%s Q:%s",rObj$valStr,paste(names(rObj$tbl),collapse=" "))

	rObj$sameTbl <- function( tbl ,fullMatch=FALSE ){
		if( length(rObj$tbl)!=length(tbl) ) return( FALSE )

		if( all(rObj$tbl==tbl) ){
			if( fullMatch ){
				if( all(names(rObj$tbl)==names(tbl)) ) return(FALSE)
			}
			return( TRUE )
		}
		return( FALSE )
	} # rObj$sameTbl()

	return( rObj )
} # fCutU.getQuoObj()

fCutU.getMtxInfo <- function( zMtx ){

	rObj <- list( mtxLen=nrow(zMtx) )
	lastZoid <- if( 0<rObj$mtxLen ) zMtx[rObj$mtxLen,] else NULL
	rObj$lastZoid <- lastZoid
	rObj$rem <- if( 0<rObj$mtxLen ) lastZoid%%10 else NULL

	rObj$quo10 <- fCutU.getQuoObj(lastZoid)
	rObj$cStep <- if( 0<rObj$mtxLen ) lastZoid[2:6]-lastZoid[1:5] else NULL
	rObj$fStep <- if( 1<rObj$mtxLen ) lastZoid-zMtx[rObj$mtxLen-1,] else NULL
	rObj$rawTail <- tail(zMtx)

	rObj$getCStepMtx <- function( rawMtx ){
		mtx <- apply( rawMtx ,1 ,function(zoid){zoid[2:6]-zoid[1:5]})
		return( t(mtx) )
	} # rObj$getCStepMtx()
	rObj$getFStepMtx <- function( rawMtx ){
		mtx <- matrix( NA ,nrow=nrow(rawMtx) ,ncol=ncol(rawMtx) )
		rowNum <- nrow(mtx)
		if( 1<rowNum ){
			for( rIdx in 1:(rowNum-1) ){
				mtx[rIdx+1,] <- rawMtx[rIdx+1,] - rawMtx[rIdx,]
			}
		}
		return( mtx )
	} # rObj$getFStepMtx()

	rObj$cStepTail <- if( 0<rObj$mtxLen ) tail(rObj$getCStepMtx(zMtx)) else NULL
	rObj$fStepTail <- if( 1<rObj$mtxLen ) tail(rObj$getFStepMtx(zMtx)) else NULL

	rObj$quoTail <- t(apply( rObj$rawTail ,1 ,function(zoid){ fCutU.getQuoObj(zoid)$size }))
	rObj$quoRebPtn <- fCutU.chkRowPtnReb( rObj$quoTail )

	return( rObj )
} # fCutU.getMtxInfo

fCutU.logAllZoidMtx <- function( zoidMtx ,logId  ){

	fileName <- sprintf("./report/%s.txt",logId)
    FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
                k.FLogStr( pMsg ,pFile=fileName ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
    }
	FLogStr( sprintf("allZoidMtx :%s",logId),pAppend=F)
	for( rIdx in 1:nrow(zoidMtx) ){
		dnaStr <- sprintf("%2d",zoidMtx[rIdx,])
		dnaStr <- paste( dnaStr ,collapse=" " )
		FLogStr(sprintf("%3d  %s",rIdx,dnaStr))
		if( 0==(rIdx%%5) ){
			FLogStr(sprintf("      "))
		}
	}
	return( fileName )

} # fCutU.logAllZoidMtx( )

fCutU.rptColValSeqNext <- function( gEnv ,allIdxF ,logId ){

	fileName <- sprintf("./toFinal/data/%s.txt",logId)
    FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
                k.FLogStr( pMsg ,pFile=fileName ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
    }
	rptSameRow <- function( fndMtx ){
		if( 2>nrow(fndMtx) ) return( "Val :  ,    Rem :  ,      (SameRow,Rebind)" )

		val.sr <- 0<length( sameRow( fndMtx ) )
		val.rb <- FALSE
		for( rIdx in 2:nrow(fndMtx) ){
			val.rb <- all( fndMtx[(rIdx-1),]==fndMtx[rIdx,] )
			if( val.rb ) break
		}

		remMtx <- fndMtx %% 10		
		rem.sr <- 0<length( sameRow( remMtx ) )
		rem.rb <- FALSE
		for( rIdx in 2:nrow(remMtx) ){
			rem.rb <- all( remMtx[(rIdx-1),]==remMtx[rIdx,] )
			if( rem.rb ) break
		}
		
		rptStr <- sprintf("Val :%s,%s  Rem :%s,%s    (SameRow,Rebind)"
						,ifelse(val.sr," *","  ")	,ifelse(val.rb," *","  ")
						,ifelse(rem.sr," *","  ")	,ifelse(rem.rb," *","  ")
					)
		return( rptStr )
	}

	FLogStr(sprintf("logId:%s",logId),pAppend=F)

	FLogStr("# =========================================================")
	FLogStr("#	Raw Value                                              =")
	FLogStr("# =========================================================")
	FLogStr("                                                           ")
	FLogStr("# =========================================================")
	FLogStr("# anaColEndPtn()")
	colPtnLst <- anaColEndPtn( gEnv$zhF ,pDebug=T )
	for( idx in 1:length(colPtnLst) ){
		valStr <- sprintf("%2d",colPtnLst[[idx]]$val)

		rebFlag <- FALSE
		if( 1<length(valStr) ){
			rebFlag <- any( valStr[2:length(valStr)]==valStr[1:(length(valStr)-1)] )
		}

		valStr <- paste( valStr ,collapse=" ")
		FLogStr(sprintf("    [%d]%s  %s",idx,ifelse(rebFlag,"*"," "),valStr))
	}
	FLogStr("                         ")


	colSize <- 2
	FLogStr("# =========================================================")
	FLogStr(sprintf("# colValSeqNext( ,pColSize=%d )",colSize))
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=colSize )
	lstSize <- length(cvSeqNextLst)
	fndSize <- sapply( cvSeqNextLst ,function(cv){nrow(cv$fndMtx)})
	for( lIdx in 1:lstSize ){
		rptStr <- rptSameRow( cvSeqNextLst[[lIdx]]$fndMtx )
		FLogStr(sprintf("    <%d> %s",lIdx,rptStr))
	}

	for( rIdx in seq_len(max(fndSize)) ){
		rValStr <- NULL
		for( lIdx in 1:lstSize ){
			valStr <- NULL
			for( cIdx in 1:colSize ){
				if( rIdx>fndSize[lIdx] ) { valStr[cIdx] <- "  "
				} else { valStr[cIdx] <- sprintf("%2d",cvSeqNextLst[[lIdx]]$fndMtx[rIdx,cIdx])	}
			}
			rValStr[lIdx] <- paste( valStr ,collapse=" " )
		}
		FLogStr(sprintf("    [%3d] %s",rIdx,paste( rValStr ,collapse="    ")))
	}
	FLogStr("    anaMtx_ColVal( cvSeqNextLst[[1]]$fndMtx )")
	FLogStr("    anaMtx_ColVal( cvSeqNextLst[[2]]$fndMtx )")
	FLogStr("    anaMtx_ColVal( cvSeqNextLst[[3]]$fndMtx )")
	FLogStr("    anaMtx_ColVal( cvSeqNextLst[[4]]$fndMtx )")
	FLogStr("    anaMtx_ColVal( cvSeqNextLst[[5]]$fndMtx )")
	FLogStr("                         ")

	colSize <- 3
	FLogStr("# =========================================================")
	FLogStr(sprintf("# colValSeqNext( ,pColSize=%d )",colSize))
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=colSize )
	lstSize <- length(cvSeqNextLst)
	fndSize <- sapply( cvSeqNextLst ,function(cv){nrow(cv$fndMtx)})
	for( lIdx in 1:lstSize ){
		rptStr <- rptSameRow( cvSeqNextLst[[lIdx]]$fndMtx )
		FLogStr(sprintf("    <%d> %s",lIdx,rptStr))
	}

	for( rIdx in seq_len(max(fndSize)) ){
		rValStr <- NULL
		for( lIdx in 1:lstSize ){
			valStr <- NULL
			for( cIdx in 1:colSize ){
				if( rIdx>fndSize[lIdx] ) { valStr[cIdx] <- "  "
				} else { valStr[cIdx] <- sprintf("%2d",cvSeqNextLst[[lIdx]]$fndMtx[rIdx,cIdx])	}
			}
			rValStr[lIdx] <- paste( valStr ,collapse=" " )
		}
		FLogStr(sprintf("    [%3d] %s",rIdx,paste( rValStr ,collapse="    ")))
	}
	FLogStr("                         ")


	FLogStr("# =========================================================")
	FLogStr("#	cStep                                                  =")
	FLogStr("# =========================================================")
	FLogStr("                                                           ")

	zMtx <- t( apply(gEnv$zhF ,1 ,function(zoid){zoid[2:6]-zoid[1:5]}) )

	FLogStr("# =========================================================")
	FLogStr("# anaColEndPtn()")
	colPtnLst <- anaColEndPtn( zMtx ,pDebug=T )
	for( idx in 1:length(colPtnLst) ){
		valStr <- sprintf("%2d",colPtnLst[[idx]]$val)

		rebFlag <- FALSE
		if( 1<length(valStr) ){
			rebFlag <- any( valStr[2:length(valStr)]==valStr[1:(length(valStr)-1)] )
		}

		valStr <- paste( valStr ,collapse=" ")
		FLogStr(sprintf("    [%d]%s  %s",idx,ifelse(rebFlag,"*"," "),valStr))
	}
	FLogStr("                         ")

	colSize <- 2
	FLogStr("# =========================================================")
	FLogStr(sprintf("# colValSeqNext( ,pColSize=%d )",colSize))
	cvSeqNextLst <- colValSeqNext( zMtx ,pColSize=colSize )
	lstSize <- length(cvSeqNextLst)
	fndSize <- sapply( cvSeqNextLst ,function(cv){nrow(cv$fndMtx)})
	for( lIdx in 1:lstSize ){
		rptStr <- rptSameRow( cvSeqNextLst[[lIdx]]$fndMtx )
		FLogStr(sprintf("    <%d> %s",lIdx,rptStr))
	}

	for( rIdx in seq_len(max(fndSize)) ){
		rValStr <- NULL
		for( lIdx in 1:lstSize ){
			valStr <- NULL
			for( cIdx in 1:colSize ){
				if( rIdx>fndSize[lIdx] ) { valStr[cIdx] <- "  "
				} else { valStr[cIdx] <- sprintf("%2d",cvSeqNextLst[[lIdx]]$fndMtx[rIdx,cIdx])	}
			}
			rValStr[lIdx] <- paste( valStr ,collapse=" " )
		}
		FLogStr(sprintf("    [%3d] %s",rIdx,paste( rValStr ,collapse="    ")))
	}
	FLogStr("                         ")

	colSize <- 3
	FLogStr("# =========================================================")
	FLogStr(sprintf("# colValSeqNext( ,pColSize=%d )",colSize))
	cvSeqNextLst <- colValSeqNext( zMtx ,pColSize=colSize )
	lstSize <- length(cvSeqNextLst)
	fndSize <- sapply( cvSeqNextLst ,function(cv){nrow(cv$fndMtx)})
	for( lIdx in 1:lstSize ){
		rptStr <- rptSameRow( cvSeqNextLst[[lIdx]]$fndMtx )
		FLogStr(sprintf("    <%d> %s",lIdx,rptStr))
	}

	for( rIdx in seq_len(max(fndSize)) ){
		rValStr <- NULL
		for( lIdx in 1:lstSize ){
			valStr <- NULL
			for( cIdx in 1:colSize ){
				if( rIdx>fndSize[lIdx] ) { valStr[cIdx] <- "  "
				} else { valStr[cIdx] <- sprintf("%2d",cvSeqNextLst[[lIdx]]$fndMtx[rIdx,cIdx])	}
			}
			rValStr[lIdx] <- paste( valStr ,collapse=" " )
		}
		FLogStr(sprintf("    [%3d] %s",rIdx,paste( rValStr ,collapse="    ")))
	}
	FLogStr("                         ")



	return( fileName )

} # fCutU.rptColValSeqNext( )

fCutU.chkRowPtnReb <- function( quoMtx ){
	# quotion값에서 2연속으로 동일 컬럼 동일 값 나타나는 것은 2개 미만이다.
	#	하지만 다른 곳에서도 응용할 수 있을 듯.

	rObj <- list( fnd=FALSE ,rebNum=0 )
	mtxLen <- nrow(quoMtx)

	matFlagMtx <- matrix( F ,nrow=3 ,ncol=ncol(quoMtx) )
	matValMtx <- matrix( NA ,nrow=3 ,ncol=ncol(quoMtx) )
	rownames(matFlagMtx) <- c("0-1","1-3","2-5")
	rownames(matValMtx) <- rownames(matFlagMtx)

	if( 2<=mtxLen ){
		rn <- "0-1" 	# row name
		matFlagMtx[rn,] <- quoMtx[(mtxLen-0),]==quoMtx[(mtxLen-1),]
		matValMtx[ rn ,matFlagMtx[rn,] ] <- quoMtx[(mtxLen-0) ,matFlagMtx[rn,] ]
	}
	if( 4<=mtxLen ){
		rn <- "1-3" 	# row name
		matFlagMtx[rn,] <- quoMtx[(mtxLen-1),]==quoMtx[(mtxLen-3),]
		matValMtx[ rn ,matFlagMtx[rn,] ] <- quoMtx[(mtxLen-1) ,matFlagMtx[rn,] ]
	}
	if( 6<=mtxLen ){
		rn <- "2-5" 	# row name
		matFlagMtx[rn,] <- quoMtx[(mtxLen-2),]==quoMtx[(mtxLen-5),]
		matValMtx[ rn ,matFlagMtx[rn,] ] <- quoMtx[(mtxLen-2) ,matFlagMtx[rn,] ]
	}
	rObj$matFlagMtx <- matFlagMtx
	rObj$matValMtx <- matValMtx

	rObj$filt <- function( quoVal ){	# quoVal <- c(2,1,1,2,3)
			matMtx <- matrix(F,nrow=nrow(rObj$matValMtx),ncol=ncol(rObj$matValMtx))
			rownames(matMtx) <- rownames(rObj$matValMtx)
			for( rIdx in 1:nrow(rObj$matValMtx) ){
				idx <- which( quoVal==rObj$matValMtx[rIdx,] )
				matMtx[rIdx,idx] <- TRUE
			}
			thld <- apply(rObj$matFlagMtx,1,sum)-1
			thld[thld<1] <- 1
			
			fltObj <- list( matMtx=matMtx )
			fltObj$filted <- FALSE
			for( rIdx in 1:nrow(matMtx) ){
				if( thld[rIdx] < sum(matMtx[rIdx,]) ){
					fltObj$filted <- TRUE
					break
				}
			}	
			if( !fltObj$filted ){
				fltObj$filted <- any(apply(matMtx,2,sum)>2)
			}
			return(fltObj)
		}

	return( rObj )

} # fCutU.chkRowPtnReb()

fCutU.getSpanMatchObj <- function( rawTail ,rpt=FALSE ){
	# rawTail <- stdMI$rawTail

	rObj <- list()

	dataSize <- nrow(rawTail)
	if( 2>dataSize ){
		rObj$matchCnt <- function( aZoid ){ return(0) }
		return( rObj )
	}
	
    vals <- sort(unique(as.vector(rawTail)))
    freqVals <- vals[ table(rawTail)>1 ]

	fndLst <- list()
	fndInfo.name <- c("fVal","srchRow","posDiff","1st.rIdx","2nd.rIdx")
    for( fVal in freqVals ){
		zoid1st <- NULL	;zoid1st.rIdx <- 0
		zoid2nd <- NULL	;zoid2nd.rIdx <- 0
		for( rIdx in nrow(rawTail):1 ){
			if( !any(rawTail[rIdx,]==fVal) ) next
			
			if( is.null(zoid2nd) ){
				zoid2nd <- rawTail[rIdx,]
				zoid2nd.rIdx <- rIdx
			} else {
				zoid1st <- rawTail[rIdx,]
				zoid1st.rIdx <- rIdx
				break
			}
		}	# rIdx

		srchRow <- zoid1st.rIdx + 1 + (dataSize-zoid2nd.rIdx)
		posDiff <- which( zoid2nd==fVal ) - which( zoid1st==fVal )
		fndInfo <- c( fVal, srchRow, posDiff ,zoid1st.rIdx ,zoid2nd.rIdx )
		names(fndInfo) <- fndInfo.name
		fndLst[[sprintf("fv:%2d",fVal)]] <- fndInfo
    }

	fndMtx <- do.call( rbind ,fndLst )
	if( rpt ){
		mtxStr <- sprintf("   %s",capture.output(fndMtx))
		cat( sprintf("%s\n",paste(mtxStr,collapse="\n")) )
	}

	srchLst <- list()
	srchLst[[1]] <- fndMtx[1,c("srchRow","posDiff")]
	if( 1 < nrow(fndMtx) ){
		for( rIdx in 2:nrow(fndMtx) ){
			fndVal <- fndMtx[rIdx,c("srchRow","posDiff")]
			matFlag <- apply( fndMtx[1:(rIdx-1),c("srchRow","posDiff"),drop=F] ,1 
								,function(val){ all(fndVal==val) }
							)

			if( all(!matFlag) ){
				srchLst[[1+length(srchLst)]] <- fndVal
			}
		}
	}

	rObj$srchLst <- srchLst
	rObj$rawTail <- rawTail
	rObj$matchCnt <- function( aZoid ){
		score <- sapply( rObj$srchLst ,function( srchInfo ){
						mCnt <- fCutU.spanMatch( rObj$rawTail[ srchInfo["srchRow"] ,]
										,aZoid
										,posDiff = srchInfo["posDiff"]
									)
						return( mCnt )
					})
		return( sum(score) )
	}

	return( rObj )

} # fCutU.getSpanMatchObj()

fCutU.getChkCStepValReb <- function( pMtx ){
	#	현재의 cStep 코드(파편)이 이전에도 같은 값으로 인해 나온 파편일까?
	#		gEnv$zhF[820,1:3], gEnv$zhF[821,1:3] 은 cStep 파편은 같으나 값이 다르다.

	cSize <- ncol(pMtx)
	rSize <- nrow(pMtx)

	rObj <- list(  )
	cStepMtx <- pMtx[,2:cSize,drop=F] - pMtx[,1:(cSize-1),drop=F]

	hpnLst <- list()
	if( rSize > 1 ){
		for( rIdx in rSize:2 ){
			# hpnLst에 기록이 있으면 skip
			if( 0<length(hpnLst) ){
				existF <- sapply( hpnLst ,function(hpnObj){ all(hpnObj$cStep==cStepMtx[rIdx,]) } )
				if( any(existF) ) next
			}

			hpnObj <- list( cStep=cStepMtx[rIdx,] ,raw=pMtx[rIdx,] )
			hpnLst[[paste(sprintf("%2d",hpnObj$cStep),collapse=",")]] <- hpnObj
		}
	}
	rObj$hpnLst <- hpnLst

	rObj$match <- function( aCodeMtx ){
		cSize <- ncol(aCodeMtx)
		flag <- apply( aCodeMtx ,1 ,function( aCode ){
					aCStep <- aCode[2:cSize] - aCode[1:(cSize-1)]
					for( idx in seq_len(length(rObj$hpnLst)) ){
						hpnObj <- rObj$hpnLst[[idx]]
						if( all(hpnObj$cStep==aCStep) && all(hpnObj$raw==aCode) ){
							return( TRUE )
						}
					}
					return( FALSE )
				})
		return( flag )
	} # rObj$match( )

	return( rObj )

} # fCutU.chkCStepValReb()


fCutU.getChkNextPtn4FV.cStep <- function( rawTail ,pDebug=FALSE ){
	# rObj <- fCutU.getChkNextPtn4FV.cStep( stdMI$rawTail ,pDebug=T )

	rObj <- list()
	rObj$cMtx <- t( apply( rawTail ,1 ,function(pRaw){ pRaw[2:6]-pRaw[1:5] }) )

	if( 2>nrow(rObj$cMtx) ){
		rObj$check <- function( aCStep ){ return( list(flgCnt=0 ,matchCnt=0 ) ) }
		return( rObj )
	}

	datLen <- nrow(rObj$cMtx)
	lastCode <- rObj$cMtx[datLen,]
	availLst <- fCutU.getChkNextPtn4FV.u.availLst( lastCode )	# avail col span

	rObj$fltLst <- list()
	for( cIdx in 1:5 ){
		tgtSpanDF <- availLst[[cIdx]]$spanDF
		for( rIdx in (datLen-1):1 ){
			fndIdx <- which( rObj$cMtx[rIdx,] == lastCode[cIdx] )
			fndIdx <- fndIdx[order( abs(fndIdx-cIdx) )]

			fndFlag <- FALSE
			for( fIdx in fndIdx ){
				rebSpanDF <- availLst[[fIdx]]$spanDF
				banValLst <- lapply( 1:nrow(rebSpanDF) ,function(sdIdx){
												banColSpan <- (1:rebSpanDF[sdIdx,"spanLen"]+rebSpanDF[sdIdx,"offset"] )
												banVal <- rObj$cMtx[rIdx+1,banColSpan]
												return(banVal)
											} )
				matchInfo <- fCutU.getChkNextPtn4FV.u.matchSpan( tgtSpanDF ,rebSpanDF ,banValLst )
				if( !is.null(matchInfo) ){
					fltObj <- list( dbgStr=sprintf("val:%d(col:%d)      last reb:%d,%d",lastCode[cIdx],cIdx,rIdx,fIdx)
									,chkSpan=matchInfo$chkSpan ,banVal=matchInfo$banVal )
					fltObj$matchInfo <- matchInfo
					rObj$fltLst[[sprintf("col%d(%2d)",cIdx,lastCode[cIdx])]] <- fltObj
					fndFlag <- TRUE
					break
				}
			}

			if(fndFlag){
				break
			}
		} # for(rIdx)
	}
	
	rObj$check <- function( aCStep ){
			matchCntBuffer <- rep( 0 ,length(rObj$fltLst) )
			for( fIdx in seq_len(length(rObj$fltLst)) ){
				matchCntBuffer[fIdx] <- sum(rObj$fltLst[[fIdx]]$banVal==aCStep[rObj$fltLst[[fIdx]]$chkSpan])
			}
			return( list( flgCnt=sum(matchCntBuffer>=2) ,matchCnt=matchCntBuffer ) )
		} # rObj$check()

	if( pDebug ){
		cat( sprintf("    %s",capture.output(rObj$cMtx)), sep="\n" ) 
		cat("---------------------------------------------------", sep="\n") 
		for( lIdx in seq_len(length(rObj$fltLst)) ){
			flt <- rObj$fltLst[[lIdx]]
			cat(sprintf("%dth flter     %s \n" ,lIdx ,flt$dbgStr ))
			cat(sprintf("   ban Val %s\n", paste(flt$banVal ,collapse=" " ) ))
			cat(sprintf("   tgt DF %s (%s)\n", flt$matchInfo$tgt.desc,paste(flt$chkSpan,collapse=" ") ))
			cat(sprintf("   reb DF %s \n", flt$matchInfo$reb.desc ))
		}
	}

	return( rObj )

} # fCutU.getChkNextPtn4FV.cStep()

fCutU.getChkNextPtn4FV.fStep <- function( rawTail ,pDebug=FALSE ){
	# rObj <- fCutU.getChkNextPtn4FV.fStep( stdMI$rawTail ,pDebug=T )

	rObj <- list()
	if( (2+1) > nrow(rawTail) ){
		rObj$check <- function( aFStep ){ return( list(flgCnt=0 ,matchCnt=0 ) ) }
		return( rObj )
	}

	# rObj$cMtx <- t( apply( rawTail ,1 ,function(pRaw){ pRaw[2:6]-pRaw[1:5] }) )
	rObj$fMtx <- apply( rawTail ,2 ,function(cVal){ cVal[2:length(cVal)]-cVal[1:(length(cVal)-1)] })


	datLen <- nrow(rObj$fMtx)
	lastCode <- rObj$fMtx[datLen,]
	availLst <- fCutU.getChkNextPtn4FV.u.availLst( lastCode )	# avail col span

	rObj$fltLst <- list()
	for( cIdx in 1:6 ){
		tgtSpanDF <- availLst[[cIdx]]$spanDF

		fndFlag <- FALSE
		for( rIdx in (datLen-1):1 ){
			fndIdx <- which( rObj$fMtx[rIdx,] == lastCode[cIdx] )
			fndIdx <- fndIdx[order( abs(fndIdx-cIdx) )]

			for( fIdx in fndIdx ){
				rebSpanDF <- availLst[[fIdx]]$spanDF
				banValLst <- lapply( 1:nrow(rebSpanDF) ,function(sdIdx){
												banColSpan <- (1:rebSpanDF[sdIdx,"spanLen"]+rebSpanDF[sdIdx,"offset"] )
												banVal <- rObj$fMtx[rIdx+1,banColSpan]
												return(banVal)
											} )
				matchInfo <- fCutU.getChkNextPtn4FV.u.matchSpan( tgtSpanDF ,rebSpanDF ,banValLst )
				if( !is.null(matchInfo) ){
					fltObj <- list( dbgStr=sprintf("val:%d(col:%d)      last reb:%d,%d",lastCode[cIdx],cIdx,rIdx,fIdx)
									,chkSpan=matchInfo$chkSpan ,banVal=matchInfo$banVal )
					fltObj$matchInfo <- matchInfo
					rObj$fltLst[[sprintf("col%d(%2d)",cIdx,lastCode[cIdx])]] <- fltObj
					fndFlag <- TRUE
					break
				}
			}

			if(fndFlag){
				break
			}
		} # for(rIdx)
		if( fndFlag ){
			next
		}

		# 동일 값이 없으면 +,- 반대값으로 검색
		for( rIdx in (datLen-1):1 ){
			fndIdx <- which( rObj$fMtx[rIdx,] == -lastCode[cIdx] )
			fndIdx <- fndIdx[order( abs(fndIdx-cIdx) )]

			for( fIdx in fndIdx ){
				rebSpanDF <- availLst[[fIdx]]$spanDF
				banValLst <- lapply( 1:nrow(rebSpanDF) ,function(sdIdx){
												banColSpan <- (1:rebSpanDF[sdIdx,"spanLen"]+rebSpanDF[sdIdx,"offset"] )
												banVal <- -rObj$fMtx[rIdx+1,banColSpan]
												return(banVal)
											} )
				matchInfo <- fCutU.getChkNextPtn4FV.u.matchSpan( tgtSpanDF ,rebSpanDF ,banValLst )
				if( !is.null(matchInfo) ){
					fltObj <- list( dbgStr=sprintf("val:%d(col:%d)      last reb:%d,%d  (reverse)",lastCode[cIdx],cIdx,rIdx,fIdx)
									,chkSpan=matchInfo$chkSpan ,banVal=matchInfo$banVal )
					fltObj$matchInfo <- matchInfo
					rObj$fltLst[[sprintf("col%d(%2d)",cIdx,lastCode[cIdx])]] <- fltObj
					fndFlag <- TRUE
					break
				}
			}

			if(fndFlag){
				break
			}
		} # for(rIdx)

	}
	
	rObj$check <- function( aCStep ){
			matchCntBuffer <- rep( 0 ,length(rObj$fltLst) )
			for( fIdx in seq_len(length(rObj$fltLst)) ){
				matchCntBuffer[fIdx] <- sum(rObj$fltLst[[fIdx]]$banVal==aCStep[rObj$fltLst[[fIdx]]$chkSpan])
			}
			return( list( flgCnt=sum(matchCntBuffer>=2) ,matchCnt=matchCntBuffer ) )
		} # rObj$check()

	if( pDebug ){
		cat( sprintf("    %s",capture.output(rObj$fMtx)), sep="\n" ) 
		cat("---------------------------------------------------", sep="\n") 
		for( lIdx in seq_len(length(rObj$fltLst)) ){
			flt <- rObj$fltLst[[lIdx]]
			cat(sprintf("%dth flter     %s \n" ,lIdx ,flt$dbgStr ))
			cat(sprintf("   ban Val %s\n", paste(flt$banVal ,collapse=" " ) ))
			cat(sprintf("   tgt DF %s (%s)\n", flt$matchInfo$tgt.desc,paste(flt$chkSpan,collapse=" ") ))
			cat(sprintf("   reb DF %s \n", flt$matchInfo$reb.desc ))
		}
	}

	return( rObj )

} # fCutU.getChkNextPtn4FV.fStep()


fCutU.getStdMI <- function( gEnv ){
	

	stdMILst <- list()

	zMtx <- gEnv$zhF
	stdMILst[["basic"]] <- fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextZW( gEnv )$zMtx
	stdMILst[["nextZW"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextQuo10( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextQuo10"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextBin"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	rebObj <- fCutU.getNextRebNumPtn( gEnv ,numPtn=NULL )
	zMtx <- rebObj$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextRebNum"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextCStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextCStepBin"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextFStepBin( gEnv )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextFStepBin"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,1 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextColVal_1"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,2 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextColVal_2"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,3 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextColVal_3"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,4 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextColVal_4"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,5 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextColVal_5"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )

	zMtx <- fCutU.getNextColVal( gEnv ,6 )$zMtx	# rptObj<-anaQuoTbl( zMtx )
	stdMILst[["nextColVal_6"]] <- if( 0==nrow(zMtx) ) NULL else fCutU.getMtxInfo( zMtx )


	return( stdMILst )
}

#	utility function for fCutU.getChkNextPtn4FV.xxxx()
fCutU.getChkNextPtn4FV.u.matchSpan <- function( tgtSpanDF ,rebSpanDF ,banValLst ){

	matchInfo <- NULL
	for( tgtIdx in 1:nrow(tgtSpanDF) ){	# 전체 매치 체크.
		tgtSpan <- tgtSpanDF[tgtIdx,"startIdx"]:tgtSpanDF[tgtIdx,"endIdx"]
		rebIdx.match <- NA
		for( rebIdx in 1:nrow(rebSpanDF) ){
			rebSpan <- rebSpanDF[rebIdx,"startIdx"]:rebSpanDF[rebIdx,"endIdx"]
			if( length(tgtSpan)!=length(rebSpan) ) next

			if( all(tgtSpan==rebSpan) ){
				rebIdx.match <- rebIdx
				break
			}
		}

		if( !is.na(rebIdx.match) ){
			matchInfo <- list( chkSpan = 1:tgtSpanDF[tgtIdx,"spanLen"]+tgtSpanDF[tgtIdx,"offset"] )
			matchInfo$banVal <- banValLst[[rebIdx.match]]
			matchInfo$tgt.desc <- sprintf("tgtIdx:%d    %s",tgtIdx,tgtSpanDF[tgtIdx,"description"])
			matchInfo$reb.desc <- sprintf("rebIdx:%d    %s",rebIdx.match,rebSpanDF[rebIdx.match,"description"])
			break
		}
	}
	if( !is.null(matchInfo) ){
		return( matchInfo )
	}

	# 3개 짜리 매치는 찾지 못했으면 상대방에 대해서 2개 짜리 매치라도..
	for( tgtIdx in 1:nrow(tgtSpanDF) ){
		tgtSpan <- tgtSpanDF[tgtIdx,"startIdx"]:tgtSpanDF[tgtIdx,"endIdx"]
		rebIdx.match <- NA
		newSpan.tgt <- NULL	;newSpan.reb <- NULL
		for( rebIdx in 1:nrow(rebSpanDF) ){
			rebSpan <- rebSpanDF[rebIdx,"startIdx"]:rebSpanDF[rebIdx,"endIdx"]

			if( 2==sum(rebSpan%in%tgtSpan) ){	# 0:1 또는 -1:0
				rebIdx.match <- rebIdx
				newSpan.tgt <- tgtSpan[tgtSpan%in%rebSpan]
				newSpan.reb <- rebSpan[rebSpan%in%tgtSpan]
				break
			}
		}

		# 	fCutCnt.nextZW     1th flter     col:1 val:5 lastIdx:5,4 
		if( !is.na(rebIdx.match) ){
			matchInfo <- list( )

			banVal.idx <- rebSpanDF[rebIdx.match,"startIdx"]:rebSpanDF[rebIdx.match,"endIdx"]
			banVal <- banValLst[[rebIdx.match]]
			banVal <- banVal[banVal.idx %in% newSpan.reb]

			# 참고 : newSpan.tgt와 newSpan.reb 모두 증,감의 방향은 동일할 수 밖에 없다
			# 로직보단 차라리 하드코딩이 알아보기 쉽겠다.... -_-;
			if( all(newSpan.tgt==c(0,1)) ){	
				matchInfo$chkSpan <- 0:1 + tgtSpanDF[tgtIdx,"cIdx"]
				matchInfo$banVal <- banVal
				matchInfo$tgt.desc <- sprintf("tgtIdx:%d    1:2+%d",tgtIdx ,tgtSpanDF[tgtIdx,"cIdx"]-1 )
				matchInfo$reb.desc <- sprintf("rebIdx:%d    1:2+%d",rebIdx.match ,rebSpanDF[rebIdx.match,"cIdx"]-1 )
			} else if( all(newSpan.tgt==c(-1,0)) ){
				matchInfo$chkSpan <- -1:0 + tgtSpanDF[tgtIdx,"cIdx"]
				matchInfo$banVal <- banVal
				matchInfo$tgt.desc <- sprintf("tgtIdx:%d    1:2+%d",tgtIdx ,tgtSpanDF[tgtIdx,"cIdx"]-2 )
				matchInfo$reb.desc <- sprintf("rebIdx:%d    1:2+%d",rebIdx.match ,rebSpanDF[rebIdx.match,"cIdx"]-2 )
			}
			break
		}

	} # for(tgtIdx)

	return( matchInfo )
} # fCutU.getChkNextPtn4FV.u.matchSpan()

#	utility function for fCutU.getChkNextPtn4FV.xxxx()
fCutU.getChkNextPtn4FV.u.availLst <- function( lastCode ){

	len <- length( lastCode )
	availLst <- list()
	for( cIdx in 1:len ){
		availObj <- list( val=lastCode[cIdx] )
		spanDF <- data.frame( cIdx=integer(0), spanLen=integer(0) ,offset=integer(0) 
								,startIdx=integer(0) ,endIdx=integer(0)
								,description=character(0) 
							)
		if( (1<cIdx) && (cIdx<len) ){	# -1:1
			newDF <- data.frame( cIdx=cIdx ,spanLen=3 ,offset=(cIdx-2) 
						,startIdx=-1	,endIdx=1
						,description=sprintf("1:3+%d middle",(cIdx-2)) )
			spanDF <- rbind( spanDF ,newDF )
		}
		if( cIdx<(len-1) ){	# to right side. 0:2
			newDF <- data.frame( cIdx=cIdx ,spanLen=3 ,offset=(cIdx-1) 
						,startIdx=0	,endIdx=2
						,description=sprintf("1:3+%d right",(cIdx-1)) )
			spanDF <- rbind( spanDF ,newDF )
		} else if( cIdx==(len-1) ){
			newDF <- data.frame( cIdx=cIdx ,spanLen=2 ,offset=(cIdx-1) 
						,startIdx=0	,endIdx=1
						,description=sprintf("1:2+%d right",(cIdx-1)) )
			spanDF <- rbind( spanDF ,newDF )
		}
		if( 2<cIdx ){	# to left side
			newDF <- data.frame( cIdx=cIdx ,spanLen=3 ,offset=(cIdx-3) 
						,startIdx=-2	,endIdx=0
						,description=sprintf("1:3+%d left",(cIdx-3)) )
			spanDF <- rbind( spanDF ,newDF )
		} else if( 2==cIdx ){
			newDF <- data.frame( cIdx=cIdx ,spanLen=2 ,offset=(cIdx-2) 
						,startIdx=-1	,endIdx=0
						,description=sprintf("1:2+%d left",(cIdx-2)) )
			spanDF <- rbind( spanDF ,newDF )
		}
		spanDF <- spanDF[ order(spanDF$spanLen,decreasing=T) ,]
		availObj$spanDF <- spanDF
		availLst[[cIdx]] <- availObj
	}

	return( availLst )

} # fCutU.getChkNextPtn4FV.u.availLst

fCutU.getNextSeq <- function( pMtx ){

	lenCol <- ncol(pMtx)	;lenRow <- nrow(pMtx)

	rObj <- list()
	if( 2>lenRow ){
		rObj$filt <- function( aCode ){	return( 0 )	}
		return( rObj )
	}

	palette <- unique(pMtx[lenRow,])
	palColLst <- vector("list",length(palette))	;names(palColLst) <- palette
	for( idx in 1:length(palette) ){
		color <- NA
		for( rIdx in (lenRow-1):1 ){
			fIdx <- which( pMtx[rIdx,]==palette[idx] )
			if( 0<length(fIdx) ){
				color <- unique(pMtx[(rIdx+1),fIdx])
				break
			}
		}
		palColLst[[idx]] <- color
	}

	checkMtx <- matrix( NA ,nrow=1 ,ncol=lenCol )	;colnames(checkMtx) <- pMtx[lenRow,]
	for( idx in 1:length(palette) ){
		palCol <- palColLst[[idx]]
		colFlag <- colnames(checkMtx) == as.character(palette[idx]) 
		checkMtx[ ,colFlag ] <- palColLst[[idx]][1]
		if( 1<length(palCol) ){
			for( dIdx in 2:length(palCol) ){
				tmpCheckMtx <- checkMtx
				tmpCheckMtx[ ,colFlag ] <- palColLst[[idx]][dIdx]
				checkMtx <- rbind( checkMtx, tmpCheckMtx )
			}
		}
		
	}

	rObj$checkMtx <- checkMtx
	rObj$filt <- function( aCode ){
		matCnt <- apply( rObj$checkMtx ,1 ,function(checkVal){ sum(checkVal==aCode,na.rm=T ) } )
		return( max(matCnt) )
	} # rObj$filt()

	return( rObj )

}	# fCutU.getNextSeq()


fCutU.getCntMtxObj <- function( stdMI ){

	pMtx <- stdMI$cStepTail
	#	pMtx[3,1]<-6	;pMtx[3,3]<-6	;pMtx[1,2]<-6	;pMtx[1,4:5]<-c(6,3)	;pMtx[6,3] <- 2
		# Z842
		# 836  8  6  3  6  3
		# 837 23  3  2  3 12
		# 838  6  3  1  8  2
		# 839  6  2  1  1  4
		# 840  2  7 17  1 14
		# 841  6  3  2  3  5


} # fCutU.getCntMtxObj()


fCutU.getFiltObjPair <- function( pMtx ,debug=F ){

	getOverlapSpan <- function( a1, a2, aLen, nCol ){
		#	a1.row < a2.row
		if( nCol < (max(a1,a2)+aLen-1) ) return(NULL)

		a1.span <- NULL		;a2.span <- NULL
		a1.fixIdx<- NULL	;a2.fixIdx<-NULL
		if( a1 >= a2 ){
			leftMargin <- a2-1
			rightMargin <- nCol - (a1+aLen-1)
			a1.span <- (a1-leftMargin):nCol
			a2.span <- 1:(a2+aLen-1 + rightMargin)
		} else {
			leftMargin <- a1-1
			rightMargin <- nCol - (a2+aLen-1)
			a1.span <- 1:(a1+aLen-1 +rightMargin)
			a2.span <- (a2-leftMargin):nCol
		}

		return( list(a1.span=a1.span ,a2.span=a2.span ,fixIdx=(1:aLen+leftMargin) ) )
	}
	scanStream <- function( vals ,infoDf ){
		vals.len <- length(vals)	# 2 이상 보장

		ptnDf <- NULL

		fStep <- vals[1:(vals.len-1)] - vals[2:vals.len]
		if( 1==abs(fStep[1]) ){	# code="fStep1"		3,4,5
			df <- data.frame( banVal=vals[1]+fStep[1] ,code="fStep.1" ,multi=F ,code.sub=NA ,desc="2?,3,4,5" )
			if( 1<length(fStep) && fStep[1]==fStep[2] )	df$multi=T	# 다수발생 여부.

			df <- cbind( df, infoDf )
			ptnDf <- rbind(ptnDf,df)

		} else if( 1<length(fStep) && (fStep[1]==fStep[2]) ){	#code="fStep.n"		3, 5, 7
			# n 중가가 2번 이상.
			df <- data.frame( banVal=vals[1]+fStep[1] ,code="fStep.n" ,multi=F ,code.sub=NA ,desc="1?,3,5,7" )
			if( 2<length(fStep) && fStep[2]==fStep[3] )	df$multi=T	# 다수발생 여부.

			df <- cbind( df, infoDf )
			ptnDf <- rbind(ptnDf,df)
		}

		if( 2<vals.len && (0==fStep[1]) && (0!=fStep[2]) ){	# code="bRebind"	4, 4, 5
			df <- data.frame( banVal=vals[3] ,code="bRebind" ,multi=F ,code.sub=NA ,desc="5?,4,4,5")
			df <- cbind( df, infoDf )
			ptnDf <- rbind(ptnDf,df)
		}

		return( ptnDf )
	}
	removeDup <- function( df ,fixCol=NULL ){
		rSize <- nrow(df)
		if( 2>rSize ) return( df )

		if( is.null(fixCol) )	fixCol <- 1:ncol(df)

		surFlag <- rep( T ,rSize )
		for( idx1 in 1:(rSize-1) ){
			if( !surFlag[idx1] )	next

			for( idx2 in (idx1+1):rSize ){
				matchFlag <- df[idx1,fixCol]==df[idx2,fixCol]
				if( any(is.na(matchFlag)) ){
					naIdx <- which(is.na(matchFlag))
					if( all(matchFlag[-naIdx]) && all(is.na(df[idx1,naIdx])) && all(is.na(df[idx2,naIdx])) ){
						surFlag[idx2] <- F
					}
				} else if( all(matchFlag) ){
					surFlag[idx2] <- F
				}
			}
		}

		return( df[surFlag,,drop=F] )
	}

	rObj <- list( pBanInfoLst=list() ,iBanInfoLst=list() ,pairPtnLst=list() ,ptn4Lst=list() )	# for self-descriptive
	rObj$findPtn <- function( aCode ,ptn ){	# 발견 위치 idx 반환. fCutU.hasPtn()는 존재 여부만 판단할 뿐이라..
		aLen <- length(aCode)	;pLen <- length(ptn)
		fndMtx <- matrix( 0, nrow=0, ncol=pLen )
		for( idx in 1:(aLen-pLen+1)){
			scanSpan <- idx + 0:(pLen-1)
			if( all(aCode[scanSpan]==ptn) ){
				fndMtx <- rbind( fndMtx , scanSpan )
			}
		}
		return( fndMtx )
	}
	rObj$explain <- function( part=NULL ){
		rptStr <- NULL
		if( is.null(part) || part=="pBanInfoLst" ){
			rptStr <- c( rptStr ,"pBanInfoLst")
			infoStr <- sapply( rObj$pBanInfoLst ,function( banInfo ){
				pairInfo <- banInfo$pairInfo
				sprintf("val:%s   cord:(%d/%s),(%d/%s) happen:%d   incPtn:%s(fix:%s)  rebPtn:%s(fix:%s)" 
					,paste(pairInfo[c("v1","v2")],collapse=",")
					,pairInfo["rf"],paste(pairInfo[c("cf1","cf2")],collapse=",")
					,pairInfo["rs"],paste(pairInfo[c("cs1","cs2")],collapse=",")
					,pairInfo["hpn"]
					,paste(banInfo$incPtn.banVal,collapse=",")	,paste(banInfo$incPtn.fixIdx,collapse=",")
					,paste(banInfo$rebPtn.banVal,collapse=",")	,paste(banInfo$rebPtn.fixIdx,collapse=",")
				)
			})
			if( 0<length(infoStr) ){
				infoStr <- paste( sprintf("  %dth %s",1:length(infoStr),infoStr) ,collapse="\n")
				rptStr <- c( rptStr ,infoStr )
			}
		}
		if( is.null(part) || part=="iBanInfoLst" ){
			rptStr <- c( rptStr ,"iBanInfoLst")
			infoStr <- sapply( rObj$iBanInfoLst ,function( banInfo ){
				incInfoDf <- banInfo$incInfoDf
				sprintf("<%s> banVal:%s(fix:%s) multiHpn:%s cord:%s val:%s" 
					,banInfo$typ
					,paste(banInfo$incPtn.banVal,collapse=",")
					,paste(banInfo$incPtn.fixIdx,collapse=",")
					,banInfo$multiHpn
					,incInfoDf[1,"cordStr"]	,incInfoDf[1,"valStr"]
				)
			})
			if( 0<length(infoStr) ){
				infoStr <- paste( sprintf("  %dth %s",1:length(infoStr),infoStr) ,collapse="\n")
				rptStr <- c( rptStr ,infoStr )
			}
		}
		if( is.null(part) || part=="pairPtnLst" ){
			rptStr <- c( rptStr ,"pairPtnLst")
			infoStr <- sapply(	rObj$pairPtnLst[["(pFV,*)"]] ,function( mtx ){
				str <- NULL
				for( rIdx in seq_len(nrow(mtx)) ){
					str <- c( str ,sprintf("(%s)",paste(mtx[rIdx,],collapse=",")) )
				}
				return( ifelse(is.null(str),"",paste(str,collapse="") ) )
			})
			if( 0<length(infoStr) ){
				infoStr <- paste( sprintf("  (pFV,*) %dth gen %s",1:length(infoStr),infoStr) ,collapse="\n")
				rptStr <- c( rptStr ,infoStr )
			}

			infoStr <- sapply(	rObj$pairPtnLst[["(*,pFV)"]] ,function( mtx ){
				str <- NULL
				for( rIdx in seq_len(nrow(mtx)) ){
					str <- c( str ,sprintf("(%s)",paste(mtx[rIdx,],collapse=",")) )
				}
				return( ifelse(is.null(str),"",paste(str,collapse="") ) )
			})
			if( 0<length(infoStr) ){
				infoStr <- paste( sprintf("  (*,pFV) %dth gen %s",1:length(infoStr),infoStr) ,collapse="\n")
				rptStr <- c( rptStr ,infoStr )
			}
		}
		if( is.null(part) || part=="ptn4Lst" ){
			rptStr <- c( rptStr ,"ptn4Lst")
			infoStr <- sapply( rObj$ptn4Lst ,function( ptn4 ){ ptn4$infoStr })
			if( 0<length(infoStr) ){
				infoStr <- paste( sprintf("  %dth %s",1:length(infoStr),infoStr) ,collapse="\n")
				rptStr <- c( rptStr ,infoStr )
			}
		}

		rptStr <- paste( rptStr ,collapse="\n" )
		cat( sprintf("%s\n",rptStr) )
	}

	if( 2>nrow(pMtx) ){
		# QQE:todo pMtx 데이터가 없는 경우를 위해 처리 필요.
		rObj$filt <- function(aCode){ return(NULL) }
		rObj$explain <- function( part=NULL ){	cat( sprintf("not enough data(row num:%d)\n",nrow(pMtx)) )	}
		return( rObj )
	}

	#	"fv:1" "fv:2" "fv:3" "fv:6"
		# 836  .  6  .  6  .
		# 837  .  .  .  .  .
		# 838  6  .  6  .  .
		# 839  6  .  .  .  6
		# 840  .  .  .  .  .
		# 841  6  .  .  .  .
	fvLineLst <- fCutU.getFallower( pMtx )
	if( debug )	rObj$fvLineLst <- fvLineLst

	#	<pBanInfoLst> Same pattern
		# 836  .  .  .  .  .
		# 837  .  .  .  .  .
		# 838  6  3  6  3  .
		# 839  .  .  .  .  .
		# 840  .  .  .  .  .
		# 841  6  3  .  .  .
	piMtx <- NULL	#	cName <- c(v1 v2 rf cf1 cf2 rs cs1 cs2 hpn)
	for( fIdx in seq_len(length(fvLineLst)) ){	# fIdx <- 2
		fvLine <- fvLineLst[[fIdx]]
		#	(pFV,*)
		for( mIdx in seq_len(length(fvLine[["(pFV,*)"]])) ){
			wMtx <- fvLine[["(pFV,*)"]][[mIdx]]
			tbl <- table(wMtx[,"v2"])
			if( !any(tbl>1) ) next

			tbl.fv <- as.integer(names(tbl[tbl>1]))
			for( fcvIdx in tbl.fv ){	# fcvIdx <- tbl.fv[1]
				mtx <- wMtx[which(wMtx[,"v2"]==fcvIdx),]
				cName <- c("v1","v2","rf","cf1","cf2","rs","cs1","cs2","hpn")
				pairInfo <- rep( 0 ,length(cName) )		;names(pairInfo)<-cName
				pairInfo["hpn"] <- nrow(mtx)
				pairInfo[c("v1","v2")] <- mtx[1,c("v1","v2")]	;pairInfo[c("rf","rs")] <- c( mtx[1,"rIdx"] ,mtx[2,"rIdx"] )
				pairInfo[c("cf1","cf2")] <- mtx[1,c("cIdx1","cIdx2")]	;pairInfo[c("cs1","cs2")] <- mtx[2,c("cIdx1","cIdx2")]
				piMtx <- rbind( piMtx ,pairInfo )
			}
		}

		#	(*,pFV)
		for( mIdx in seq_len(length(fvLine[["(*,pFV)"]])) ){
			wMtx <- fvLine[["(*,pFV)"]][[mIdx]]
			tbl <- table(wMtx[,"v1"])
			if( !any(tbl>1) ) next

			tbl.fv <- as.integer(names(tbl[tbl>1]))
			for( fcvIdx in tbl.fv ){	# 	fcvIdx <- tbl.fv[1]
				mtx <- wMtx[which(wMtx[,"v1"]==fcvIdx),]
				cName <- c("v1","v2","rf","cf1","cf2","rs","cs1","cs2","hpn")
				pairInfo <- rep( 0 ,length(cName) )		;names(pairInfo)<-cName
				pairInfo["hpn"] <- nrow(mtx)
				pairInfo[c("v1","v2")] <- mtx[1,c("v1","v2")]	;pairInfo[c("rf","rs")] <- c( mtx[1,"rIdx"] ,mtx[2,"rIdx"] )
				pairInfo[c("cf1","cf2")] <- mtx[1,c("cIdx1","cIdx2")]	;pairInfo[c("cs1","cs2")] <- mtx[2,c("cIdx1","cIdx2")]
				piMtx <- rbind( piMtx ,pairInfo )
			}
		}

	}
	if( !is.null(piMtx) ){
		# piMtx : remove duplicatred row
		piMtx <- removeDup(piMtx)

		# piMtx.f : remove mis guided row
		idStr <- sprintf("%d_%d",piMtx[,"v1"],piMtx[,"v2"])
		idStr.span <- unique(idStr)
		piMtx.f <- NULL	# for문 내부 주석 참고.
		for( id in idStr.span ){
			# 837  .  3  2  .  .	(3,2)->(3,6)->(3,2) 흐름과
			# 838  .  3  6  3  2	(3,2)->(3,2)->(3,2) 흐름,
			# 840  .  .  .  .  .	(3,5)->(3,2)->(3,2) 라는 각각의 흐름 때문에 
			# 841  .  3  2  3  5	(3,2) 발생 row가 흐름별로 달리 나타나는 문제를 정리.	
			mtx <- piMtx[idStr==id,,drop=F]
			rf.max <- max(mtx[,"rf"])	;rs.max <- max(mtx[,"rs"])	;hpn.max <- max(mtx[,"hpn"])
			latest.pi <- mtx[ mtx[,"rf"]==rf.max & mtx[,"rs"]==rs.max, ,drop=F ][1, ]
			latest.pi["hpn"] <- hpn.max
			piMtx.f <- rbind(piMtx.f,latest.pi)
		}

		# final : rObj$pBanInfoLst <- pBanInfoLst
		if( !is.null(piMtx.f) ){
			pBanInfoLst <- apply( piMtx.f ,1 ,function(piInfo){
				#  v1 v2 rf cf1 cf2 rs cs1 cs2 hpn
				#   2  3  6   3   4  2   3   4   2
				banObj <- list( pairInfo=piInfo )
				olSpan <- getOverlapSpan( piInfo["cs1"] ,piInfo["cf1"] ,2 ,ncol(pMtx) )

				#	incPtn	: 1,2,3,2 -> 3,2,3,4 ----> 5?,2,3,6?
				vDiff <- pMtx[piInfo["rf"],olSpan$a2.span] - pMtx[piInfo["rs"],olSpan$a1.span]
				banObj$incPtn.banVal <- pMtx[piInfo["rf"],olSpan$a2.span]+vDiff
				banObj$incPtn.fixIdx <- olSpan$fixIdx

				#	rebPtn
				step <- nrow(pMtx) - piInfo["rf"] + 1
				banObj$rebPtn.banCol <- olSpan$a2.span
				banObj$rebPtn.banVal <- rep( NA, ncol(pMtx) )
				banObj$rebPtn.banVal[banObj$rebPtn.banCol] <- pMtx[piInfo["rs"]+step,olSpan$a1.span]
				banObj$rebPtn.fixIdx <- piInfo[c("cf1","cf2")]

				return( banObj )
			})
			names(pBanInfoLst) <- sapply( pBanInfoLst ,function(banInfo){ paste(banInfo$pairInfo[c("v1","v2")],collapse="-") })
			rObj$pBanInfoLst <- pBanInfoLst
		}
	}


	#	<iBanInfoLst> increase pattern
		# 838  6  .  6  .  .
		# 839  6  2  .  .  .
		# 840  .  .  .  .  .
		# 841  6  3  .  .  .
	iBanInfoLst <- list()	# increase info
	for( lIdx in seq_len(length(fvLineLst)) ){
		fvLine <- fvLineLst[[lIdx]]
		for( mIdx in seq_len(length(fvLine[["(pFV,*)"]])) ){
			mtx <- fvLine[["(pFV,*)"]][[mIdx]]

			if( (nrow(mtx)<2) || any(is.na(mtx[1:2,"v2"])) )	next
			if( 1==abs(mtx[1,"v2"]-mtx[2,"v2"]) ){	# add inc1Df
				fStep <- mtx[1,"v2"]-mtx[2,"v2"]	# +- 문제때문..
				df <- data.frame( v1=mtx[1,"v1"] ,v2=(mtx[1,"v2"]+fStep) ,baseCol="v1"	,fStep=fStep
							,cIdx1.f=mtx[1,"cIdx1"]	,cIdx2.f=mtx[1,"cIdx2"]	,cIdx1.s=mtx[2,"cIdx1"]	,cIdx2.s=mtx[2,"cIdx2"]
							,valStr=sprintf("(%d,%d)(%d,%d)",mtx[1,"v1"],mtx[1,"v2"],mtx[2,"v1"],mtx[2,"v2"])
							,cordStr=sprintf("%s(%d,%d:%d)(%d,%d:%d)"	,names(fvLineLst)[lIdx]
											,mtx[1,"rIdx"],mtx[1,"cIdx1"],mtx[1,"cIdx2"] 
											,mtx[2,"rIdx"],mtx[2,"cIdx1"],mtx[2,"cIdx2"]
										)
						)
				banObj <- list( typ="inc1" ,incInfoDf=df )

				olSpan <- getOverlapSpan( df$cIdx1.s ,df$cIdx1.f ,2 ,ncol(pMtx) )
				vDiff <- pMtx[mtx[1,"rIdx"],olSpan$a2.span] - pMtx[mtx[2,"rIdx"],olSpan$a1.span]
				banObj$incPtn.banVal <- pMtx[mtx[1,"rIdx"],olSpan$a2.span] + vDiff
				banObj$incPtn.fixIdx <- olSpan$fixIdx

				banObj$multiHpn <- F
				if( nrow(mtx)>=3 && !is.na(mtx[3,"v2"]) )	banObj$multiHpn <- fStep==(mtx[2,"v2"]-mtx[3,"v2"])

				iBanInfoLst[[1+length(iBanInfoLst)]] <- banObj
			}

			if( (nrow(mtx)<3) || any(is.na(mtx[1:3,"v2"])) )	next
			fStep <- mtx[1:2,"v2"]-mtx[2:3,"v2"]
			if( fStep[1]==fStep[2] ){
				df <- data.frame( v1=mtx[1,"v1"] ,v2=(mtx[1,"v2"]+fStep[1]) ,baseCol="v1"	,fStep=fStep[1]
							,cIdx1.f=mtx[1,"cIdx1"]	,cIdx2.f=mtx[1,"cIdx2"]	,cIdx1.s=mtx[2,"cIdx1"]	,cIdx2.s=mtx[2,"cIdx2"]
							,valStr=sprintf("(%d,%d)(%d,%d)",mtx[1,"v1"],mtx[1,"v2"],mtx[2,"v1"],mtx[2,"v2"])
							,cordStr=sprintf("%s(%d,%d:%d)(%d,%d:%d)"	,names(fvLineLst)[lIdx]
											,mtx[1,"rIdx"],mtx[1,"cIdx1"],mtx[1,"cIdx2"] 
											,mtx[2,"rIdx"],mtx[2,"cIdx1"],mtx[2,"cIdx2"]
										)
						)
				banObj <- list( typ="incN" ,incInfoDf=df )

				olSpan <- getOverlapSpan( df$cIdx1.s ,df$cIdx1.f ,2 ,ncol(pMtx) )
				vDiff <- pMtx[mtx[1,"rIdx"],olSpan$a2.span] - pMtx[mtx[2,"rIdx"],olSpan$a1.span]
				banObj$incPtn.banVal <- pMtx[mtx[1,"rIdx"],olSpan$a2.span] + vDiff
				banObj$incPtn.fixIdx <- olSpan$fixIdx

				banObj$multiHpn <- F
				if( nrow(mtx)>=4 && !is.na(mtx[4,"v2"]) )	banObj$multiHpn <- fStep[1]==(mtx[3,"v2"]-mtx[4,"v2"])

				iBanInfoLst[[1+length(iBanInfoLst)]] <- banObj
			}

		}

		for( mIdx in seq_len(length(fvLine[["(*,pFV)"]])) ){
			mtx <- fvLine[["(*,pFV)"]][[mIdx]]

			if( (nrow(mtx)<2) || any(is.na(mtx[1:2,"v1"])) )	next
			if( 1==abs(mtx[1,"v1"]-mtx[2,"v1"]) ){	# add inc1Df
				fStep <- mtx[1,"v1"]-mtx[2,"v1"]	# +- 문제때문..
				df <- data.frame( v1=(mtx[1,"v1"]+fStep) ,v2=mtx[1,"v2"] ,baseCol="v2"	,fStep=fStep
							,cIdx1.f=mtx[1,"cIdx1"]	,cIdx2.f=mtx[1,"cIdx2"]	,cIdx1.s=mtx[2,"cIdx1"]	,cIdx2.s=mtx[2,"cIdx2"]
							,valStr=sprintf("(%d,%d)(%d,%d)",mtx[1,"v1"],mtx[1,"v2"],mtx[2,"v1"],mtx[2,"v2"])
							,cordStr=sprintf("%s(%d,%d:%d)(%d,%d:%d)"	,names(fvLineLst)[lIdx]
											,mtx[1,"rIdx"],mtx[1,"cIdx1"],mtx[1,"cIdx2"] 
											,mtx[2,"rIdx"],mtx[2,"cIdx1"],mtx[2,"cIdx2"]
										)
						)
				banObj <- list( typ="inc1" ,incInfoDf=df )

				olSpan <- getOverlapSpan( df$cIdx1.s ,df$cIdx1.f ,2 ,ncol(pMtx) )
				vDiff <- pMtx[mtx[1,"rIdx"],olSpan$a2.span] - pMtx[mtx[2,"rIdx"],olSpan$a1.span]
				banObj$incPtn.banVal <- pMtx[mtx[1,"rIdx"],olSpan$a2.span] + vDiff
				banObj$incPtn.fixIdx <- olSpan$fixIdx

				banObj$multiHpn <- F
				if( nrow(mtx)>=3 && !is.na(mtx[3,"v1"]) )	banObj$multiHpn <- fStep==(mtx[2,"v1"]-mtx[3,"v1"])

				iBanInfoLst[[1+length(iBanInfoLst)]] <- banObj
			}

			if( (nrow(mtx)<3) || any(is.na(mtx[1:3,"v1"])) )	next
			fStep <- mtx[1:2,"v1"]-mtx[2:3,"v1"]
			if( fStep[1]==fStep[2] ){
				df <- data.frame( v1=(mtx[1,"v1"]+fStep[1]) ,v2=mtx[1,"v2"] ,baseCol="v2"	,fStep=fStep[1]
							,cIdx1.f=mtx[1,"cIdx1"]	,cIdx2.f=mtx[1,"cIdx2"]	,cIdx1.s=mtx[2,"cIdx1"]	,cIdx2.s=mtx[2,"cIdx2"]
							,valStr=sprintf("(%d,%d)(%d,%d)",mtx[1,"v1"],mtx[1,"v2"],mtx[2,"v1"],mtx[2,"v2"])
							,cordStr=sprintf("%s(%d,%d:%d)(%d,%d:%d)"	,names(fvLineLst)[lIdx]
											,mtx[1,"rIdx"],mtx[1,"cIdx1"],mtx[1,"cIdx2"] 
											,mtx[2,"rIdx"],mtx[2,"cIdx1"],mtx[2,"cIdx2"]
										)
						)
				banObj <- list( typ="incN" ,incInfoDf=df )

				olSpan <- getOverlapSpan( df$cIdx1.s ,df$cIdx1.f ,2 ,ncol(pMtx) )
				vDiff <- pMtx[mtx[1,"rIdx"],olSpan$a2.span] - pMtx[mtx[2,"rIdx"],olSpan$a1.span]
				banObj$incPtn.banVal <- pMtx[mtx[1,"rIdx"],olSpan$a2.span] + vDiff
				banObj$incPtn.fixIdx <- olSpan$fixIdx

				banObj$multiHpn <- F
				if( nrow(mtx)>=4 && !is.na(mtx[4,"v1"]) )	banObj$multiHpn <- fStep[1]==(mtx[3,"v1"]-mtx[4,"v1"])

				iBanInfoLst[[1+length(iBanInfoLst)]] <- banObj
			}

		}

	}
	if( 0<length(iBanInfoLst) ){	# multiHpn정리, 중복 정리

		dupILst <-	lapply( iBanInfoLst ,function(iBan){ 
						idStr <-	sprintf( "%s  %s %s" ,iBan$typ ,iBan$incInfoDf[,"valStr"] ,iBan$incInfoDf[,"cordStr"] )
						list( idStr=idStr ,multiHpn=iBan$multiHpn )
					})

		# multiHpn 정리(True값이 존재하면 T로 통일.)
		idStr.uni <- unique( sapply(dupILst ,function(p){p$idStr}) )
		idStr.multiHpn <- rep( F ,length(idStr.uni) )	;names(idStr.multiHpn) <- idStr.uni
		for( idx in seq_len(length(dupILst)) ){
			idStr.multiHpn[dupILst[[idx]]$idStr] <- dupILst[[idx]]$multiHpn || idStr.multiHpn[dupILst[[idx]]$idStr]
		}
		for( idx in seq_len(length(iBanInfoLst)) ){
			idStr <- dupILst[[idx]]$idStr
			iBanInfoLst[[idx]]$multiHpn <- idStr.multiHpn[idStr]
		}

		# 중복 제거
		iBanInfoLst.len <- length(iBanInfoLst)
		if( 1<iBanInfoLst.len ){
			surFlag <- rep( T ,length(dupILst) )
			for( idx1 in 1:(iBanInfoLst.len-1) ){
				for( idx2 in (idx1+1):iBanInfoLst.len ){
					if( dupILst[[idx1]]$idStr==dupILst[[idx2]]$idStr ){
						surFlag[idx2] <- FALSE
					}
				}
			}
			iBanInfoLst <- iBanInfoLst[surFlag]
		}

	}
	rObj$iBanInfoLst <- iBanInfoLst	# increase info


	#	<pairPtnLst> symmetry pattern   Z842
		# 836  8  6  3  6  3
		# 837 23  3  2  3 12
		# 838  6  3  6  3  2
		# 839  6  2  1  1  6
		# 840  2  7 17  1 14	바로 이전 패턴의 재현이 2개 이상 동시발생?
		# 841  6  3  2  3  5	(6,2)-(6,3)-(6,2?)	(2,1)-(2,3)-(2,1?)
	pairPtnLst <- vector("list",2)	;names(pairPtnLst) <- c("(pFV,*)","(*,pFV)")
	for( lIdx in seq_len(length(fvLineLst)) ){
		fvLine <- fvLineLst[[lIdx]]
		for( mIdx in seq_len(length(fvLine[["(pFV,*)"]])) ){
			mtx <- fvLine[["(pFV,*)"]][[mIdx]]
			for( mrIdx in 1:nrow(mtx) ){
				if( mrIdx>length(pairPtnLst[["(pFV,*)"]]) ){
					pairPtnLst[["(pFV,*)"]][[mrIdx]] <- matrix( 0, nrow=0,ncol=2 )
				}

				if( any(is.na(mtx[,c("v1","v2")])) ){	next
				} else {
					pairPtnLst[["(pFV,*)"]][[mrIdx]] <- rbind( pairPtnLst[["(pFV,*)"]][[mrIdx]], mtx[mrIdx,c("v1","v2"),drop=F] )
				}
			}
		}

		for( mIdx in seq_len(length(fvLine[["(*,pFV)"]])) ){
			mtx <- fvLine[["(*,pFV)"]][[mIdx]]
			for( mrIdx in 1:nrow(mtx) ){
				if( mrIdx>length(pairPtnLst[["(*,pFV)"]]) ){
					pairPtnLst[["(*,pFV)"]][[mrIdx]] <- matrix( 0, nrow=0,ncol=2 )
				}

				if( any(is.na(mtx[,c("v1","v2")])) ){	next
				} else {
					pairPtnLst[["(*,pFV)"]][[mrIdx]] <- rbind( pairPtnLst[["(*,pFV)"]][[mrIdx]], mtx[mrIdx,c("v1","v2"),drop=F] )
				}
			}
		}
	}
	pairPtnLst[["(pFV,*)"]] <- lapply( pairPtnLst[["(pFV,*)"]] ,removeDup )
	names(pairPtnLst[["(pFV,*)"]]) <- sprintf("%dth Gen",1:length(pairPtnLst[["(pFV,*)"]]))
	pairPtnLst[["(*,pFV)"]] <- lapply( pairPtnLst[["(*,pFV)"]] ,removeDup )
	names(pairPtnLst[["(*,pFV)"]]) <- sprintf("%dth Gen",1:length(pairPtnLst[["(*,pFV)"]]))
	rObj$pairPtnLst <- pairPtnLst

	#	<ptn4Lst> pair 주변 4개 길이의 재현 배제
		# 836  8  6  3  6  3
		# 837 23  3  2  3 12	4개 길이의 재현 배제
		# 838  6  3  6  3  2	ex : 3
		# 839  6  2  1  1  6		3,2 --> 6th row 6,3,2,3   3,2,3,5
		# 840  2  7 17  1 14				3th row 6,3,6,3   3,6,3,2
		# 841  6  3  2  3  5				1st row 8,6,3,6   6,3,6,3(eadge)
	cWidth <- ncol(pMtx)
	ptn4Lst <- list()
	for( lIdx in seq_len(length(fvLineLst)) ){
		fvLine <- fvLineLst[[lIdx]]
		for( mIdx in seq_len(length(fvLine[["(pFV,*)"]])) ){
			mtx <- fvLine[["(pFV,*)"]][[mIdx]]
			for( mrIdx in 1:nrow(mtx) ){
				if( cWidth==mtx[mrIdx,"cIdx1"] ) next

				ptn <- list( rIdx=mtx[mrIdx,"rIdx"] ,cIdx=c(mtx[mrIdx,"cIdx1"],mtx[mrIdx,"cIdx2"]) )
				if( 1==mtx[mrIdx,"cIdx1"] ){	ptn$cSpan <- 1:4
				} else if( cWidth==(mtx[mrIdx,"cIdx1"]+1) ){	ptn$cSpan <- (cWidth-4:1+1) 
				} else {	ptn$cSpan <- (mtx[mrIdx,"cIdx1"]-1):(mtx[mrIdx,"cIdx2"]+1) }

				ptn$cVal <- pMtx[ ptn$rIdx ,ptn$cIdx ]
				ptn$cSpanVal <- pMtx[ ptn$rIdx ,ptn$cSpan ]
				ptn$infoStr <- sprintf("(%d/%s) %s",ptn$rIdx,paste(ptn$cIdx,collapse=","),paste(ptn$cSpanVal,collapse=",")  )
				ptn4Lst[[1+length(ptn4Lst)]] <- ptn
			}
		}

		for( mIdx in seq_len(length(fvLine[["(*,pFV)"]])) ){
			mtx <- fvLine[["(*,pFV)"]][[mIdx]]
			for( mrIdx in 1:nrow(mtx) ){
				if( 1==mtx[mrIdx,"cIdx2"] ) next

				ptn <- list( rIdx=mtx[mrIdx,"rIdx"] ,cIdx=c(mtx[mrIdx,"cIdx1"],mtx[mrIdx,"cIdx2"]) )
				if( 2==mtx[mrIdx,"cIdx2"] ){	ptn$cSpan <- 1:4
				} else if( cWidth==mtx[mrIdx,"cIdx2"] ){	ptn$cSpan <- (cWidth-4:1+1) 
				} else {	ptn$cSpan <- (mtx[mrIdx,"cIdx1"]-1):(mtx[mrIdx,"cIdx2"]+1) }

				ptn$cVal <- pMtx[ ptn$rIdx ,ptn$cIdx ]
				ptn$cSpanVal <- pMtx[ ptn$rIdx ,ptn$cSpan ]
				ptn$infoStr <- sprintf("(%d/%s) %s",ptn$rIdx,paste(ptn$cIdx,collapse=","),paste(ptn$cSpanVal,collapse=",")  )
				ptn4Lst[[1+length(ptn4Lst)]] <- ptn
			}
		}
	}
	if( 1<length(ptn4Lst) ){	# remove Duplicated
		uIdx <- sapply( ptn4Lst ,function(ptn){
					sprintf("%d/%s",ptn$rIdx,paste(ptn$cIdx,collapse=","))
		})
		dupFlag <- rep( F, length(uIdx) )
		for( idx1 in 1:(length(uIdx)-1) ){
			if( dupFlag[idx1] ) next
			for( idx2 in (idx1+1):length(uIdx) ){
				if( uIdx[idx1]==uIdx[idx2] ) dupFlag[idx2] <- T
			}
		}
		ptn4Lst <- ptn4Lst[!dupFlag]
	}
	rObj$ptn4Lst <- ptn4Lst
	names(rObj$ptn4Lst) <- sapply( rObj$ptn4Lst ,function(ptn){
					sprintf("(%d/%s)",ptn$rIdx,paste(ptn$cIdx,collapse=","))
	})

	rObj$filt <- function( aCode ){
		rstObj <- list()
		# pBanInfoLst
		pairBanLst <- list()
		if( 0<length(rObj$pBanInfoLst) ){
			for( lIdx in 1:length(rObj$pBanInfoLst) ){
				pBI <- rObj$pBanInfoLst[[lIdx]]	# banInfo
				pI	<- pBI$pairInfo
				
				# typ="rebPair"
				foundIdxMtx <- rObj$findPtn( aCode ,pI[c("v1","v2")] )
				if( 0<nrow(foundIdxMtx) ){
					rebLastCol <- apply( foundIdxMtx ,1 ,function(fIdx){all(fIdx==pI[c("cf1","cf2")])} )
					df <- data.frame( typ="rebPair" ,id=lIdx 
									,info=sprintf("val:%s(%d/%s)",paste(pI[c("v1","v2")],collapse=",")
													,pI["rf"],paste(pI[c("cf1","cf2")],collapse=",")
									) 
					)
					cut.incPtn3<-F	;cut.incPtn4<-F		# thld 크기 차이
					if( 2<length(pBI$incPtn.banVal) ){
						cut.incPtn3=fCutU.hasPtn(pBI$incPtn.banVal,aCode,thld=3,fixIdx=pBI$incPtn.fixIdx)
						cut.incPtn4=fCutU.hasPtn(pBI$incPtn.banVal,aCode,thld=4,fixIdx=pBI$incPtn.fixIdx)	
					}
					cut.df <- data.frame( cut.hpn=pI["hpn"]	,cut.lastCol=any(rebLastCol) ,cut.multiFnd=(1<nrow(foundIdxMtx))
									,cut.incPtn3=cut.incPtn3	,cut.incPtn4=cut.incPtn4
									,thldInfo=sprintf("cut.incPtn3=T ,cut.incPtn3=T")
					)
					pairBanLst[[sprintf("%d rebPair",lIdx)]] <- list( fndDf=df ,cutDf=cut.df )
				}

				# typ="pairNextPtn"
				if( all(pBI$rebPtn.banVal[pBI$rebPtn.fixIdx]==aCode[pBI$rebPtn.fixIdx]) ){
					df <- data.frame( typ="pairNextPtn" ,id=lIdx 
									,info=sprintf("from pair %s(%d/%s)",paste(pI[c("v1","v2")],collapse=",")
											,pI["rf"],paste(pI[c("cf1","cf2")],collapse=",")
									) 
					)
					cut.extMatNum <- 0
					if( 2<length(pBI$rebPtn.banCol) ){
						banCol <- setdiff( pBI$rebPtn.banCol ,pBI$rebPtn.fixIdx )
						cut.extMatNum <- sum( pBI$rebPtn.banVal[banCol]==aCode[banCol] )
					}
					cut.df <- data.frame( cut.extMatNum=cut.extMatNum 
									,thldInfo=sprintf("cut.extMatNum>0")
								)
					pairBanLst[[sprintf("%d pairNextPtn",lIdx)]] <- list( fndDf=df ,cutDf=cut.df )
				}
			}
		} # pBanInfoLst -> pairBanLst
		rstObj$pairBanLst <- pairBanLst

		# iBanInfoLst
		iBanLst <- list()
		if( 0<length(rObj$iBanInfoLst) ){
			for( lIdx in 1:length(rObj$iBanInfoLst) ){
				iBanInfo <- rObj$iBanInfoLst[[lIdx]]
				infoDf <- iBanInfo$incInfoDf
				foundIdxMtx <- rObj$findPtn( aCode ,infoDf[1,c("v1","v2")] )
				if( 0==nrow(foundIdxMtx) ) next

				df <- data.frame( typ=iBanInfo$typ	,id=lIdx
					,info=sprintf("val:%s  cord:%s",infoDf[1,"valStr"],infoDf[1,"cordStr"])
				)

				rebLastCol <- apply( foundIdxMtx ,1 ,function(fIdx){all(fIdx==infoDf[c("cIdx1.f","cIdx2.f")])} )
				cut.incPtn3<-F	;cut.incPtn4<-F		# thld 크기 차이
				if( 2<length(iBanInfo$incPtn.banVal) ){
					cut.incPtn3=fCutU.hasPtn(iBanInfo$incPtn.banVal,aCode,thld=3,fixIdx=iBanInfo$incPtn.fixIdx)
					cut.incPtn4=fCutU.hasPtn(iBanInfo$incPtn.banVal,aCode,thld=4,fixIdx=iBanInfo$incPtn.fixIdx)
				}
				cut.df <- data.frame( cut.lastCol=any(rebLastCol) ,cut.multiFnd=(1<nrow(foundIdxMtx))
								,cut.incPtn3=cut.incPtn3	,cut.incPtn4=cut.incPtn4
								,thldInfo=sprintf("cut.incPtn3=T ,cut.incPtn3=T")
				)

				iBanLst[[sprintf("%d incBan",lIdx)]] <- list( fndDf=df ,cutDf=cut.df )
			}
		}
		rstObj$iBanLst <- iBanLst

		pairPtnLst <- list()
		fCnt1 <- rep( 0, length(rObj$pairPtnLst[["(pFV,*)"]]) )
		for( lIdx in seq_len(length(rObj$pairPtnLst[["(pFV,*)"]])) ){
			mtx <- rObj$pairPtnLst[["(pFV,*)"]][[lIdx]]
			for( rIdx in seq_len(nrow(mtx)) ){
				if( fCutU.hasPtn( mtx[rIdx,], aCode ) ) fCnt1[lIdx] <- fCnt1[lIdx] + 1
			}
		}
		pairPtnLst[["(pFV,*)"]] <- fCnt1
		fCnt2 <- rep( 0, length(rObj$pairPtnLst[["(*,pFV)"]]) )
		for( lIdx in seq_len(length(rObj$pairPtnLst[["(*,pFV)"]])) ){
			mtx <- rObj$pairPtnLst[["(*,pFV)"]][[lIdx]]
			for( rIdx in seq_len(nrow(mtx)) ){
				if( fCutU.hasPtn( mtx[rIdx,], aCode ) ) fCnt2[lIdx] <- fCnt2[lIdx] + 1
			}
		}
		pairPtnLst[["(*,pFV)"]] <- fCnt2
		rstObj$pairPtnLst <- pairPtnLst

		ptn4Str <- character(0)
		for( lIdx in seq_len(length(rObj$ptn4Lst)) ){
			ptn4 <- rObj$ptn4Lst[[lIdx]]
			if( !all(ptn4$cVal==aCode[ptn4$cIdx]) ) next

			if( all(ptn4$cSpanVal==aCode[ptn4$cSpan]) ){
				ptn4Str <- c( ptn4Str ,ptn4$infoStr )
			}
		}
		rstObj$ptn4Str <- ptn4Str

		return( rstObj )

	} # rObj$filt()

	return( rObj )

} # fCutU.getFiltObjPair()

fCutU.getFallower <- function( pMtx ){
	#	List Obj : "tLst"    "(pFV,*)" 	"(*,pFV)"

	getTree <- function( truncLst ,cIdxs ,rIdx ){
		#	truncLst : FreqVal에 대한 발생 좌표 추적 경로정보 Table
		if( 0==length(cIdxs) ) return( truncLst )

		rLst <- list()
		for( cIdx in cIdxs ){
			idxMtx <- matrix( c(rIdx,cIdx) ,nrow=1 )
			wLst <- truncLst
			if( 0==length(wLst) ){	wLst[[1]] <- idxMtx
			} else {
				for( lIdx in 1:length(wLst) )	wLst[[lIdx]] <- rbind(wLst[[lIdx]],idxMtx)
			}
			rLst <- append( rLst ,wLst )
		}

		return( rLst )
	} # getTree()

	mtxN <- dim(pMtx)	;names(mtxN)<-c("r","c")
	tbl <- table(pMtx)
	fV <- as.integer(names(tbl))[tbl>1]

	fvInfoLst <- list()	#	"tLst"    "(pFV,*)" 	"(*,pFV)"
	for( fvIdx in fV ){
		infoObj <- list()

		tLst <- list()	# trunc List
		for( rIdx in mtxN["r"]:1 ){
			tLst <- getTree( tLst ,which(pMtx[rIdx,]==fvIdx) ,rIdx )
		}
		infoObj[["tLst"]] <- tLst

		#	(pFV,*)
		valLst <- lapply( tLst ,function( idxMtx ){
			cNames <- c("rIdx","v1","v2","cIdx1","cIdx2")
			rMtx <- matrix( 0 ,nrow=nrow(idxMtx) ,ncol=length(cNames) )	;colnames(rMtx)<-cNames
			rMtx[,"v1"]<-fvIdx	;rMtx[,"rIdx"]<-idxMtx[,1]	;rMtx[,"cIdx1"]<-idxMtx[,2]
			# ---
			for( idx in 1:nrow(idxMtx) ){
				rMtx[idx,"cIdx2"]		<- if( mtxN["c"]==idxMtx[idx,2] ) NA else idxMtx[idx,2]+1
				rMtx[idx,"v2"]	<- if( mtxN["c"]==idxMtx[idx,2] ) NA else pMtx[ idxMtx[idx,1],idxMtx[idx,2]+1 ]
			}
			return( rMtx )
		})
		infoObj[["(pFV,*)"]] <- valLst

		#	(*,pFV)
		valLst <- lapply( tLst ,function( idxMtx ){
			cNames <- c("rIdx","v1","v2","cIdx1","cIdx2")
			rMtx <- matrix( 0 ,nrow=nrow(idxMtx) ,ncol=length(cNames) )	;colnames(rMtx)<-cNames
			rMtx[,"v2"]<-fvIdx	;rMtx[,"rIdx"]<-idxMtx[,1]	;rMtx[,"cIdx2"]<-idxMtx[,2]
			# ---
			for( idx in 1:nrow(idxMtx) ){
				rMtx[idx,"cIdx1"]	<- if( 1==idxMtx[idx,2] ) NA else idxMtx[idx,2]-1
				rMtx[idx,"v1"]		<- if( 1==idxMtx[idx,2] ) NA else pMtx[ idxMtx[idx,1],idxMtx[idx,2]-1 ]
			}
			return( rMtx )
		})
		infoObj[["(*,pFV)"]] <- valLst

		fvInfoLst[[sprintf("fv:%d",fvIdx)]] <- infoObj
	}

	return( fvInfoLst )

} # fCutU.getFallower()

fCutU.rawBanVal <- function( pMtx ){
	#	pMtx <- stdMI$rawTail

	rObj <- list()

	banDf <- u0.zoidMtx_ana(pMtx)
	banDf <- banDf[1<=banDf$banVal & banDf$banVal<=45 ,]

	banValLst <- list()
	for( idx in 1:ncol(pMtx) ){
		banValLst[[idx]] <- matrix( 0, ncol=0, nrow=2 )
		rownames(banValLst[[idx]]) <- c("val","hpnCnt")
	}

	for( rIdx in seq_len(nrow(banDf)) ){
		mtx <- banValLst[[banDf[rIdx,]$tgt.col]]
		fndCol <- which( mtx["val",]==banDf[rIdx,"banVal"] )
		if( 0==length(fndCol) ){
			banValLst[[banDf[rIdx,]$tgt.col]] <- cbind( mtx ,c(banDf[rIdx,"banVal"],1) )
		} else {
			banValLst[[banDf[rIdx,]$tgt.col]]["hpnCnt",fndCol] <- 1 + banValLst[[banDf[rIdx,]$tgt.col]]["hpnCnt",fndCol]
		}
	}

	rObj$banValLst <- banValLst

	rObj$filt <- function( aCode ){
		rstObj <- list()

		cName <- c("col","val","hpnCnt")
		fndMtx <- matrix( 0, nrow=0, ncol=length(cName) )	;colnames(fndMtx)<-cName
		for( lIdx in seq_len(length(rObj$banValLst)) ){
			mtx <- rObj$banValLst[[lIdx]]
			fndIdx <- which(aCode[lIdx]==mtx["val",])
			if( 0<length(fndIdx) ){
				fndMtx <- rbind( fndMtx ,c(lIdx,mtx[,fndIdx]) )
			}
		}

		hpnCnt <- c( nrow(fndMtx) ,sum(2==fndMtx[,"hpnCnt"]) ,sum(3<=fndMtx[,"hpnCnt"]) )
		names(hpnCnt) <- c("hpn","doubleF","moreThanTripleF")

		rstObj <- list( hpnCnt=hpnCnt ,fndMtx=fndMtx )
		return( rstObj )
	}
	rObj$explain <- function( ){
		rptStr <- character(0)
		for( idx in seq_len(length(rObj$banValLst)) ){
			mtx <- rObj$banValLst[[idx]]
			str <- apply( mtx ,2 ,function(rData){ sprintf("%d(%d)",rData[1],rData[2]) })
			str <- sprintf("  %dth col %s\n",idx,paste( str ,collapse=", " ))
			rptStr <- c( rptStr ,str )
		}
		cat( paste(rptStr,collapse="") )
	}
	return( rObj )
}

fCutU.remBanVal <- function( pMtx ){

	banDf <- u0.zoidMtx_ana( pMtx%%10 )
	banDf <- banDf[-1<banDf$banVal & banDf$banVal<11 ,]
	banDf[banDf$banVal==10,"banVal"] <- 0

	banValLst <- list()
	for( idx in 1:ncol(pMtx) ){
		banValLst[[idx]] <- matrix( 0, ncol=0, nrow=2 )
		rownames(banValLst[[idx]]) <- c("val","hpnCnt")
	}

	for( rIdx in seq_len(nrow(banDf)) ){
		mtx <- banValLst[[banDf[rIdx,]$tgt.col]]
		fndCol <- which( mtx["val",]==banDf[rIdx,"banVal"] )
		if( 0==length(fndCol) ){
			banValLst[[banDf[rIdx,]$tgt.col]] <- cbind( mtx ,c(banDf[rIdx,"banVal"],1) )
		} else {
			banValLst[[banDf[rIdx,]$tgt.col]]["hpnCnt",fndCol] <- 1 + banValLst[[banDf[rIdx,]$tgt.col]]["hpnCnt",fndCol]
		}
	}

	rObj <- list( banValLst=banValLst )
	rObj$exctValLst <- fCutU.rawBanVal(pMtx)$banValLst

	rObj$filt <- function( aCode ){
		rstObj <- list()

		aRem <- aCode %% 10
		cName <- c("col","val","hpnCnt")
		fndMtx <- matrix( 0, nrow=0, ncol=length(cName) )	;colnames(fndMtx)<-cName
		for( lIdx in seq_len(length(rObj$banValLst)) ){
			mtx <- rObj$banValLst[[lIdx]]
			exctMtx <- rObj$exctValLst[[lIdx]]
			
			exctFnd <- which( aCode[lIdx]==exctMtx["val",] )
			if( 0<length(exctFnd) ) next

			fndIdx <- which(aRem[lIdx]==mtx["val",])
			if( 0<length(fndIdx) ){
				fndMtx <- rbind( fndMtx ,c(lIdx,mtx[,fndIdx]) )
			}
		}

		hpnCnt <- c( nrow(fndMtx) ,sum(2==fndMtx[,"hpnCnt"]) ,sum(3<=fndMtx[,"hpnCnt"]) )
		names(hpnCnt) <- c("hpn","doubleF","moreThanTripleF")

		rstObj <- list( hpnCnt=hpnCnt ,fndMtx=fndMtx )
		return( rstObj )
	}
	rObj$explain <- function( ){
		rptStr <- character(0)
		for( idx in seq_len(length(rObj$banValLst)) ){
			mtx <- rObj$banValLst[[idx]]
			exctMtx <- rObj$exctValLst[[idx]]
			str <- apply( mtx ,2 ,function(rData){ sprintf("%d(%d)",rData[1],rData[2]) })
			exctStr <- apply( exctMtx ,2, function(rData){ sprintf("%d",rData[1]) })
			str <- sprintf("  %dth col %s  / exct:%s\n",idx,paste( str ,collapse=", " ),paste( exctStr ,collapse=", " ))
			rptStr <- c( rptStr ,str )
		}
		cat( paste(rptStr,collapse="") )
	}


}

fCutU.cStepBanVal <- function( pMtx ){
	#	pMtx <- stdMI$rawTail
	rObj <- list()

	banDf <- u0.zoidCMtx_ana( pMtx )
	banDf <- banDf[1<=banDf$banVal ,]

	banValLst <- list()
	for( idx in 1:(ncol(pMtx)-1) ){
		banValLst[[idx]] <- matrix( 0, ncol=0, nrow=2 )
		rownames(banValLst[[idx]]) <- c("val","hpnCnt")
	}

	for( rIdx in seq_len(nrow(banDf)) ){
		mtx <- banValLst[[banDf[rIdx,]$tgt.col]]
		fndCol <- which( mtx["val",]==banDf[rIdx,"banVal"] )
		if( 0==length(fndCol) ){
			banValLst[[banDf[rIdx,]$tgt.col]] <- cbind( mtx ,c(banDf[rIdx,"banVal"],1) )
		} else {
			banValLst[[banDf[rIdx,]$tgt.col]]["hpnCnt",fndCol] <- 1 + banValLst[[banDf[rIdx,]$tgt.col]]["hpnCnt",fndCol]
		}
	}

	rObj$banValLst <- banValLst
	rObj$filt <- function( aZoid ){
		rstObj <- list()

		aCStep <- aZoid[2:6]-aZoid[1:5]
		cName <- c("col","val","hpnCnt")
		fndMtx <- matrix( 0, nrow=0, ncol=length(cName) )	;colnames(fndMtx)<-cName
		for( lIdx in seq_len(length(rObj$banValLst)) ){
			mtx <- rObj$banValLst[[lIdx]]
			fndIdx <- which(aCStep[lIdx]==mtx["val",])
			if( 0<length(fndIdx) ){
				fndMtx <- rbind( fndMtx ,c(lIdx,mtx[,fndIdx]) )
			}
		}

		hpnCnt <- c( nrow(fndMtx) ,sum(2==fndMtx[,"hpnCnt"]) ,sum(3<=fndMtx[,"hpnCnt"]) )
		names(hpnCnt) <- c("hpn","doubleF","moreThanTripleF")

		rstObj <- list( hpnCnt=hpnCnt ,fndMtx=fndMtx )
		return( rstObj )
	}
	rObj$explain <- function( ){
		rptStr <- character(0)
		for( idx in seq_len(length(rObj$banValLst)) ){
			mtx <- rObj$banValLst[[idx]]
			str <- apply( mtx ,2 ,function(rData){ sprintf("%d(%d)",rData[1],rData[2]) })
			str <- sprintf("  %dth col %s\n",idx,paste( str ,collapse=", " ))
			rptStr <- c( rptStr ,str )
		}
		cat( paste(rptStr,collapse="") )
	}
	return( rObj )

}

fCutU.fStepBanVal <- function( pMtx ){
	#	pMtx <- stdMI$rawTail

	rObj <- list()
	if( 1<nrow(pMtx) ){	# 1개 데이터 뿐이라면 fStep 제약 구성도 없을테니.
		rObj$lastZoid <- pMtx[nrow(pMtx),]
	}

	banDf <- u0.zoidFMtx_ana( pMtx )

	banValLst <- list()
	for( idx in 1:ncol(pMtx) ){
		banValLst[[idx]] <- matrix( 0, ncol=0, nrow=2 )
		rownames(banValLst[[idx]]) <- c("val","hpnCnt")
	}

	for( rIdx in seq_len(nrow(banDf)) ){
		mtx <- banValLst[[banDf[rIdx,]$tgt.col]]
		fndCol <- which( mtx["val",]==banDf[rIdx,"banVal"] )
		if( 0==length(fndCol) ){
			banValLst[[banDf[rIdx,]$tgt.col]] <- cbind( mtx ,c(banDf[rIdx,"banVal"],1) )
		} else {
			banValLst[[banDf[rIdx,]$tgt.col]]["hpnCnt",fndCol] <- 1 + banValLst[[banDf[rIdx,]$tgt.col]]["hpnCnt",fndCol]
		}
	}

	rObj$banValLst <- banValLst

	rObj$filt <- function( aZoid ){
		rstObj <- list()

		aFStep <- aZoid - rObj$lastZoid	# rObj$lastZoid가 없으면, rObj$banValLst 역시 없으므로..
		cName <- c("col","val","hpnCnt")
		fndMtx <- matrix( 0, nrow=0, ncol=length(cName) )	;colnames(fndMtx)<-cName
		for( lIdx in seq_len(length(rObj$banValLst)) ){
			mtx <- rObj$banValLst[[lIdx]]
			fndIdx <- which(aFStep[lIdx]==mtx["val",])
			if( 0<length(fndIdx) ){
				fndMtx <- rbind( fndMtx ,c(lIdx,mtx[,fndIdx]) )
			}
		}

		hpnCnt <- c( nrow(fndMtx) ,sum(2==fndMtx[,"hpnCnt"]) ,sum(3<=fndMtx[,"hpnCnt"]) )
		names(hpnCnt) <- c("hpn","doubleF","moreThanTripleF")

		rstObj <- list( hpnCnt=hpnCnt ,fndMtx=fndMtx )
		return( rstObj )
	}
	rObj$explain <- function( ){
		rptStr <- character(0)
		for( idx in seq_len(length(rObj$banValLst)) ){
			mtx <- rObj$banValLst[[idx]]
			str <- apply( mtx ,2 ,function(rData){ sprintf("%d(%d)",rData[1],rData[2]) })
			str <- sprintf("  %dth col %s\n",idx,paste( str ,collapse=", " ))
			rptStr <- c( rptStr ,str )
		}
		cat( paste(rptStr,collapse="") )
	}
	return( rObj )

}


#	그룹 sum패턴 존재 확인 : sum(aCStep[c( , )])==sum(aCStep[c( , )])
#		forReport : T이면 보고만, F이면 필터링용 객체 반환.
fCutU.rptGrpSum <- function( aCode ,forReport=T ){
	#	aCode <- c( -4, -2, 16, 14, 24 )
	aLen <- length( aCode )
	colCord <- 1:aLen
	eadge <- aLen %/% 2

	sumLst <- list()
	for( grp1Size in 2:eadge ){
		grp1Mtx <- combinations( aLen ,grp1Size )
		for( rIdx1 in 1:nrow(grp1Mtx) ){
			leftCol <- (1:aLen)[ -grp1Mtx[rIdx1,] ]
			for( grp2Size in 2:length(leftCol) ){
				grp2Mtx <- combinations( length(leftCol) ,grp2Size )
				for( rIdx2 in 1:nrow(grp2Mtx) ){
					cord1 <- grp1Mtx[rIdx1,]
					cord2 <- leftCol[ grp2Mtx[rIdx2,] ]
					cord.str <- c( paste( cord1,collapse="," ), paste( cord2,collapse="," ) )
					cordVal1 <- aCode[cord1]
					cordVal2 <- aCode[cord2]

					uId <- NULL
					if( 1==order(cord.str)[1] ){
						uId <- sprintf("(%s/%s)",cord.str[1],cord.str[2])
					} else {
						uId <- sprintf("(%s/%s)",cord.str[2],cord.str[1])
					}

					if( 1<length(sumLst) && any(names(sumLst)==uId) ) next
					if( sum(aCode[cord1]) != sum(aCode[cord2]) ) next
					if( any(0==cordVal1) || any(0==cordVal2) ) next
					if( 0<length(intersect(cordVal1,cordVal2)) ) next

					sumInfo <- list( sumVal=c(sum(aCode[cord1]),sum(aCode[cord2])) 
										,cord1=cord1	,cord2=cord2
										,cordValStr=c( paste(cordVal1,collapse=",") ,cord2Val=paste(cordVal2,collapse=",") )
									)

					vals <- abs(c(cordVal1,cordVal2))	# (-2,-6)/( 4, 4 ) 처럼 최저값의 배수로 끝나는 패턴인지
					if( 1<min(vals) ){
						if( all( (vals%%min(vals)) == 0 ) ){
							sumInfo$warn1 <- sprintf("warn1 - multiple of %d",min(vals))
						}
					}

					sumLst[[uId]] <- sumInfo
				}
			}
		}
	}

	rptStr <- NULL
	for( uIdx in names(sumLst) ){
		sumInfo <- sumLst[[uIdx]]
		str <- sprintf("    %s sum:%d %s\n",uIdx,sumInfo$sumVal[1]
					,ifelse( is.null(sumInfo$warn1) , "", sumInfo$warn1 )
				)
		rptStr <- c( rptStr ,str )
	}

	if( forReport ){
		cat( paste(rptStr,collapse="") )
	} else {
		rObj <- list( sumLst=sumLst )
		return( rObj )
	}
} # fCutU.rptGrpSum()

#	toZnnn.R 에서의 finalCut() 함수에서 사용.
#		주의 : NA 은 미발생 허용을 의미.
#			   0  은 미발생을 허용치 않겠다는 의미이다.
fCutU.cutScore <- function( score ,pMinMaxSum=c(NA,NA) ,pMinMaxHpn=c(NA,NA) ,pMinMaxEvent=c(2,NA,NA) ){
	#	적용치 않을 Min, Max는 NA를 입력
	#     score <- c( 1, 0, 0, 1, 0, 1, 1, 0, 2, 0, 2, 0 ) # 1 --  8(2)
	#		pMinMaxSum=c(7,12)	;pMinMaxHpn=c(5,8) ;pMinMaxEvent=c(2,1,3)

	cName <- c( "min.sum", "max.sum", "min.hpn", "max.hpn", "min.evt", "max.evt" )
	cutRst <- rep( NA ,length(cName) )		;names(cutRst) <- cName

	scoreSum <- sum( score )
	if( !is.na(pMinMaxSum[1]) ){	# min of sum
		cutRst["min.sum"] <- (scoreSum <= pMinMaxSum[1])
	}
	if( !is.na(pMinMaxSum[2]) ){	# max of sum
		cutRst["max.sum"] <- (scoreSum >= pMinMaxSum[2])
	}

	scoreHpn <- sum( score>0 )
	if( !is.na(pMinMaxHpn[1]) ){	# min of happen
		cutRst["min.hpn"] <- (scoreHpn <= pMinMaxHpn[1])
	}
	if( !is.na(pMinMaxHpn[2]) ){	# max of happen
		cutRst["max.hpn"] <- (scoreHpn >= pMinMaxHpn[2])
	}

	scoreEvent <- sum( score>=pMinMaxEvent[1] )
	if( !is.na(pMinMaxEvent[2]) ){	# min of event
		cutRst["min.evt"] <- (scoreEvent <= pMinMaxEvent[2])
	}
	if( !is.na(pMinMaxEvent[3]) ){	# max of event
		cutRst["max.evt"] <- (scoreEvent >= pMinMaxEvent[3])
	}

	return( cutRst )

} # fCutU.cutScore()


fCutU.commonCutCnt <- function( gEnv, allIdxF ,zMtx
						,pZWidth=TRUE	,pQuoTbl=TRUE	,pRebThld=2
						,pScoreMtx=TRUE	,rpt=FALSE
					 ) {
	#	pZWidth=TRUE	;pQuoTbl=TRUE	;pRebThld=2	;pScoreMtx=TRUE	;rpt=FALSE
	flgCnt <- rep( 0 ,length(allIdxF) )
	stdMI <- fCutU.getMtxInfo( zMtx )
	scoreMtx <- NULL
	cStepValMtx <- NULL
	if( pScoreMtx ){
		cName <- c("reb","nbor","spanM"
						,"quoAll","quoPtn","zw"
						,"remH0","remH1","cStep2","cStep3"
						,"w1CStep.cnt","w1FStep.cnt","w1CStep.matLen","w1FStep.matLen"
					)
		scoreMtx <- matrix( 0, nrow=length(allIdxF) ,ncol=length(cName) )
		colnames(scoreMtx) <-cName

		cName <- c("c31","c32","c33","c34","c21","c22","c23","c24","c25","max2","min2")
		cStepValMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cName) )
		colnames(cStepValMtx) <- cName
	}

	fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- 0
					if( pRebThld<=sum(stdMI$lastZoid%in%aZoid) ) cnt<-cnt+1
					if( (pRebThld-1)<=sum(stdMI$lastZoid==aZoid) ) cnt<-cnt+1
					return( cnt )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flag <- fltCnt==0	# 양쪽 다 나온 경우도 빈번해서. 그냥 둘 중 하나만 나와도 flag처리.
	# flgCnt[!flag] <- flgCnt[!flag] + 1	# 너무 빈번하니 별도계산.
	if( pScoreMtx ) scoreMtx[,"reb"] <- !flag

	neighborObj <- fCutU.neighborObj( stdMI$rawTail )
	fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( neighborObj$matchCnt(aZoid) )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flag <- fltCnt==0
	flgCnt[!flag] <- flgCnt[!flag] + 1
	if( pScoreMtx ) scoreMtx[,"nbor"] <- fltCnt

	spanMatchObj <- fCutU.getSpanMatchObj( stdMI$rawTail )
	fltCnt <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( spanMatchObj$matchCnt(aZoid) )
				})	;kIdx<-anaFltCnt(fltCnt,rpt)
	flag <- fltCnt==0
	flgCnt[!flag] <- flgCnt[!flag] + 1
	if( pScoreMtx ) scoreMtx[,"spanM"] <- fltCnt

	if( pQuoTbl ){
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoTbl <- table(aZoid%/%10)
						return( !stdMI$quo10$sameTbl(quoTbl) )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
		if( pScoreMtx ) scoreMtx[,"quoAll"] <- !flag
		stdQuo <- fCutU.chkRowPtnReb(stdMI$quoTail)
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoSize <- fCutU.getQuoObj(aZoid)$size
						return( !stdQuo$filt(quoSize)$filted )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
		if( pScoreMtx ) scoreMtx[,"quoPtn"] <- !flag
	}
	if( pZWidth && stdMI$mtxLen>0 ){
		lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						return( lastZW!=(aZoid[6]-aZoid[1]) )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
		if( pScoreMtx ) scoreMtx[,"zw"] <- !flag
	}

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){		# rem ptn 재현 3이상
						fnd <- fCutU.hasPtn( stdMI$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
	if( pScoreMtx ) scoreMtx[,"remH0"] <- !flag

	if( 1<stdMI$mtxLen ){
		tgtZoidRem <- zMtx[stdMI$mtxLen-1,] %% 10	# rem ptn 재현 4이상 - hIdx-1 에서.
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						aCode <- aZoid%%10
						for( cIdx in 1:3 ){
							fnd <- fCutU.hasPtn( tgtZoidRem[cIdx+0:3] ,aCode )
							if( fnd ) return( FALSE )
						}
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
		if( pScoreMtx ) scoreMtx[,"remH1"] <- !flag

		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						aCode <- aZoid[2:6] - aZoid[1:5]
						for( cIdx in 1:4 ){	# cStep 반복. 동일 재현이 2개 붙어서 발생.
							fnd <- fCutU.hasPtn( stdMI$cStep[cIdx+0:1] ,aCode )
							if( fnd ) return( FALSE )
						}
						return( TRUE )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
		if( pScoreMtx ) scoreMtx[,"cStep2"] <- !flag
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						cnt <- sum( stdMI$cStep==(aZoid[2:6]-aZoid[1:5]) )
						return( 3 > cnt )	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
		if( pScoreMtx ) scoreMtx[,"cStep3"] <- !flag

		wCObj <- fCutU.getChkNextPtn4FV.cStep( stdMI$rawTail )
		wFObj <- fCutU.getChkNextPtn4FV.fStep( stdMI$rawTail )
		for( idx in seq_len(length(allIdxF)) ){
			aZoid <- gEnv$allZoidMtx[allIdxF[idx],]
			#	scoreMtx	"w1CStep.cnt","w1FStep.cnt","w1CStep.matLen","w1FStep.matLen"
			aCStep <- aZoid[2:6] - aZoid[1:5]
			chkObj.c <- wCObj$check(aCStep)
			scoreMtx[idx,"w1CStep.cnt"] <- chkObj.c$flgCnt
			scoreMtx[idx,"w1CStep.matLen"] <- ifelse( 0<length(chkObj.c$matchCnt) ,max(chkObj.c$matchCnt) ,0 )

			aFStep <- aZoid - stdMI$lastZoid
			chkObj.f <- wFObj$check(aFStep)
			scoreMtx[idx,"w1FStep.cnt"] <- chkObj.f$flgCnt
			scoreMtx[idx,"w1FStep.matLen"] <- ifelse( 0<length(chkObj.f$matchCnt) ,max(chkObj.f$matchCnt) ,0 )
		}

		# cName <- c("c31","c32","c33","c34","c21","c22","c23","c24","c25","max2","min2")
		# cStepValMtx <- matrix( 0 ,nrow=length(allIdxF) ,ncol=length(cName) )
		for( idx in 1:4 ){
			colSpan <- 0:2+idx	# column span of cStepMtx

			logId <- sprintf("c3%d",idx)
			obj <- fCutU.getChkCStepValReb( zMtx[,colSpan,drop=F] )
			cStepValMtx[,logId] <- obj$match( gEnv$allZoidMtx[allIdxF,colSpan,drop=F] )
		}
		for( idx in 1:5 ){
			colSpan <- 0:1+idx	# column span of cStepMtx

			logId <- sprintf("c2%d",idx)
			obj <- fCutU.getChkCStepValReb( zMtx[,colSpan,drop=F] )
			cStepValMtx[,logId] <- obj$match( gEnv$allZoidMtx[allIdxF,colSpan,drop=F] )
		}
		cStepMax2 <- sort(stdMI$lastZoid[2:6]-stdMI$lastZoid[1:5] ,decreasing=T)[1:2]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						return( !all( cStepMax2 == sort( aZoid[2:6]-aZoid[1:5] ,decreasing=T)[1:2] ) )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		cStepValMtx[,"max2"] <- !flag
		flgCnt[!flag] <- flgCnt[!flag] + 1
		cStepMin2 <- sort(stdMI$lastZoid[2:6]-stdMI$lastZoid[1:5] )[1:2]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						return( !all( cStepMax2 == sort( aZoid[2:6]-aZoid[1:5] )[1:2] ) )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		cStepValMtx[,"min2"] <- !flag
		flgCnt[!flag] <- flgCnt[!flag] + 1
	}

	rObj <- list( flgCnt=flgCnt ,scoreMtx=scoreMtx ,cStepValMtx=cStepValMtx ,lastZoid=stdMI$lastZoid )
	rObj$scoreMtx2 <- fCutU.ccc.score2( gEnv ,allIdxF ,zMtx )
	rObj$scoreMtx3 <- fCutU.ccc.score3( gEnv ,allIdxF ,zMtx )
	rObj$scoreMtx4 <- fCutU.ccc.score4( gEnv ,allIdxF ,zMtx )
	return( rObj )

} #	fCutU.commonCutCnt( )

fCutU.ccc.score2 <- function( gEnv, allIdxF, zMtx ){
	#	...ab. <-- left slide ( k, a, b 패턴에 대한 V 값)
	#   ..k...
	#   .V....

	# zMtx : 각 ph에서의 히스토리.
	#	zMtx <- gEnv$zhF

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


	cName <- c("rebV","rebC","rebC2","rebL","rebR","rebL.cnt","rebR.cnt")
	cName <- c( cName ,c("inc.r","inc.c","inc.r2","inc.c2","inc.r3","inc.c3") )
	scoreMtx <- matrix( 0, nrow=length(allIdxF), ncol=length(cName) )
	colnames( scoreMtx ) <- cName
		#	rebV	: 값 재발 수
		#	rebC	: 동일 컬럼 재발 수
		#	rebL, rebR	: left slide reb, right slide reb
		#	inc.raw, inc.cStep : 가장 최근의 증/감 반복.(raw, cStep)

	stdMI <- fCutU.getMtxInfo( zMtx )

	if( TRUE ){	# reb3V ,reb2C ,rebL ,rebR
		slideObj <- getSlideReb( zMtx )
		inc.stdRaw		<- if( 2>stdMI$mtxLen ){ NULL 
							} else {
								vDiff <- stdMI$lastZoid - zMtx[stdMI$mtxLen-1,]
								stdMI$lastZoid+vDiff
							}
		inc.stdRaw2		<- if( 4>stdMI$mtxLen ){ NULL 
							} else {
								vDiff <- zMtx[stdMI$mtxLen-1,] - zMtx[stdMI$mtxLen-3,]
								zMtx[stdMI$mtxLen-1,]+vDiff
							}
		inc.stdRaw3		<- if( 6>stdMI$mtxLen ){ NULL 
							} else {
								vDiff <- zMtx[stdMI$mtxLen-2,] - zMtx[stdMI$mtxLen-5,]
								zMtx[stdMI$mtxLen-2,]+vDiff
							}
		inc.stdCStep	<- if( 2>stdMI$mtxLen ){ NULL 
							} else {
								h2Zoid <- zMtx[stdMI$mtxLen-1,]
								vDiff <- stdMI$cStep - (h2Zoid[2:6]-h2Zoid[1:5])
								stdMI$cStep+vDiff
							}
		inc.stdCStep2	<- if( 4>stdMI$mtxLen ){ NULL 
							} else {
								cStep <- zMtx[,2:6] - zMtx[,1:5]
								vDiff <- cStep[stdMI$mtxLen-1,] - cStep[stdMI$mtxLen-3,]
								cStep[stdMI$mtxLen-1,]+vDiff
							}
		inc.stdCStep3	<- if( 6>stdMI$mtxLen ){ NULL 
							} else {
								cStep <- zMtx[,2:6] - zMtx[,1:5]
								vDiff <- cStep[stdMI$mtxLen-2,] - cStep[stdMI$mtxLen-5,]
								cStep[stdMI$mtxLen-2,]+vDiff
							}


		rawLen <- nrow( stdMI$rawTail )
		for( aIdx in seq_len(length(allIdxF)) ){
			aZoid <- gEnv$allZoidMtx[allIdxF[aIdx],]
			aCStep <- aZoid[2:6] - aZoid[1:5]
			aFStep <- aZoid - stdMI$lastZoid
			scoreMtx[aIdx,"rebV"] <- sum( aZoid %in% stdMI$lastZoid )
			scoreMtx[aIdx,"rebC"] <- sum( aZoid == stdMI$lastZoid )
			if( 1<rawLen ){
				scoreMtx[aIdx,"rebC2"] <- sum( aZoid == stdMI$rawTail[rawLen-1,] )
			}

			if( !is.null(slideObj) ){
				scoreMtx[aIdx,"rebL"] <- sum( aZoid[slideObj$lMtx["col",]] == slideObj$lMtx["val",] ,na.rm=T )
				scoreMtx[aIdx,"rebR"] <- sum( aZoid[slideObj$rMtx["col",]] == slideObj$rMtx["val",] ,na.rm=T )
				scoreMtx[aIdx,"rebL.cnt"] <- sum( !is.na(slideObj$lMtx["val",]) )
				scoreMtx[aIdx,"rebR.cnt"] <- sum( !is.na(slideObj$rMtx["val",]) )
			}

			if( !is.null(inc.stdRaw ) )	scoreMtx[aIdx,"inc.r"]	<- sum(aZoid==inc.stdRaw  ,na.rm=T)
			if( !is.null(inc.stdRaw2) )	scoreMtx[aIdx,"inc.r2"]	<- sum(aZoid==inc.stdRaw2 ,na.rm=T)
			if( !is.null(inc.stdRaw3) )	scoreMtx[aIdx,"inc.r3"]	<- sum(aZoid==inc.stdRaw3 ,na.rm=T)

			if( !is.null(inc.stdCStep ) )	scoreMtx[aIdx,"inc.c"]	<- sum(aCStep==inc.stdCStep  ,na.rm=T)
			if( !is.null(inc.stdCStep2) )	scoreMtx[aIdx,"inc.c2"]	<- sum(aCStep==inc.stdCStep2 ,na.rm=T)
			if( !is.null(inc.stdCStep3) )	scoreMtx[aIdx,"inc.c3"]	<- sum(aCStep==inc.stdCStep3 ,na.rm=T)

		}
	}	# reb3V ,reb2C
	#	dbgN<-"rebV"	;table(scoreMtx[,dbgN])	;dbgIdx<-head(which(scoreMtx[,dbgN]==1))	;aIdx<-dbgIdx[1]

	return( scoreMtx )

}	# fCutU.ccc.score2

fCutU.ccc.score3 <- function( gEnv, allIdxF, zMtx ){

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
		rObj <- list( matLst=list() )
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
		rowLen <- nrow( mtx )	;colLen <- ncol(mtx)
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


	cName <- c("rebPtn.1","rebPtn.n","rebC.C1","rebC.F1","rebC.C2","rebC.F2")
	cName <- c( cName ,"snMax.r" ,"snFCnt.r" ,"snMax.c" ,"snFCnt.c" )	# seqNext Cnt - Max val, Flag Cnt

	scoreMtx <- matrix( 0, nrow=length(allIdxF), ncol=length(cName) )
	colnames( scoreMtx ) <- cName
	#	rebC.Cn/rebC.Cn : rebC의 CStep,FStep 버전 (h-1,h-2)

	if( 0==nrow(zMtx) ){ return( scoreMtx ) }

	stdMI <- fCutU.getMtxInfo( zMtx )
	rowLen <- nrow(stdMI$rawTail)

	rebPtn.1 <- getRebPtn.1( stdMI )
	rebPtn.n <- getRebPtn.n( stdMI )
	seqNextPtn.raw		<- getSeqPtn( stdMI$rawTail )
	seqNextPtn.cStep	<- getSeqPtn( stdMI$cStepTail )
	if( TRUE ){
		for( aIdx in seq_len(length(allIdxF)) ){
			aZoid <- gEnv$allZoidMtx[allIdxF[aIdx],]
			aCStep <- aZoid[2:6] - aZoid[1:5]
			aFStep <- aZoid - stdMI$lastZoid

			scoreMtx[aIdx,"rebC.C1"] <- sum(stdMI$cStep==aCStep)
			scoreMtx[aIdx,"rebC.F1"] <- sum(stdMI$fStep==aFStep)

			if( 1<rowLen ){
				scoreMtx[aIdx,"rebC.C2"] <- sum(stdMI$cStepTail[rowLen-1,]==aCStep)
				scoreMtx[aIdx,"rebC.F2"] <- sum(stdMI$fStepTail[rowLen-1,]==aFStep)

				# rebPtn.1
				if( 0<nrow(rebPtn.1$matInfo) ){
					reb.lastZoid <- stdMI$lastZoid[rebPtn.1$matInfo[,"fromC"]]
					reb.aZoid <- aZoid[rebPtn.1$matInfo[,"toC"]]
					scoreMtx[aIdx,"rebPtn.1"] <- sum( reb.aZoid==reb.lastZoid )
				}

				# rebPtn.n
				if( length(rebPtn.n)>0 ){
					flag <- sapply( rebPtn.n ,function( matMtx ){
									fromVal <- stdMI$lastZoid[matMtx["from",]]
									toVal <- aZoid[matMtx["to",]]
									return( all(fromVal==toVal) )
								})
					scoreMtx[aIdx,"rebPtn.n"] <- sum( flag )
				}

				#	"sncMax.raw" ,"sncFCnt.raw" 
				snMatCnt.raw <- seqNextPtn.raw$filt( aZoid )$matCnt
				scoreMtx[aIdx,"snMax.r"] <- max( snMatCnt.raw )
				scoreMtx[aIdx,"snFCnt.r"] <- sum( snMatCnt.raw>=2 )

				#	"sncMax.cStep" ,"sncFCnt.cStep"
				snMatCnt.cStep <- seqNextPtn.cStep$filt( aZoid )$matCnt
				scoreMtx[aIdx,"snMax.c"] <- max( snMatCnt.cStep )
				scoreMtx[aIdx,"snFCnt.c"] <- sum( snMatCnt.cStep>=2 )

			}
		} # for
	}

	return( scoreMtx )

}	# fCutU.ccc.score3

fCutU.ccc.score4 <- function( gEnv, allIdxF, zMtx ){

	# zMtx : 각 ph에서의 히스토리.
	#	zMtx <- gEnv$zhF
	getFreqVal <- function( valMtx ){	# valMtx <- stdMI$rawTail
		tbl <- table(valMtx)	;tbl <- tbl[tbl>1]
		fVal <- as.integer(names(tbl))

		rowLen <- nrow( valMtx )
		rowLst <- list()
		for( vIdx in fVal ){
			matLst <- list()
			for( rIdx in rowLen:1 ){
				fndCol <- which(valMtx[rIdx,]==vIdx)
				if( length(fndCol)==0 ) next

				fObj <- list( rIdx=rIdx ,fndCol=fndCol )
				matLst[[1+length(matLst)]] <- fObj
			}
			if( 2>length(matLst) ) next

			rowLst[[1+length(rowLst)]] <- list( fVal=vIdx
													,rIdx=sapply(matLst,function(p){p$rIdx})  
													,colLst=lapply(matLst,function(p){p$fndCol})
												)
		}
		rObj <- rowLst
		return( rObj )
	}
	getStdDiff <- function( valMtx ,ptnMin=3){
		getSpanInfo <- function( preCIdx ,postCIdx ,colNum ){
			fixIdx <- NULL
			preSpan <- NULL		;postSpan <- NULL
			if( preCIdx<postCIdx ){
				lMargin <- preCIdx - 1
				rMargin <- colNum - postCIdx
				fixIdx <- preCIdx
			} else {
				lMargin <- postCIdx-1
				rMargin <- colNum - preCIdx
				fixIdx <- postCIdx
			}

			rObj <- list( preSpan=(preCIdx-lMargin):(preCIdx+rMargin) ,postSpan=(postCIdx-lMargin):(postCIdx+rMargin) ,fixIdx=fixIdx )
		}

		fVLst <- getFreqVal(valMtx)
		colNum<-ncol(valMtx)	;rowNum<-nrow(valMtx)

		ptnLst <- list()
		for( idx in seq_len(length(fVLst)) ){
			fVObj <- fVLst[[idx]]
			for( postCIdx in fVObj$colLst[[1]] ){
				for( preCIdx in fVObj$colLst[[2]] ){
					spanInfo <- getSpanInfo( preCIdx, postCIdx, colNum)
					postSpan <- valMtx[fVObj$rIdx[1],spanInfo$postSpan]
					preSpan <- valMtx[fVObj$rIdx[2],spanInfo$preSpan]
					banPtn <- postSpan + (postSpan - preSpan)
					if( ptnMin<=length(banPtn) ){
						ptnObj <- list( fixIdx=spanInfo$fixIdx ,fixVal=fVObj$fVal ,banPtn=banPtn )
						ptnLst[[1+length(ptnLst)]] <- ptnObj
					}
				} # for( preCIdx )
			}
		} # for(idx)
		rObj <- list( ptnLst=ptnLst ,fVLst=fVLst )
		return( rObj )
	}
	getFreqVal.raw <- function( stdMI ){
		diffObj <- getStdDiff( stdMI$rawTail )

		ptnLst <- list()
		for( idx in seq_len(length(diffObj$ptnLst)) ){	# refine diffObj$ptnLst
			ptnObj <- diffObj$ptnLst[[idx]]
			ptnObj$banPtn[ ptnObj$banPtn<= 0 ] <- NA
			ptnObj$banPtn[ ptnObj$banPtn >45 ] <- NA
			if( 3>sum(!is.na(ptnObj$banPtn)) )	next

			ptnLst[[1+length(ptnLst)]] <- ptnObj
		}

		rObj <- list( ptnLst=ptnLst )
		rObj$filt <- function( aCode ,pThld=3 ){
			rstObj <- list( flag=FALSE )
			for( idx in seq_len(length(rObj$ptnLst)) ){
				ptnObj <- rObj$ptnLst[[idx]]
				flag <- fCutU.hasPtn(ptnObj$banPtn,aCode,thld=pThld,fixIdx=ptnObj$fixIdx)
				if( flag ){
					rstObj$flag <- TRUE
					rstObj$ptnObj <- ptnObj
					break
				}
			}
			return( rstObj )
		}
		return( rObj )
	} # getFreqVal.raw()
	getFreqVal.cStep <- function( stdMI ){
		diffObj <- getStdDiff( stdMI$cStepTail )

		ptnLst <- list()
		for( idx in seq_len(length(diffObj$ptnLst)) ){	# refine diffObj$ptnLst
			ptnObj <- diffObj$ptnLst[[idx]]
			ptnObj$banPtn[ ptnObj$banPtn<= 0 ] <- NA
			if( 3>sum(!is.na(ptnObj$banPtn)) )	next

			ptnLst[[1+length(ptnLst)]] <- ptnObj
		}

		rObj <- list( ptnLst=ptnLst )
		rObj$filt <- function( aCode ,pThld=3 ){
			rstObj <- list( flag=FALSE )
			for( idx in seq_len(length(rObj$ptnLst)) ){
				ptnObj <- rObj$ptnLst[[idx]]
				flag <- fCutU.hasPtn(ptnObj$banPtn,aCode,thld=pThld,fixIdx=ptnObj$fixIdx)
				if( flag ){
					rstObj$flag <- TRUE
					rstObj$ptnObj <- ptnObj
					break
				}
			}
			return( rstObj )
		}
		return( rObj )
	} # getFreqVal.cStep()
	getFreqVal.fStep <- function( stdMI ){
		diffObj <- getStdDiff( stdMI$fStepTail )

		ptnLst <- list()
		for( idx in seq_len(length(diffObj$ptnLst)) ){	# refine diffObj$ptnLst
			ptnObj <- diffObj$ptnLst[[idx]]
			#	딱히 기준을 잡기가 애매하네.
			if( 3>sum(!is.na(ptnObj$banPtn)) )	next

			ptnLst[[1+length(ptnLst)]] <- ptnObj
		}

		rObj <- list( ptnLst=ptnLst )
		rObj$filt <- function( aCode ,pThld=3 ){
			rstObj <- list( flag=FALSE )
			for( idx in seq_len(length(rObj$ptnLst)) ){
				ptnObj <- rObj$ptnLst[[idx]]
				flag <- fCutU.hasPtn(ptnObj$banPtn,aCode,thld=pThld,fixIdx=ptnObj$fixIdx)
				if( flag ){
					rstObj$flag <- TRUE
					rstObj$ptnObj <- ptnObj
					break
				}
			}
			return( rstObj )
		}
		return( rObj )
	} # getFreqVal.fStep()

	cName <- c("incRaw3","incC3","incF3","incRaw2","incC2","incF2","(1,6)")
	cName <- c( cName ,c("nextVal.r","nextVal.c","nextVal.f") )
	scoreMtx <- matrix( 0, nrow=length(allIdxF), ncol=length(cName) )
	colnames( scoreMtx ) <- cName
	#	incRaw,incC,incF : raw,cStep,fStep의 일정증감.
	#		*.r	: 대칭 방향에서의 일정증감.

	if( 0==nrow(zMtx) ){ return( scoreMtx ) }

	stdMI <- fCutU.getMtxInfo( zMtx )
	rowLen <- nrow(stdMI$rawTail)
	freqVal.raw		<- getFreqVal.raw( stdMI )
	freqVal.cStep	<- getFreqVal.cStep( stdMI )
	freqVal.fStep	<- getFreqVal.fStep( stdMI )
	nextSeq.raw		<- fCutU.getNextSeq( stdMI$rawTail )
	nextSeq.cStep	<- fCutU.getNextSeq( stdMI$cStepTail )
	nextSeq.fStep	<- fCutU.getNextSeq( stdMI$fStepTail )
	if( TRUE ){
		for( aIdx in seq_len(length(allIdxF)) ){
			aZoid <- gEnv$allZoidMtx[allIdxF[aIdx],]
			aCStep <- aZoid[2:6] - aZoid[1:5]
			aFStep <- aZoid - stdMI$lastZoid

			scoreMtx[aIdx,"(1,6)"] <- sum(stdMI$lastZoid[c(1,6)]==aZoid[c(1,6)])

			if( 1<rowLen ){
				scoreMtx[aIdx,"incRaw2"] <- freqVal.raw$filt( aZoid	,pThld=2 )$flag
				scoreMtx[aIdx,"incC2"] <- freqVal.cStep$filt( aCStep ,pThld=2  )$flag
				scoreMtx[aIdx,"incF2"] <- freqVal.fStep$filt( aFStep ,pThld=2  )$flag

				scoreMtx[aIdx,"incRaw3"] <- freqVal.raw$filt( aZoid	,pThld=3 )$flag
				scoreMtx[aIdx,"incC3"] <- freqVal.cStep$filt( aCStep ,pThld=3  )$flag
				scoreMtx[aIdx,"incF3"] <- freqVal.fStep$filt( aFStep ,pThld=3  )$flag

				scoreMtx[aIdx,"nextVal.r"] <- nextSeq.raw$filt( aZoid	)
				scoreMtx[aIdx,"nextVal.c"] <- nextSeq.cStep$filt( aCStep )
				scoreMtx[aIdx,"nextVal.f"] <- nextSeq.fStep$filt( aFStep )
			}
		} # for
	}

	return( scoreMtx )

}	# fCutU.ccc.score4

#=================================================================================
fCutU.testLab <- function( ){
	testSpan <- 500:nrow(gEnv$zhF)
	partSpan <- 3:5
		# 500:~
		#	1:3 11/324	2:4 11/324	3:5 8/324

	flag <- rep( FALSE ,length(testSpan) )	;names(flag)<-testSpan
	for( tIdx in testSpan ){
		obj <- fCutU.getChkCStepValReb( gEnv$zhF[1:(tIdx-1),partSpan] )
		aZoidMtx <- gEnv$zhF[tIdx,partSpan,drop=F]
		flag[as.character(tIdx)] <- obj$match( aZoidMtx )
	}
	table(flag)

} # fCutU.testLab( )

