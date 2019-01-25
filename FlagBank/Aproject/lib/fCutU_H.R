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
	if( is.null(thld) ){
		thld <- sum(!is.na(src))
	} else if( thld>sum(!is.na(src)) ){
		return( FALSE )
	}

	src.len <- length(src)	;tgt.len <- length(tgt)
	colSpan <- 1:src.len - 1
	for( cIdx in 1:(tgt.len-src.len+1) ){
		matFlag <- tgt[cIdx+colSpan]==src
		if( thld<=sum(matFlag,na.rm=T) ){
			if( !is.null(fixIdx) && !matFlag[fixIdx] ){
				next	
			}
			return( TRUE )
		}
	}
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
	lastZoid <- zMtx[rObj$mtxLen,]
	rObj$lastZoid <- lastZoid
	rObj$rem <- lastZoid%%10

	rObj$quo10 <- fCutU.getQuoObj(lastZoid)
	rObj$cStep <- lastZoid[2:6]-lastZoid[1:5]
	rObj$fStep <- lastZoid-zMtx[rObj$mtxLen-1,]
	rObj$rawTail <- tail(zMtx)

	rObj$getCStepMtx <- function( rawMtx ){
		mtx <- apply( rawMtx ,1 ,function(zoid){zoid[2:6]-zoid[1:5]})
		return( t(mtx) )
	} # rObj$getCStepMtx()

	rObj$cStepTail <- tail(rObj$getCStepMtx(zMtx))

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
	if( pZWidth ){
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
	}
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

	rObj <- list( flgCnt=flgCnt ,scoreMtx=scoreMtx ,cStepValMtx=cStepValMtx ,lastZoid=stdMI$lastZoid )
	rObj$scoreMtx2 <- fCutU.ccc.score2( gEnv ,allIdxF ,zMtx )
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


	cName <- c("rebV","rebC","rebL","rebR","rebL.cnt","rebR.cnt")
	scoreMtx <- matrix( 0, nrow=length(allIdxF), ncol=length(cName) )
	colnames( scoreMtx ) <- cName
		#	rebV	: 값 재발 수
		#	rebC	: 동일 컬럼 재발 수
		#	rebL, rebR	: left slide reb, right slide reb

	stdMI <- fCutU.getMtxInfo( zMtx )

	if( TRUE ){	# reb3V ,reb2C ,rebL ,rebR
		slideObj <- getSlideReb( zMtx )
		for( aIdx in seq_len(length(allIdxF)) ){
			aZoid <- gEnv$allZoidMtx[allIdxF[aIdx],]
			scoreMtx[aIdx,"rebV"] <- sum( aZoid %in% stdMI$lastZoid )
			scoreMtx[aIdx,"rebC"] <- sum( aZoid == stdMI$lastZoid )

			if( !is.null(slideObj) ){
				scoreMtx[aIdx,"rebL"] <- sum( aZoid[slideObj$lMtx["col",]] == slideObj$lMtx["val",] ,na.rm=T )
				scoreMtx[aIdx,"rebR"] <- sum( aZoid[slideObj$rMtx["col",]] == slideObj$rMtx["val",] ,na.rm=T )
				scoreMtx[aIdx,"rebL.cnt"] <- sum( !is.na(slideObj$lMtx["val",]) )
				scoreMtx[aIdx,"rebR.cnt"] <- sum( !is.na(slideObj$rMtx["val",]) )
			}
		}
	}	# reb3V ,reb2C
	#	dbgN<-"rebV"	;table(scoreMtx[,dbgN])	;dbgIdx<-head(which(scoreMtx[,dbgN]==1))	;aIdx<-dbgIdx[1]

	return( scoreMtx )

}	# fCutU.ccc.score2


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

