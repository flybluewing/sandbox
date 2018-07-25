# fCutU_H.R 최종접근

fCutU.hasPtn <- function( src ,tgt ){ # < official >

	src.len <- length(src)	;tgt.len <- length(tgt)
	colSpan <- 1:src.len - 1
	for( cIdx in 1:(tgt.len-src.len+1) ){
		if( all(tgt[cIdx+colSpan]==src) ){
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
} # fCutU.getNextZW()

fCutU.getNextCStepBin <- function( gEnv ){ # < official >
	hLen <- nrow( gEnv$zhF )
	cStepMtx <- t( apply(gEnv$zhF ,1 ,function(zoid){(zoid[2:6]-zoid[1:5])}) )
	lastBin <- cStepMtx[hLen,] %% 2

	flag <- sapply( 1:(hLen-1) ,function( hIdx ){ all((cStepMtx[hIdx,]%%2)==lastBin) })
	flag.idx <- which( flag )+1

	rObj <- list( zMtx=gEnv$zhF[flag.idx,] ,lastBin=lastBin )
	return( rObj )
} # fCutU.getNextZW()

fCutU.getRebNum <- function( gEnv ,rebNum=0 ){ # < official >

	hLen <- nrow( gEnv$zhF )

	rebCnt <- sapply( 2:hLen ,function( hIdx ){ sum(gEnv$zhF[(hIdx-1),] %in% gEnv$zhF[hIdx,]) })
	rebCnt <- c( 0 ,rebCnt )
	flag.idx <- which( rebCnt==rebNum )

	rObj <- list( zMtx=gEnv$zhF[flag.idx,] ,rebCnt=rebCnt )
	return( rObj )

} # fCutU.getNextZW()

fCutU.getNextRebNumPtn <- function( gEnv ,numPtn ){	# <official>
	rebNum <- sapply( 2:nrow(gEnv$zhF) ,function(hIdx){ sum(gEnv$zhF[(hIdx-1),]%in%gEnv$zhF[hIdx,]) } )
	rebNum <- c( 0 ,rebNum )	;names(rebNum) <- 1:length(rebNum)
	rebNumLen <- length(rebNum)
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

fCutU.commonCutCnt <- function( gEnv, allIdxF ,zMtx
						,pZWidth=TRUE	,pQuoTbl=TRUE	,pRebTwo=TRUE
						,rpt=FALSE
					 ) {

	flgCnt <- rep( 0 ,length(allIdxF) )
	stdMI <- fCutU.getMtxInfo( zMtx )
		# mtxLen lastZoid rem quo10 cStep fStep rawTail cStepTail

	if(pRebTwo){
		# <remove>
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						return( 2>sum(stdMI$lastZoid%in%aZoid) )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 2
	}

	if( pQuoTbl ){
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoTbl <- table(aZoid%/%10)
						return( !stdMI$quo10$sameTbl(quoTbl) )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
		stdQuo <- fCutU.chkRowPtnReb(stdMI$quoTail)
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						quoSize <- fCutU.getQuoObj(aZoid)$size
						return( !stdQuo$filt(quoSize)$filted )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
	}
	if( pZWidth ){
		lastZW <- stdMI$lastZoid[6] - stdMI$lastZoid[1]
		flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
						return( lastZW!=(aZoid[6]-aZoid[1]) )
					})	;kIdx<-anaFlagFnd(!flag,rpt)
		flgCnt[!flag] <- flgCnt[!flag] + 1
	}

    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					return( 1>sum(stdMI$lastZoid==aZoid) )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					aCode <- aZoid%%10
					for( cIdx in 1:4 ){		# rem ptn 재현 3이상
						fnd <- fCutU.hasPtn( stdMI$rem[cIdx+0:2] ,aCode )
						if( fnd ) return( FALSE )
					}
					return( TRUE )
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1
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
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
					cnt <- sum( stdMI$cStep==(aZoid[2:6]-aZoid[1:5]) )
					return( 3 > cnt )	# cStep 반복 전체갯수는 3개 이하.(2가 너무 많다보니 변경.)
				})	;kIdx<-anaFlagFnd(!flag,rpt)
    flgCnt[!flag] <- flgCnt[!flag] + 1

	return( flgCnt )
} #	fCutU.commonCutCnt( )

