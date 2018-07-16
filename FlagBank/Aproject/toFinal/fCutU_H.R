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

# 정리되면 지울 것.
# zoidMtx <- gEnv$allZoidMtx[allIdxF,]	;logId="allZoid.idx1"
logAllZoidMtx <- function( zoidMtx ,logId ){

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
} # logAllZoidMtx()

fCutU.rptColValSeqNext <- function( gEnv ,allIdxF ,logId ){

	fileName <- sprintf("./toFinal/data/%s.txt",logId)
    FLogStr <- function( pMsg ,pTime=F ,pAppend=T ,pConsole=F ){
                k.FLogStr( pMsg ,pFile=fileName ,pTime=pTime ,pAppend=pAppend ,pConsole=pConsole )
    }

	FLogStr(sprintf("logId:%s",logId),pAppend=F)

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

	return( fileName )

} # fCutU.rptColValSeqNext( )


