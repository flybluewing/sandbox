#   scoreMtx.grp.lst[[rIdx]]$mf

bFMtxB.BScrLst <- list()

bFMtxB.BScrLst[["bScr01"]] <- function( stdMIObj ){

	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="bScr01"	,zMtx.size=nrow(zMtx)
					,lastZoid=stdMI$lastZoid
				)

	rObj$valPtnLst <- NULL
	if( TRUE ){
		valLst <- apply( zMtx ,2 ,function(p){sort(unique(p))} )

		valPtnLst <- list()
		for( cIdx in 1:length(valLst) ){
			infoLst <- list()
			for( cVal in valLst[[cIdx]] ){
				fIdx <- which(zMtx[,cIdx]==cVal)
				fIdx.last <- fIdx[ length(fIdx)]
				lastZoid0 <- zMtx[ fIdx.last,]
				cStep <- lastZoid0[2:6] - lastZoid0[1:5]
				infoLst[[sprintf("cVal%02d",cVal)]] <- list( lastZoid=lastZoid0 ,rem=lastZoid0%%10 ,cStep=cStep )
			}
			valPtnLst[[ names(valLst)[cIdx] ]] <- list( vals=valLst[[cIdx]] ,infoLst=infoLst )
		}

		rObj$valPtnLst <- valPtnLst
	}

	rObj$zwPtn <- NULL
	if( TRUE ){
		zwVals <- sort(unique( apply( zMtx ,1 ,function(p){p[6]-p[1]}) ))

		infoLst <- list()
		for( zwIdx in zwVals ){
			fIdx <- which(zwVals==zwIdx)
			fIdx.last <- fIdx[length(fIdx)]
			lastZoid0 <- zMtx[ fIdx.last,]
			cStep <- lastZoid0[2:6] - lastZoid0[1:5]
			infoLst[[sprintf("zw%02d",zwIdx)]] <- list( lastZoid=lastZoid0 ,rem=lastZoid0%%10 ,cStep=cStep )
		}

		rObj$zwPtn <- list( vals=zwVals ,infoLst=infoLst )
	}

    rObj$available <- TRUE

	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){

		aLen <- nrow(aZoidMtx)
		cName <- c(	"raw.1" ,"raw.3" ,"raw.4" ,"raw.6"	,"rem.1" ,"rem.2" ,"rem.3" ,"rem.4" ,"rem.5" ,"rem.6"
					,"c.1" ,"c.2" ,"c.3" ,"c.4" ,"c.5" ,"c.6"
					,"raw.ZW" ,"rem.ZW" ,"c.ZW"
				)
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}

		if( !rObj$available )	return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
            aCStep <- aZoid[2:6] - aZoid[1:5]
			aRem <- aZoid %% 10		;aZW <- aZoid[6]-aZoid[1]

			for( colIdx in 1:length(rObj$valPtnLst) ){
				fIdx <- which(rObj$valPtnLst[[colIdx]]$vals==aZoid[colIdx])
				if( 0<length(fIdx) ){
					iObj <- rObj$valPtnLst[[colIdx]]$infoLst[[fIdx]]
					if( colIdx %in% c(1,3,4,6) ){	# 2,4는 각각 1과 6을 너무 고정시키기 때문에.
						scoreMtx[aIdx,sprintf("raw.%d",colIdx)] <- sum(aZoid==iObj$lastZoid)
					}
					scoreMtx[aIdx,sprintf("rem.%d",colIdx)] <- sum(aRem==iObj$rem)
					scoreMtx[aIdx,sprintf("c.%d",colIdx)] <- sum(aCStep==iObj$cStep)
				}
			}

			fIdx <- which( rObj$zwPtn$vals==aZW )
			if( 0<length(fIdx) ){
				iObj <- rObj$zwPtn$infoLst[[fIdx]]
				scoreMtx[aIdx,sprintf("raw.ZW",colIdx)] <- sum(aZoid==iObj$lastZoid)
				scoreMtx[aIdx,sprintf("rem.ZW",colIdx)] <- sum(aRem==iObj$rem)
				scoreMtx[aIdx,sprintf("c.ZW",colIdx)] <- sum(aCStep==iObj$cStep)
			}

        }

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )

    }

	return( rObj )

} # bFMtxB.BScrLst[["bScr01"]]

bFMtxB.BScrLst[["bScr02"]] <- function( stdMIObj ){ # fCutCnt.colValSeqNext() ,anaColEndPtn( gEnv$zhF ,pDebug=T )
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="bScr02"	,zMtx.size=nrow(zMtx)
					,lastZoid=stdMI$lastZoid
				)

	# rObj$checkRawVal( ) -----------------------------
	#	rawVal sequence
	colPtnLst <- anaColEndPtn( zMtx ,pDebug=T )
	valLen <- sapply( colPtnLst ,function(p){length(p$val)} )
	banValMtx <- matrix( NA ,ncol=6 ,nrow=max(valLen) )
	for( idx in 1:length(colPtnLst) ){
		len <- length(colPtnLst[[idx]]$val)
		banValMtx[1:len,idx] <- colPtnLst[[idx]]$val
	}
	valCnt <- apply(banValMtx,1,function(p){sum(!is.na(p))})
	rObj$banValMtx <- banValMtx[valCnt>=2,]
	rObj$checkRawVal <- function( aZoid ){
		rVal <- c( lastMatch=0 ,mat2=0 ,matN=0 )
		datSize <- nrow(rObj$banValMtx)
		if( 0<datSize ){
			rVal["lastMatch"] <- sum(rObj$banValMtx[1,]==aZoid,na.rm=T)
		}
		if( 1<datSize ){
			for( idx in 2:datSize ){
				cnt <- sum(aZoid==rObj$banValMtx[idx,],na.rm=T)
				if( cnt==2 ) rVal["mat2"] <- 1 + rVal["mat2"]

				if( cnt >2 ) rVal["matN"] <- 1 + rVal["matN"]
			}
		}

		return( rVal )
	}

	#= ptn2 ==================================================
	# rObj$checkBanRem2() -----------------------------
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=2 )
	banRem2Lst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,]%%10 else integer(0)
					})
	rObj$cvSeqNextLst2.rem <- banRem2Lst
	rObj$cvSeqNextLst2 <- cvSeqNextLst
	rObj$cvSeqNextLst2.lenMax <- max( sapply(rObj$cvSeqNextLst2,function(p){nrow(p$fndMtx)}) )
	rObj$checkCvSeqNextLst2 <- function( aZoid ){

		rVal <- c( lastMatAll=0 ,lastMatTot=0 , lastMatAll.rem=0 ,lastMatTot.rem=0 ,matAll.max=0 ,matTot.max=0 )

		aRem <- aZoid %% 10
		if( 0 < rObj$cvSeqNextLst2.lenMax ){
			#	lastMatAll.rem ,lastMatTot.rem
			remLst <- rObj$cvSeqNextLst2.rem
			for( idx in 1:length(remLst) ){
				if( 2>length(remLst[[idx]]) ) next

				matCnt <- sum(aRem[0:1+idx]==remLst[[idx]])
				rVal["lastMatTot.rem"] <- matCnt + rVal["lastMatTot.rem"]

				if( 2==matCnt ) rVal["lastMatAll.rem"] <- 1 + rVal["lastMatAll.rem"]
			}

			# lastMatAll ,lastMatTot
			cvLst <- rObj$cvSeqNextLst2
			for( idx in 1:length(cvLst) ){
				if( 0==nrow(cvLst[[idx]]$fndMtx) ) next

				matCnt <- sum(aZoid[0:1+idx]==cvLst$code)
				rVal["lastMatTot"] <- matCnt + lastMatTot

				if( 2==matCnt ) rVal["lastMatAll"] <- 1 + lastMatAll["lastMatAll"]
			}
		}
		if( 1 < rObj$cvSeqNextLst2.lenMax ){
			# matAll.max ,matTot.max
			matAll <- rep( 0 ,rObj$cvSeqNextLst2.lenMax )
			matTot <- rep( 0 ,rObj$cvSeqNextLst2.lenMax )
			cvLst <- rObj$cvSeqNextLst2
			for( rIdx in 2:rObj$cvSeqNextLst2.lenMax ){
				for( cIdx in 1:length(cvLst) ){
					if( rIdx>nrow(cvLst[[cIdx]]$fndMtx) ) next

					matCnt <- sum( aZoid[0:1+cIdx]==cvLst[[cIdx]]$fndMtx[rIdx,] )
					matTot[rIdx] <- matCnt + matTot[rIdx]

					if( 2==matCnt ) matAll[rIdx] <- 1 + matAll[rIdx]
				}
			}
			rVal["matAll.max"] <- max(matAll)
			rVal["matTot.max"] <- max(matTot)
		}

		return( rVal )
	}

	# rObj$banCStep2Mtx -----------------------------
	banCStep2Lst <- lapply( cvSeqNextLst ,function(p){
		if( 0==nrow(p$fndMtx) ){	return( integer(0) )
		} else {
			return( p$fndMtx[,2]-p$fndMtx[,1] )
		}
	})
	valLen <- sapply( banCStep2Lst ,length)
	banCStep2Mtx <- matrix( NA ,ncol=5 ,nrow=max(valLen) )
	for( idx in 1:length(banCStep2Lst) ){
		len <- length(banCStep2Lst[[idx]])
		banCStep2Mtx[1:len,idx] <- banCStep2Lst[[idx]]
	}
	valCnt <- apply(banCStep2Mtx,1,function(p){sum(!is.na(p))})
	rObj$banCStep2Mtx <- banCStep2Mtx[valCnt>=2,]

	#= ptn3 ==================================================
	cvSeqNextLst <- colValSeqNext( gEnv$zhF ,pColSize=3 )
	# rObj$banRem3Lst -----------------------------
	banRem3Lst <- lapply( cvSeqNextLst, function(p){ 
						if( 0<nrow(p$fndMtx) ) p$fndMtx[1,]%%10 else integer(0)
					})
	rObj$banRem3Lst <- banRem3Lst

	# rObj$banCStep3Lst -----------------------------
	rObj$banCStep3Lst <- lapply( cvSeqNextLst ,function(p){
		if( 0==nrow(p$fndMtx) ){	return( integer(0) )
		} else {
			return( p$fndMtx[,2:3]-p$fndMtx[,1:2] )
		}
	})

    rObj$available <- TRUE

	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){

		aLen <- nrow(aZoidMtx)
		cName <- c(	"rem1.num" ,"rem1.len.tot" ,"rem1.len.val"
				)
		scoreMtx <- matrix( 0, nrow=aLen, ncol=length(cName) )	;colnames(scoreMtx) <- cName

		infoMtx <- NULL
		if( makeInfoStr ){
			cName <- c( "zMtx.size" )
			infoMtx <- matrix( "" ,nrow=aLen ,ncol=length(cName) )	;colnames(infoMtx) <- cName
			infoMtx[,"zMtx.size"] <- rObj$zMtx.size
		}

		if( !rObj$available )	return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )

		for( aIdx in 1:aLen ){
			aZoid <- aZoidMtx[aIdx,]
            aCStep <- aZoid[2:6] - aZoid[1:5]
			aRem <- aZoid %% 10		#	;aFStep <- aZoid - rObj$lastZoid



            # for( idx in 1:4 ){	# c3.x
            # 	logId <- sprintf("c3%d",idx)
            # 	scoreMtx[,logId] <- rObj$cInfo$mat3Lst[[logId]]$match( aZoidMtx[,0:2+idx,drop=F] )
            # }

        }

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )

    }

	return( rObj )

} # bFMtxB.BScrLst[["bScr02"]]




