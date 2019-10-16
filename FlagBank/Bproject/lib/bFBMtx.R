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

bFMtxB.BScrLst[["bScr02"]] <- function( stdMIObj ){
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="bScr02"	,zMtx.size=nrow(zMtx)
					,lastZoid=stdMI$lastZoid
				)

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
			# aRem <- aZoid %% 10	;aFStep <- aZoid - rObj$lastZoid

            # for( idx in 1:4 ){	# c3.x
            # 	logId <- sprintf("c3%d",idx)
            # 	scoreMtx[,logId] <- rObj$cInfo$mat3Lst[[logId]]$match( aZoidMtx[,0:2+idx,drop=F] )
            # }

        }

		return( list(scoreMtx=scoreMtx,infoMtx=infoMtx) )

    }

	return( rObj )

} # bFMtxB.BScrLst[["bScr02"]]




