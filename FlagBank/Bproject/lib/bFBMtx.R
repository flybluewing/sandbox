#   scoreMtx.grp.lst[[rIdx]]$mf

bFMtxB.BScrLst <- list()

bFMtxB.BScrLst[["bScr01"]] <- function( stdMIObj ){
	stdMI <- stdMIObj$stdMI
	zMtx <- stdMIObj$zMtx
	rObj <- list( 	idStr="bScr01"	,zMtx.size=nrow(zMtx)
					,lastZoid=stdMI$lastZoid
				)

    rObj$available <- TRUE

	rObj$fMtxObj <- function( aZoidMtx ,makeInfoStr=F ){

		aLen <- nrow(aZoidMtx)
		cName <- c(	"rem0.num" ,"rem0.len.tot" ,"rem0.len.val"
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




