# 작업 전달용 임시메모

curHIdx <- testSpan[1]

hMtxLst.bak <- hMtxLst

hMtxLst <- curHMtxLst
hName <- "sfcLate" ;mName <- "score2"    ;fcName <- "rebV.r" ;auxInfo=c(auxInfo="")
tgtId <- c(hName=hName,mName=mName,fcName=fcName)
fColObj <- B.getHMtxLst_byFCol( hMtxLst )
lastMtx <- fColObj[[hName]][[mName]][[fcName]]	# h * phase

mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )

scoreMtx <- mtxGrp[[mName]][[fcName]]
smRow <- scoreMtx[1,]
alreadyDead=NULL
idx <- 1






		cutterObj$cut <- function( scoreMtx ,alreadyDead=NULL ){

			val <- scoreMtx[,cutterObj$idObj["fcName"]]
			val.len <- length( val )
			if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

			cutLst <- list()
			for( idx in seq_len(val.len) ){
				if( alreadyDead[idx] )	next

				if( !bUtil.in(val[idx],cutterObj$maxMin) ){
					infoStr <- c(info=sprintf("val:%d",val[idx]))
					cutLst[[1+length(cutLst)]] <- list( idx=idx ,idObjDesc=cutterObj$idObjDesc ,info=infoStr )
				}
			}

			return( cutLst )
		} # cutterObj$cut()

					if( 0<length(cuttedLst) ){
						if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
						} else {
							cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
	                        surFlag[cut_aIdx] <- FALSE
						}
					}





