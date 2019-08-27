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





if( FALSE ){	# test fCutU.getFiltObjPair( )

	temp.getCaptureRpt <- function( captureStr ,indent="    " ){
		captureStr <- paste( indent ,captureStr )
		captureStr <- paste( captureStr ,collapse="\n" )
		return( captureStr )
	}

    logger <- k.getFlogObj( "./log/Test_fCutU.getFiltObjPair.txt" )
	logger$fLogStr( "Test fCutU.getFiltObjPair( )", pAppend=F ,pTime=T )

	stdZoid <- c( 14,18,22,26,31,44 )
    stdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst ) # B.rptStdMI.grp( stdMI.grp )   ;stdMI.grp$anyWarn()
	for( pName in names(stdMI.grp$basic) ){	# pName <- names(stdMI.grp$basic)[1]
		logger$fLogStr( sprintf("< %s > =====================================================",pName) )
		stdMI <- stdMI.grp$basic[[pName]]$stdMI
        dfStr <- capture.output( anaMtx(stdMI$rawTail,NULL) )
        logger$fLogStr( sprintf("%s \n",temp.getCaptureRpt( dfStr )) )

		stdCStep <- stdZoid[2:6] - stdZoid[1:5]
		stdFStep <- stdZoid - stdMI$lastZoid
		stdRem <- stdZoid %% 10

		logger$fLogStr( "[RawTail]" )
		obj <- fCutU.getFiltObjPair( stdMI$rawTail )
		dfStr <- capture.output( obj$explain() )
        logger$fLogStr( sprintf("%s \n",temp.getCaptureRpt( dfStr )) )
		logger$fLogStr( sprintf("         obj$filt( stdZoid )  # %s",paste(stdZoid,collapse=",")) )
		dfStr <- capture.output( obj$filt(stdZoid) )
		logger$fLogStr( sprintf("%s \n",temp.getCaptureRpt( dfStr,indent="             " )) )

		logger$fLogStr( "[CStepTail]" )
		obj <- fCutU.getFiltObjPair( stdMI$cStepTail )
		dfStr <- capture.output( obj$explain() )
        logger$fLogStr( sprintf("%s \n",temp.getCaptureRpt( dfStr )) )
		logger$fLogStr( sprintf("         obj$filt( stdCStep )  # %s",paste(stdCStep,collapse=",")) )
		dfStr <- capture.output( obj$filt(stdCStep) )
		logger$fLogStr( sprintf("%s \n",temp.getCaptureRpt( dfStr,indent="             " )) )

		# logger$fLogStr( "[FStepTail]" )
		# obj <- fCutU.getFiltObjPair( stdMI$fStepTail )
		# dfStr <- capture.output( obj$explain() )
        # logger$fLogStr( sprintf("%s \n",temp.getCaptureRpt( dfStr )) )
		# logger$fLogStr( sprintf("         obj$filt( stdFStep )  # %s",paste(stdFStep,collapse=",")) )
		# dfStr <- capture.output( obj$filt(stdFStep) )
		# logger$fLogStr( sprintf("%s \n",temp.getCaptureRpt( dfStr,indent="             " )) )

		# logger$fLogStr( "[rem]" )
		# obj <- fCutU.getFiltObjPair( stdMI$rawTail %% 10 )
		# dfStr <- capture.output( obj$explain() )
        # logger$fLogStr( sprintf("%s \n",temp.getCaptureRpt( dfStr )) )
		# logger$fLogStr( sprintf("         obj$filt( stdRem )  # %s",paste(stdRem,collapse=",")) )
		# dfStr <- capture.output( obj$filt(stdRem) )
		# logger$fLogStr( sprintf("%s \n",temp.getCaptureRpt( dfStr,indent="             " )) )
	}

}	# test fCutU.getFiltObjPair( )