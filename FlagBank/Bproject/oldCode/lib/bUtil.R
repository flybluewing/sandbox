
bUtil.cut <- function( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,anaOnly=F ,logger=NULL ){
    #   anaOnly=T : scoreMtx[1,] 만 분석하며, 그 대신 cutting 정보를 추가한다.
	#	logger <- k.getFlogObj( "./log/cutLog.txt" )

	reportStatus <- function( tStmp ,strWhere ,surFlag ,logger ){
		#	strWhere <- sprintf("[%s,%s] stdLst",hName,mName)
		if( is.null(logger) )	return( NULL )
		tDiff <- Sys.time() - tStmp
		rptStr <- sprintf("    %s  %d/%d  %.1f%s ",strWhere,sum(surFlag),length(surFlag),tDiff,units(tDiff) )
		logger$fLogStr( rptStr )
	}

    # scoreMtx.grp <- wScoreMtx.grp ;anaOnly=T
    scMtxName <- names(cut.grp$mtxInfoLst)
	if( !is.null(tgt.scMtx) )	scMtxName <- intersect( scMtxName ,tgt.scMtx )

	# bScrMtxName
	bScrMtxName <- names(cut.grp$mtxInfoLst.bScr)
	if( !is.null(tgt.scMtx) )	bScrMtxName <- intersect( bScrMtxName ,tgt.scMtx )

    datLen <- 1
	cutInfoLst <- list()
	if( !anaOnly ){
		cutInfoLst <- NULL

		if( 0<length(scMtxName) ){
			datLen <- nrow(scoreMtx.grp$basic[[1]][[ scMtxName[1] ]]$scoreMtx)
		} else if( 0<length(bScrMtxName) ){
			datLen <- nrow(scoreMtx.grp$mf[[ bScrMtxName[1] ]]$scoreMtx)
		}
	}

	tStmp <- Sys.time()
	if( !is.null(logger) ) logger$fLogStr("Start", pTime=T ,pAppend=F )

    surFlag <- rep( T ,datLen )
    for( hName in fHName ){ # hName <- fHName[1]
        for( mName in scMtxName ){ # mName <- scMtxName[1]
            #   "stdLst" -------------------------------------------
            for( pName in cut.grp$phaseName ){   # pName <- cut.grp$phaseName[1]
                cutLst <- cut.grp$cutterLst[[hName]][[mName]]$stdLst[[pName]]
                scoreMtx <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx
                for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                    cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
					if( 0<length(cuttedLst) ){
						if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
						} else {
							cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
	                        surFlag[cut_aIdx] <- FALSE
						}
					}
                }
				reportStatus( tStmp ,sprintf("     %s",pName) ,surFlag ,logger )
            }
			reportStatus( tStmp ,sprintf("[%s,%s] stdLst",hName,mName) ,surFlag ,logger )

			#   "fColLst" ------------------------------------------
			mtxGrp <- getScoreMtx.grp_byFCol( scoreMtx.grp )
            for( fcName in cut.grp$mtxInfoLst[[mName]] ){	# fcName <- cut.grp$mtxInfoLst[[mName]][1]
				cutLst <- cut.grp$cutterLst[[hName]][[mName]]$fColLst[[fcName]]
				scoreMtx <- mtxGrp[[mName]][[fcName]]
                for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                    cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
					if( 0<length(cuttedLst) ){
						if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
						} else {
							cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
	                        surFlag[cut_aIdx] <- FALSE
						}
					}
                }
			}
			reportStatus( tStmp ,sprintf("[%s,%s] fColLst",hName,mName) ,surFlag ,logger )

			#   "hIdxLst" ------------------------------------------
			mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
			for( aIdx in seq_len(datLen) ){	# aIdx <- 1
				# 이전과 달리, 1개 aZoid에 대한 처리임을 주의.
				if( !surFlag[aIdx] && !anaOnly ) next

				cutLst <- cut.grp$cutterLst[[hName]][[mName]]$hIdxLst
				scoreMtx <- mtxGrp[[mName]][[aIdx]]
                for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                    cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,aIdx )
					if( 0<length(cuttedLst) ){
						if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
						} else {
	                        surFlag[aIdx] <- FALSE
						}
					}
                }
			}
			reportStatus( tStmp ,sprintf("[%s,%s] hIdxLst",hName,mName) ,surFlag ,logger )

        }

		for( mName in bScrMtxName ){
			cutLst <- cut.grp$cutterLst.bScr[[hName]][[mName]]$stdLst
			scoreMtx <- scoreMtx.grp$mf[[mName]]$scoreMtx
			for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
				cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
				if( 0<length(cuttedLst) ){
					if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
					} else {
						cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
						surFlag[cut_aIdx] <- FALSE
					}
				}
			}

			reportStatus( tStmp ,sprintf("[%s,%s] bScrMtx",hName,mName) ,surFlag ,logger )
		}

    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ) )

} # bUtil.cut()

