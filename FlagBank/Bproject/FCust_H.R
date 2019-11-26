bFCust.defaultHardFlag <- function( pName ){
    hardPh	<- c("basic")
    pFlag <- pName %in% hardPh

	flag <- c( p=pFlag )
	return( list( flag=flag ) )
}

bFCust.getEvt <- function( smRow ,fCol ){
    #   evt : evt define matrix. colname:("lev","val")

    evtMtx <- matrix( NA, nrow=2 ,ncol=length(smRow) )
    colnames( evtMtx ) <- names(smRow)
    rownames( evtMtx ) <- c( "val" ,"lev" )
    for( fcName in names(fCol) ){
        fndIdx <- which( smRow[fcName]==fCol[[fcName]]$evt[,"val"] )
        if( 0==length(fndIdx) ){
            evtMtx[,fcName] <- NA
        } else {
            evtMtx["val",fcName] <- smRow[fcName]
            evtMtx["lev",fcName] <- fCol[[fcName]]$evt[fndIdx,"lev"]
        }
    }

    return( evtMtx )
}
bFCust.evtComp <- function( evtLevH1 ,evtLevH2 ,levMin=1 ){
    # evtLevH1가 가장 최신, evtLevH2는 그 이전.

    matFlag <- evtLevH1 == evtLevH2
    matFlag[is.na(matFlag) | evtLevH1<levMin ] <- FALSE

    # evtLevDup
    evtLevDup <- evtLevH1
    evtLevDup[!matFlag] <- NA

    allMat <- FALSE
    evt1Cnt <- sum( (evtLevH1>=levMin) ,na.rm=T )
    evt2Cnt <- sum( (evtLevH2>=levMin) ,na.rm=T )
    if( evt1Cnt==evt2Cnt ){
        allMat <- evt1Cnt == sum( !is.na(evtLevDup) )
    }
    
    return( list( levDup=evtLevDup ,allMat=allMat ) )
}

FCust_stdCut.rawRow <- function( hName ,mName ,pName ,scoreMtxH ){

    rObj <- list( defId=c(hName=hName,mName=mName,pName=pName) )

    hLen <- nrow(scoreMtxH)
    rObj$lastScore <- scoreMtxH[hLen,]
    rObj$available <- TRUE

    if( TRUE ){
        cfg <- scoreMtxCfg[[mName]]
        if( !is.null(cfg) ){
            rObj$isHard <- if( is.null(cfg$isHard) ) bFCust.defaultHardFlag else cfg$isHard

            rObj$lastEvt <- bFCust.getEvt( rObj$lastScore ,cfg$fCol )
            if( hLen>1 ){
                evt2 <- bFCust.getEvt( scoreMtxH[hLen-1 ,] ,cfg$fCol )
                rObj$evtReb <- bFCust.evtComp( evt2["lev",] ,rObj$lastEvt["lev",] ) # ,levMin=2
                if( all(is.na(rObj$evtReb$levDup)) )    rObj$evtReb <- NULL

            }

            rObj$checkRawReb.cnt <- sum(rObj$lastScore>0)
            rObj$checkRawReb.flag <- rObj$checkRawReb.cnt >= cfg$rowReb["rawMin"]
        } else {
            rObj$available <- FALSE
        }   # cfg

    }


    rObj$cut <- function( scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( list(cutLst=cutLst,surFlag=!alreadyDead) )

        hardFlag <- rObj$isHard( rObj$defId["pName"] )$flag["p"]
        cfg <- scoreMtxCfg[[ rObj$defId["mName"] ]]

        # each fCol --------------------------------------------
        cutLst.fCol <- list()
        for( fcName in names(cfg$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( !anaMode && alreadyDead[aIdx] ) next

                surWin <- cfg$fCol[[fcName]]$rng[ ,ifelse(hardFlag,"lev1","lev2")]
                val <- scoreMtx[aIdx,fcName]
                if( !bUtil.in(val,surWin) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("%s(%d)",fcName,val )
                    cObj <- cutLst.fCol[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.fCol[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.fCol[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                    # infoStr <- sprintf("val:%d",val )
                    # idObjDesc <- c( typ="rawFCol" ,rObj$defId ,fcName=fcName )
                    # cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=idObjDesc ,info=infoStr )
                }

            }

        }

        # sm row: evtCnt  --------------------------------------------
        cutLst.rowE <- list()

        # sm row: rebound --------------------------------------------
        cutLst.reb <- list()
        for( aIdx in seq_len(val.len) ){
            smRow <- scoreMtx[aIdx ,]

            # raw Reb
            if( !anaMode && alreadyDead[aIdx] ) next
            if( rObj$checkRawReb.flag ){
                if( all(rObj$lastScore==smRow) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("rReb:%d",sum(smRow>0) )
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                    # infoStr <- sprintf("rReb:%d",sum(smRow>0) )
                    # idObjDesc <- c( typ="rawReb" ,rObj$defId )
                    # cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=idObjDesc ,info=infoStr )
                }
            }

            # evt Reb
            if( !anaMode && alreadyDead[aIdx] ) next
            if( 0 < sum(rObj$lastEvt["lev",]>0,na.rm=T) ){
                evt.sm <- bFCust.getEvt(smRow,cfg$fCol)
                evtComp <- bFCust.evtComp( evt.sm["lev",] ,rObj$lastEvt["lev",] )
                if( evtComp$allMat || 1<sum(!is.na(evtComp$levDup)) ){
                    alreadyDead[aIdx] <- TRUE

                    infoStr <- sprintf("eReb:%d",sum(!is.na(rObj$lastEvt["lev",])) )
                    cObj <- cutLst.reb[[as.character(aIdx)]]
                    if( is.null(cObj) ){
                        cutLst.reb[[as.character(aIdx)]] <- list( idx=aIdx ,info=infoStr )
                    } else {
                        cutLst.reb[[as.character(aIdx)]]$info <- paste( cObj$info, infoStr, collapse=", " )
                    }

                    # infoStr <- sprintf("eReb:%d",sum(smRow>0) )
                    # idObjDesc <- c( typ="rawEvtReb" ,rObj$defId )
                    # cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=idObjDesc ,info=infoStr )
                }
            }

        }

        # browser( TRUE )
        if( anaMode ){  # build cutLst 
            idxFCol <- if( length(cutLst.fCol)==0 ) integer(0) else sapply( cutLst.fCol  ,function(p){p$idx} )
            idxReb  <- if( length(cutLst.reb)==0 ) integer(0) else sapply( cutLst.reb   ,function(p){p$idx} )
            idxRowE <- if( length(cutLst.rowE)==0 ) integer(0) else sapply( cutLst.rowE  ,function(p){p$idx} )

            idxAll <- union(idxFCol,idxReb)
            idxAll <- sort(union(idxAll,idxRowE))

            cutLst <- list()
            names(cutLst.fCol)  <- idxFCol
            names(cutLst.reb)   <- idxReb
            names(idxRowE)      <- idxRowE

            for( aIdx in idxAll ){
                idStr <- as.character(aIdx)

                cLst <- list()
                if( !is.null(cutLst.fCol[[idStr]]) ){
                    cLst[["rawFCol"]] <- cutLst.fCol[[idStr]]
                    cLst[["rawFCol"]]$idObjDesc <- c( typ="rawFCol" ,rObj$defId )
                }
                if( !is.null(cutLst.reb[[idStr]]) ){
                    cLst[["rawReb"]] <- cutLst.reb[[idStr]]
                    cLst[["rawReb"]]$idObjDesc <- c( typ="rawReb" ,rObj$defId )
                }
                if( !is.null(cutLst.rowE[[idStr]]) ){
                    cLst[["rawE"]] <- cutLst.rowE[[idStr]]
                    cLst[["rawE"]]$idObjDesc <- c( typ="rawE" ,rObj$defId )
                }

                cutLst[[idStr]] <- list( idx=aIdx ,cLst=cLst )
            }
        }

        return( list(cutLst=cutLst,surFlag=!alreadyDead) )
    }

    return( rObj )

}




bFCust.getFCustGrp <- function( hMtxLst ,tgt.scMtx=NULL ){

    rObj <- list(   sfcHLst = hMtxLst$sfcHLst
                    ,mtxInfoLst = hMtxLst$mtxInfoLst
					,mtxInfoLst.bScr = hMtxLst$mtxInfoLst.bScr
                    ,phaseName = hMtxLst$phaseName
    )

	# custObj <- bFCust.getCust()
    if( !is.null(tgt.scMtx) ){
        availMtx <- intersect( tgt.scMtx ,names(rObj$mtxInfoLst) )
        rObj$mtxInfoLst <- rObj$mtxInfoLst[availMtx]

        availMtx <- intersect( tgt.scMtx ,names(rObj$mtxInfoLst.bScr) )
        rObj$mtxInfoLst.bScr <- rObj$mtxInfoLst.bScr[availMtx]
    }

	cutterLst <- list()
	cutterLst.bScr <- list()
	for( hName in names(rObj$sfcHLst) ){	# hName <- names(rObj$sfcHLst)[1]
        mLst <- list()
        for( mName in names(rObj$mtxInfoLst) ){	# mName <- names(rObj$mtxInfoLst)[1]
            stdLst <- list()
            for( pName in rObj$phaseName ){
                scoreMtxObj <- B.HMtxLst_getMtxLst( hMtxLst , hName ,mName ,pName )
                stdLst[[pName]] <- FCust_stdCut.rawRow( hName ,mName ,pName ,scoreMtxObj$scoreMtx )
            }
            fColLst <- list()   # preserve
            hIdxLst <- list()   # preserve
            mLst[[mName]] <- list( stdLst=stdLst ,fColLst=fColLst ,hIdxLst=hIdxLst )
        }
        cutterLst[[hName]] <- mLst

		mLst <- list()
		for( mName in names(rObj$mtxInfoLst.bScr) ){

        }
		cutterLst.bScr[[hName]] <- mLst

    }

    rObj$cutterLst <- cutterLst
    rObj$cutterLst.bScr <- cutterLst.bScr

    return( rObj )

}


bFCust.cut <- function( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=NULL ,anaOnly=F ,logger=NULL ){
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
            for( pName in cut.grp$phaseName ){   # pName <- cut.grp$phaseName[1]
                cutterObj <- cut.grp$cutterLst[[hName]][[mName]]$stdLst[[pName]]
                scoreMtx <- scoreMtx.grp$basic[[pName]][[mName]]$scoreMtx

                rst <- cutterObj$cut( scoreMtx ,!surFlag )
                if( 0<length(rst$cutLst) ){
                    if( anaOnly ){	
                        # cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,info=cuttedLst[[1]]$info )
                        cutInfoLst[[1+length(cutInfoLst)]] <- lapply( rst$cutLst[[1]]$cLst ,function(p){ c(p$idObjDesc, info=p$info) })

                    } else {
                        cut_aIdx <- sapply( rst$cutLst ,function(p){p$idx} )
                        surFlag[cut_aIdx] <- FALSE
                    }
                }

                # for( cnIdx in names(cutLst) ){  # cnIdx <- names(cutLst)[1]
                #     cuttedLst <- cutLst[[cnIdx]]$cut( scoreMtx ,!surFlag )
				# 	if( 0<length(cuttedLst) ){
				# 		if( anaOnly ){	cutInfoLst[[1+length(cutInfoLst)]] <- c( cuttedLst[[1]]$idObjDesc ,cuttedLst[[1]]$info )
				# 		} else {
				# 			cut_aIdx <- sapply( cuttedLst ,function(p){p$idx} )
	            #             surFlag[cut_aIdx] <- FALSE
				# 		}
				# 	}
                # }

				reportStatus( tStmp ,sprintf("     %s",pName) ,surFlag ,logger )
            }
			reportStatus( tStmp ,sprintf("[%s,%s] stdLst",hName,mName) ,surFlag ,logger )
        }

		for( mName in bScrMtxName ){
			reportStatus( tStmp ,sprintf("[%s,%s] bScrMtx",hName,mName) ,surFlag ,logger )
        }
    }

    return( list( surFlag=surFlag ,cutInfoLst=cutInfoLst ) )
}


