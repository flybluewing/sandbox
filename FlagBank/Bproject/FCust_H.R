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

    rObj$isHard <- if( is.null(cfg$isHard) ) bFCust.defaultHardFlag else cfg$isHard

    hLen <- nrow(scoreMtxH)
    rObj$lastScore <- scoreMtxH[hLen,]

    if( TRUE ){
        cfg <- scoreMtxCfg[[mName]]
        rObj$lastEvt <- bFCust.getEvt( rObj$lastScore ,cfg$fCol )
        if( hLen>1 ){
            evt2 <- bFCust.getEvt( scoreMtxH[hLen-1 ,] ,cfg$fCol )
            rObj$evtReb <- bFCust.evtComp( evt2["lev",] ,rObj$lastEvt["lev",] ) # ,levMin=2
            if( all(is.na(rObj$evtReb$levDup)) )    rObj$evtReb <- NULL

        }

        rObj$checkRawReb.cnt <- sum(rObj$lastScore>0)
        rObj$checkRawReb.flag <- rObj$checkRawReb.cnt >= cfg$rowReb["rawMin"]

    }

    rObj$available <- TRUE

    rObj$cut <- function( scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( cutLst )

        hardFlag <- rObj$isHard( rObj$defId["pName"] )$flag["p"]
        anaId <- sprintf( )

        # fCol --------------------------------------------
        for( fcName in names(rObj$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( alreadyDead[aIdx] ) next

                fCol <- rObj$fCol[[fcName]]
                surWin <- fCol$rng[ ,ifelse(hardFlag,"lev1","lev2")]
                val <- scoreMtx[aIdx,fcName]
                if( !bUtil.in(val,surWin) ){
                    if( anaMode ){
                        infoStr <- sprintf("fCol:%s(%d)",fcName,val )
                        idObjDesc <- c( rObj$defId ,fcName=fcName )
                        cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=idObjDesc ,info=infoStr )
                    } else {    alreadyDead[aIdx] <- TRUE   }
                }

            }

        }

        # sm row --------------------------------------------
        cfg <- scoreMtxCfg[[rObj$mName]]
        for( aIdx in seq_len(val.len) ){

            # raw Reb
            if( alreadyDead[aIdx] ) next
            if( rObj$checkRawReb.flag ){
                if( all(rObj$lastScore==smRow) ){
                    if( anaMode ){
                        infoStr <- sprintf("RebRaw:%d",sum(smRow>0) )
                        cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=rObj$defId ,info=infoStr )
                    } else {    alreadyDead[aIdx] <- TRUE   }
                }
            }

            # evt Reb
            if( alreadyDead[aIdx] ) next
            if( 0 < sum(rObj$lastEvt["lev",]>0,na.rm=T) ){
                evt.sm <- bFCust.getEvt(smRow,cfg$fCol)
                evtComp <- bFCust.evtComp( evt.sm["lev",] ,rObj$lastEvt["lev",] )
                if( evtComp$allMat || 1<sum(!is.na(evtComp$levDup)) ){
                    if( anaMode ){
                        infoStr <- sprintf("RebEvt:%d",sum(smRow>0) )
                        cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=rObj$defId ,info=infoStr )                        
                    } else {    alreadyDead[aIdx] <- TRUE   }
                }
            }

        }

        return( cutLst )
    }

    return( rObj )

}




bFCust.getFCustGrp <- function( hMtxLst ,tgt.scMtx ){

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
                stdLst[[pName]] <- FCust_stdCut.rawRow( hName ,mName ,scoreMtxObj$scoreMtx )
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

    return( rObj )

}

