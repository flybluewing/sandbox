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
bFCust.evtComp <- function( evtLevH1 ,evtLevH2 ,levMin=2 ){
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

FCust_stdCut.rawRow <- function( hName ,mName ,scoreMtxH ){

    rObj <- list( hName=hName ,mName=mName )

    cfg <- scoreMtxCfg[[mName]]

    rObj$fCol <- cfg$fCol
    rObj$isHard <- if( is.null(cfg$isHard) ) bFCust.defaultHardFlag else cfg$isHard



    rObj$available <- TRUE

    rObj$cut <- function( pName ,scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( cutLst )

        hardFlag <- rObj$isHard( pName )$flag["p"]
        anaId <- sprintf( )

        # fCol --------------------------------------------
        for( fcName in names(rObj$fCol) ){
            for( aIdx in seq_len(val.len) ){
                if( alreadyDead[aIdx] ) next

                fCol <- rObj$fCol[[fcName]]
                surWin <- fCol$rng[ ,ifelse(hardFlag,"lev1","lev2")]
                val <- scoreMtx[aIdx,fcName]
                if( !bUtil.in(val,surWin) ){
                    alreadyDead[aIdx] <- TRUE
                    if( anaMode ){
                        infoStr <- sprintf("cut Id : %s",paste(cObj$cId,collapse=",") )
                        idObjDesc <- c(hName=rObj$hName,pName=rObj$mName,fcName=fcName)
                        cutLst[[1+length(cutLst)]] <- list( idx=aIdx ,idObjDesc=idObjDesc ,info=infoStr )
                    }
                }

            }

        }

        # sm row --------------------------------------------
        evtMtx <- 
        for( aIdx in seq_len(val.len) ){
            if( alreadyDead[aIdx] ) next


        }

        return( cutLst )
    }

    return( rObj )

}
