

bS_stdCut.rawRow <- function( hName ,mName ,pName ,scoreMtxH ){

    rObj <- list( defId=c(hName=hName,mName=mName,pName=pName) )

    hLen <- nrow(scoreMtxH)
    rObj$lastScore <- scoreMtxH[hLen,]
    rObj$available <- TRUE

    if( rObj$available ){
        cfg <- bsScoreMtxCfg[[mName]]
        if( !is.null(cfg) ){

            rObj$lastEvt <- bFCust.getEvt( rObj$lastScore ,cfg$fCol )
            if( hLen>1 ){
                evt2 <- bFCust.getEvt( scoreMtxH[hLen-1 ,] ,cfg$fCol )
                rObj$evtReb <- bFCust.evtComp( evt2["lev",] ,rObj$lastEvt["lev",] ) # ,levMin=2
                if( all(is.na(rObj$evtReb$levDup)) )    rObj$evtReb <- NULL
            }

        } else {
            rObj$available <- FALSE
        }

    }


    rObj$cut <- function( scoreMtx ,alreadyDead=NULL ,anaMode=TRUE ){

        val.len <- nrow(scoreMtx)
        if( is.null(alreadyDead) )	alreadyDead <- rep( F, val.len )

        cutLst <- list()
        if( !rObj$available ) return( list(cutLst=cutLst,surFlag=!alreadyDead) )

        # hardFlag <- rObj$isHard( rObj$defId["pName"] )$flag["p"]
        # cfg <- scoreMtxCfg[[ rObj$defId["mName"] ]]
        cat(" -- TODO --") 

        return( list(cutLst=cutLst,surFlag=!alreadyDead) )

    }

    return( rObj )

}


bS_stdCutExt.rawRow <- function( hName ,mName ,pName ,scoreMtxH ,fltName ){  
    cat(" -- TODO --") 
}



bS_stdCut.hIdx <- function( hName ,mName ,mtxLst ){


    rObj <- list( defId=c(hName=hName,mName=mName) )

    hLen <- length(mtxLst)
    rObj$lastMtx <- mtxLst[[hLen]]
    rObj$available <- TRUE

    if( rObj$available ){
        cfg <- bsScoreMtxCfg[[mName]]
        if( !is.null(cfg) ){

            stdEvt.H2 <- NULL
            if( 1<length(mtxLst) ){
            	stdEvt.H2 <- bFCust.getEvt_byHIdx( mtxLst[[length(mtxLst)-1]] ,cfg ,NULL )
            }
            rObj$stdEvt.H1 <- bFCust.getEvt_byHIdx( mtxLst[[length(mtxLst)]] ,cfg ,lastEvt=stdEvt.H2 )

            szObj.H2 <- NULL
            if( 1<length(mtxLst) ){
                mtxLst.H2 <- mtxLst[1:(length(mtxLst)-1)]
                szObj.H2 <- bFCust.getSkipZero_byHIdx( mtxLst.H2 ,cfg )
            }
            rObj$szObj <- bFCust.getSkipZero_byHIdx( mtxLst ,cfg ,lastSZ=szObj.H2 )

        } else {
            rObj$available <- FALSE
        }   # cfg

    }


    rObj$getRawScore <- function( rawMtx ){ }

    rObj$getRaw4Ass <- function( rawObj ){  }

    rObj$getSummScore <- function( rawObj ){}

    rObj$cut <- function( scoreMtx ,anaMode=TRUE ){   # 하나씩 오므로, alreadyDead 처리.
        cat(" -- TODO --") 
        return( list( cLst=cLst ,scObj=scObj ) )
    }

    return( rObj )

}



