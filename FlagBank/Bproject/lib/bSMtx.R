bSMtxLst <- list()

mName <- "sScore01"
if( TRUE ){

    bSMtxMaker <- function( mName ){
        rObj <- list( 	idStr=mName  ,mName=mName )

        rObj$getStdMI <- function(){

        }

        rObj$fMtxObj <- function( aZoidMtx ,zhF ){

        }

        return( rObj )
    }
    bSMtxLst[[mName]] <- bSMtxMaker( mName )

}



# ================================================================================
# = bS.StdMI
# ================================================================================

bS.stdMIMakerLst <- list()

pName <- "colVal1"
if( TRUE ){

    bS.stdMIMakerLst[[pName]] <- function( aZoidMtx ,zhF ){
        #   aZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]
        #   zhF <- gEnv$zhF
        stdMIObjLst <- list()
        mInfo <- list( idStr=pName )

        stdMILst <- list()
        for( pIdx in sort(unique(aZoidMtx[,1])) ){
            zMtx <- zhF[zhF[,1]==pIdx ,,drop=F]

            stdMI <- fCutU.getMtxInfo(zMtx)

            

            stdMILst[[as.character(pIdx)]] <- stdMI
        }

        return( list(stdMIObjLst=stdMIObjLst ,mInfo=mInfo) )
    }

}

pName <- "colVal3"
if( TRUE ){

    bS.stdMIMakerLst[[pName]] <- function( aZoidMtx ,zhF ){

    }

}

pName <- "colVal6"
if( TRUE ){

    bS.stdMIMakerLst[[pName]] <- function( aZoidMtx ,zhF ){

    }

}



