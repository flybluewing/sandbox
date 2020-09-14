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

if( TRUE ){ # "colVal1"
    bS.stdMIMakerLst[["colVal1"]] <- function( aZoidMtx ,zhF ){
        #   aZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]
        #   zhF <- gEnv$zhF

        mInfo <- list( idStr="colVal1" )

        stdMILst <- list()
        for( pIdx in sort(unique(aZoidMtx[,1])) ){
            zMtx <- zhF[zhF[,1]==pIdx ,,drop=F]

            stdMI <- fCutU.getMtxInfo(zMtx)
            stdMI$mInfo <- c(idStr=sprintf("colVal1(%d)",pIdx))
            stdMILst[[as.character(pIdx)]] <- stdMI
        }

        return( list(stdMILst=stdMILst ,mInfo=mInfo) )
    }
}

if( TRUE ){ # "colVal3"
    bS.stdMIMakerLst[["colVal3"]] <- function( aZoidMtx ,zhF ){
        #   aZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]
        #   zhF <- gEnv$zhF

        mInfo <- list( idStr="colVal3" )

        stdMILst <- list()
        for( pIdx in sort(unique(aZoidMtx[,1])) ){
            zMtx <- zhF[zhF[,3]==pIdx ,,drop=F]

            stdMI <- fCutU.getMtxInfo(zMtx)
            stdMI$mInfo <- c(idStr=sprintf("colVal3(%d)",pIdx))
            stdMILst[[as.character(pIdx)]] <- stdMI
        }

        return( list(stdMILst=stdMILst ,mInfo=mInfo) )
    }
}

if( TRUE ){ # "colVal6"
    bS.stdMIMakerLst[["colVal6"]] <- function( aZoidMtx ,zhF ){
        #   aZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]
        #   zhF <- gEnv$zhF

        mInfo <- list( idStr="colVal6" )

        stdMILst <- list()
        for( pIdx in sort(unique(aZoidMtx[,1])) ){
            zMtx <- zhF[zhF[,6]==pIdx ,,drop=F]

            stdMI <- fCutU.getMtxInfo(zMtx)
            stdMI$mInfo <- c(idStr=sprintf("colVal6(%d)",pIdx))
            stdMILst[[as.character(pIdx)]] <- stdMI
        }

        return( list(stdMILst=stdMILst ,mInfo=mInfo) )
    }
}




# kLst <- lapply( bS.stdMIMakerLst ,function( pFunc ){ pFunc( aZoidMtx, zhF ) })



