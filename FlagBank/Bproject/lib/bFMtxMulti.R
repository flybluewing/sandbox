#   multi-filter



bFMtxMFltLst <- list()

mfName <- "mfABCD"
if( TRUE ){

    bFMtxMFltLst[[mfName]] <- function( ){

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mfName

        fltObj$fltMNames <- c("scoreA","scoreB","scoreC","scoreD")
        fltObj$mInfo$cName <- c("xxx","xxx")

        fltObj$getScore <- function( scrMtxLst ){

        }
        fltObj$getScoreMtx <- function( scoreMtx.grp ){
            # scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )

            scrMtxLst <- lapply( scoreMtx.grp$basic ,function( mtxObj ){
                rLst <- list(    scoreA=mtxObj$scoreA$scoreMtx 
                                ,scoreB=mtxObj$scoreB$scoreMtx 
                                ,scoreC=mtxObj$scoreC$scoreMtx 
                                ,scoreD=mtxObj$scoreD$scoreMtx 
                )
                return( rLst )
            })

            rMtx <- matrix( 0 ,nrow=nrow(scrMtxLst[["scoreA"]]) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName

            return( rMtx )
        }

    }

}

