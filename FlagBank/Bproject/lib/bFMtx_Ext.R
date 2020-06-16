
bFMtxExtFltLst <- list()

mName <- "score01"
if( TRUE ){
    bFMtxExtFltLst[[mName]] <- list()
    fltCreater <- function( mName ,cName=c("sc1","sc2","sc3","sc4","sc5") ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- cName

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic$score1$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName
            rVal <- sample( 0:1 ,length(rVal) ,replace=T )
            return( rVal )
        }
        fltObj$getScoreMtx <- function( scoreMtx ){
            rMtx <- matrix( 0 ,nrow=nrow(scoreMtx) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName

            for( rIdx in seq_len(nrow(rMtx)) ){
                rMtx[rIdx,] <- fltObj$getScore( scoreMtx[rIdx,] )
            }

            return( rMtx )
        }

        return(fltObj)
    }
    bFMtxExtFltLst[[mName]]$flter01 <- fltCreater(mName)
        #   bFMtxExtFltLst[[mName]]$flter01$getScoreMtx( scoreMtx.grp$basic$basic$score1$scoreMtx )
    fltCreater <- function( mName ,cName=c("sd2","sd3","sd4","sd5") ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- cName

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic$score1$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName
            rVal <- sample( 0:1 ,length(rVal) ,replace=T )
            return( rVal )
        }
        fltObj$getScoreMtx <- function( scoreMtx ){
            rMtx <- matrix( 0 ,nrow=nrow(scoreMtx) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName

            for( rIdx in seq_len(nrow(rMtx)) ){
                rMtx[rIdx,] <- fltObj$getScore( scoreMtx[rIdx,] )
            }

            return( rMtx )
        }

        return(fltObj)
    }
    bFMtxExtFltLst[[mName]]$flter02 <- fltCreater(mName)
}

mName <- "score02"
if( TRUE ){
    bFMtxExtFltLst[[mName]] <- list()
    fltCreater <- function( mName ,cName=c("ec1","ec2","ec3") ){    # $flter01

        fltObj <- list( mInfo=list() )
        fltObj$mInfo$mName = mName
        fltObj$mInfo$cName <- cName

        fltObj$getScore <- function( score ){
            #   score <- scoreMtx.grp$basic$basic$score1$scoreMtx[1,]
            rVal <- rep(0,length(fltObj$mInfo$cName))  ;names(rVal) <- fltObj$mInfo$cName
            rVal <- sample( 0:1 ,length(rVal) ,replace=T )
            return( rVal )
        }
        fltObj$getScoreMtx <- function( scoreMtx ){
            rMtx <- matrix( 0 ,nrow=nrow(scoreMtx) ,ncol=length(fltObj$mInfo$cName) )
            colnames(rMtx) <- fltObj$mInfo$cName

            for( rIdx in seq_len(nrow(rMtx)) ){
                rMtx[rIdx,] <- fltObj$getScore( scoreMtx[rIdx,] )
            }

            return( rMtx )
        }

        return(fltObj)
    }
    bFMtxExtFltLst[[mName]]$flter01 <- fltCreater(mName)
}



