# ================================================================================
# = VP : Value Pack (aZoidMtx + stdMI)
# ================================================================================

bS.vp_ColVal <- function( gEnv, aZoidMtx, fixCol ){
    vpObj <- list( idStr=sprintf("colVal%d",fixCol)
                    ,mInfo = c(fixCol=fixCol)
    )


    colVal <- sort(unique(aZoidMtx[,fixCol]))
    stdMILst <- list()
    for( vIdx in colVal ){
        zMtx <- gEnv$zhF[ gEnv$zhF[,fixCol]==vIdx ,,drop=F]
        stdMI <- fCutU.getMtxInfo(zMtx)
        stdMI$mInfo <- c( fixCol=fixCol  ,vIdx=vIdx )
        stdMILst[[sprintf("%s_%d",vpObj$idStr,vIdx)]] <- stdMI
    }
    vpObj$stdMILst <- stdMILst

    vpObj$getCodeH <- function( stdMI ){
        wLst <- list()
        wLst$rawTail <- stdMI$rawTail[,-vpObj$mInfo["fixCol"],drop=F]
        wLst$lastRaw <- NULL
        if( 0<nrow(wLst$rawTail) ){
            wLst$lastRaw <- wLst$rawTail[nrow(wLst$rawTail),]
        }

        wLst$cStepTail <- stdMI$cStepTail
        wLst$fStepTail <- stdMI$fStepTail[,-vpObj$mInfo["fixCol"],drop=F]
        return( wLst )
    }

    vpObj$getCodeW <- function( aZoidMtx ){
        aObj <- list( miNames=names(vpObj$stdMILst) )

        miIdStr <- rep("N/A",nrow(aZoidMtx))

        cStepMtx <- aZoidMtx[,2:6]-aZoidMtx[,1:5]
        fStepMtx <- matrix( 0 ,ncol=ncol(aZoidMtx) ,nrow=nrow(aZoidMtx) )
        for( nIdx in names(vpObj$stdMILst) ){
            stdMI <- vpObj$stdMILst[[nIdx]]
            curWorkArea <- which(aZoidMtx[,stdMI$mInfo["fixCol"]]==stdMI$mInfo["vIdx"])
            miIdStr[curWorkArea] <- nIdx

            mtx <- apply(aZoidMtx[curWorkArea,,drop=F],1,function(p){ p-stdMI$lastZoid})
            fStepMtx[curWorkArea,] <-t(mtx)
        }

        aObj$miIdStr <- miIdStr
        aObj$aZoidMtx <- aZoidMtx[,-vpObj$mInfo["fixCol"],drop=F]
        aObj$cStepMtx <- cStepMtx
        aObj$fStepMtx <- fStepMtx[,-vpObj$mInfo["fixCol"],drop=F]

        rownames(aObj$aZoidMtx) <- miIdStr
        rownames(aObj$cStepMtx) <- miIdStr
        rownames(aObj$fStepMtx) <- miIdStr
        return( aObj )
    }

    return(vpObj)
}

