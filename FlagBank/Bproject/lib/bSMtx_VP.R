# ================================================================================
# = VP : Value Pack (aZoidMtx + stdMI)
# ================================================================================
if( FALSE ){    # document
    # 주의사항
    #   - vpObj$getCodeW() 에서 fStep 생성 시, aZoid마다 해당되는 stdMI가 다르다!
}


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

        cStepMtx <- aZoidMtx[,2:6,drop=F]-aZoidMtx[,1:5,drop=F]
        fStepMtx <- matrix( 0 ,ncol=ncol(aZoidMtx) ,nrow=nrow(aZoidMtx) )
        for( nIdx in names(vpObj$stdMILst) ){
            stdMI <- vpObj$stdMILst[[nIdx]]
            if(0==nrow(stdMI$rawTail))  next

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

bS.vp_remPair <- function( gEnv, aZoidMtx ){

    vpObj <- list( idStr=sprintf("remPair")
                    ,mInfo = c()
    )
    vpObj$checkPair <- function( remRow ,pairIdx ){

        val <- remRow[pairIdx]
        if( val[1]!=val[2] )    return( FALSE )

        if( any(val[1]==remRow[-pairIdx] ) ) return( FALSE )
        
        return( TRUE )
    }

    # aZoidMtx 내 pair group 검색
    #   하나의 aZoid에서 2개 이상 pair 발생 가능함을 고려해야 함.
    #   때문에 stdMILst의 workSpan은 서로 겹치는 부분이 있을 수가 없다.(bSMtx로부터의 score 값이 유일해야 하므로.)
    #   따라서 aZoid내에 다수 pair가 존재하더라도, 하나의 pair만 인정하기로 한다.
    #   aZoidMtx 전체적으로 많이 발생한 pair 패턴에 우선권 부여필요.

    pairInfoLst <- apply( aZoidMtx%%10 ,1 ,function( aCode ){
        pairLst <- bUtil.findSeg( aCode ,pairMaxLen=2 ,useValName=FALSE )

        # if( 0==length(pairLst) )    return(pairLst)

        # pairId <- character(0)
        # for( idx in seq_len(length(pairLst)) ){
        #     pairId <- c( pairId ,paste(pairLst[[idx]]$idx,collapse="_") )
        # }
        # names(pairLst) <- pairId

        return( pairLst )
    })

    # 발생 횟수가 가장 많은 pair 검색.
    pairIdStrLst <- lapply( pairInfoLst ,function( piLst ){
        return(names(piLst))
    })
    idStr <- do.call( c ,pairIdStrLst )
    idStrTbl <- sort( table(idStr) ,decreasing=T )

    leftFlag <- rep( T ,nrow(aZoidMtx) )
    workSpanLst <- list()
    for( grpIdx in names(idStrTbl) ){   # 실행효율보다, 로직이 보이는 쪽으로 코딩한다. ^^;
        fndFlag <- sapply( pairIdStrLst ,function(pairId){ grpIdx %in% pairId })
        workFlag <- leftFlag & fndFlag
        workSpanLst[[grpIdx]] <- which( workFlag )

        leftFlag[fndFlag] <- FALSE
    }

    # 발견된 pair group 별로 stdMILst 생성.
    stdMILst <- list()
    remH <- gEnv$zhF %% 10
    for( nIdx in names(workSpanLst) ){
        if( 0==length(workSpanLst[[nIdx]]) ) next

        pairInfo <- pairInfoLst[[ workSpanLst[[nIdx]][1] ]][[nIdx]]
        
        fndFlag <- apply( remH ,1 ,function( remRow ){ 
            return( vpObj$checkPair( remRow ,pairInfo$idx ) )
        })

        zMtx <- gEnv$zhF[ fndFlag ,,drop=F]
        stdMI <- fCutU.getMtxInfo(zMtx)
        stdMI$pairIdx <- pairInfo$idx
        stdMILst[[nIdx]] <- stdMI
    }

    vpObj$stdMILst <- stdMILst

    vpObj$getCodeH <- function( stdMI ){
        wLst <- list()
        pairIdx <- stdMI$pairIdx

        wLst$rawTail <- stdMI$rawTail[,-pairIdx[1],drop=F]
        wLst$lastRaw <- NULL
        if( 0<nrow(wLst$rawTail) ){
            wLst$lastRaw <- wLst$rawTail[nrow(wLst$rawTail),]
        }

        wLst$cStepTail <- stdMI$cStepTail[,-pairIdx[1] ,drop=F]
        wLst$fStepTail <- stdMI$fStepTail[,-pairIdx[1] ,drop=F]
        return( wLst )
    }

    vpObj$getCodeW <- function( aZoidMtx ){
        aObj <- list( miNames=names(vpObj$stdMILst) )

        miIdStr <- rep("N/A",nrow(aZoidMtx))    ;names(miIdStr) <- rownames(aZoidMtx)
            #   aZoid 자체는 다수의 pair 패턴에 속할 수 있다.
            #   따라서 가장 많은 pair 패턴에 우선 배당하기로 한다.(즉 stdMILst 순서를 따름.)

        aLen <- nrow(aZoidMtx)
        aCodeMtx <- aZoidMtx %% 10
        leftFlag <- rep( TRUE ,aLen )   ;names(leftFlag) <- names(miIdStr)
        for( nIdx in names(vpObj$stdMILst) ){
            stdMI <- vpObj$stdMILst[[nIdx]]
            for( aIdx in seq_len(aLen) ){
                if( !leftFlag[aIdx] )   next

                if( !vpObj$checkPair( aCodeMtx[aIdx,] ,stdMI$pairIdx ) )    next

                leftFlag[aIdx] <- FALSE
                miIdStr[aIdx] <- nIdx
            }
        }

        cStepMtx <- aZoidMtx[,2:6 ,drop=F] - aZoidMtx[,1:5 ,drop=F]

        aObj$miIdStr <- miIdStr
        aObj$aZoidMtx <- matrix( 0 ,nrow=aLen ,ncol=(6-1) )  ;rownames(aObj$aZoidMtx) <- miIdStr
        aObj$cStepMtx <- matrix( 0 ,nrow=aLen ,ncol=(5-1) )  ;rownames(aObj$cStepMtx) <- miIdStr
        aObj$fStepMtx <- matrix( 0 ,nrow=aLen ,ncol=(6-1) )  ;rownames(aObj$fStepMtx) <- miIdStr
        for( nIdx in names(vpObj$stdMILst) ){
            startIdx <- vpObj$stdMILst[[nIdx]]$pairIdx[1]
            curWorkArea <- which(miIdStr==nIdx)
            aObj$aZoidMtx[curWorkArea,] <- aZoidMtx[curWorkArea,-startIdx ,drop=F]
            aObj$cStepMtx[curWorkArea,] <- cStepMtx[curWorkArea,-startIdx ,drop=F]

            tMtx <- apply( aZoidMtx[curWorkArea,,drop=F] ,1 ,function( aZoid ){
                                aZoid - vpObj$stdMILst[[nIdx]]$lastZoid
            })
            fStepMtx <- t(tMtx)

            aObj$fStepMtx[curWorkArea,] <- fStepMtx[,-startIdx ,drop=F]
        }

        return( aObj )
    }


    return(vpObj)
}


bS.vp_zw <- function( gEnv, aZoidMtx ){

    vpObj <- list( idStr=sprintf("zw")
                    ,mInfo = c()
    )

    zwGrp <- apply( aZoidMtx ,1 ,function(aZoid){ aZoid[6]-aZoid[1] })
    zwGrp <- sort(unique(zwGrp))

    stdMILst <- list()
    zwH <- apply( gEnv$zhF ,1 ,function(zh){ zh[6]-zh[1] })
    for( zwIdx in zwGrp ){
        zMtx <- gEnv$zhF[zwH==zwIdx, ,drop=F]
        if( 0==nrow(zMtx) ) next

        stdMI <- fCutU.getMtxInfo(zMtx)
        stdMI$zw <- zwIdx
        stdMILst[[sprintf("%d",zwIdx)]] <- stdMI
    }
    vpObj$stdMILst <- stdMILst


    vpObj$getCodeH <- function( stdMI ){
        wLst <- list()
        wLst$rawTail <- stdMI$rawTail[,1:5]
        wLst$lastRaw <- wLst$rawTail[nrow(wLst$rawTail),]

        wLst$cStepTail <- stdMI$cStepTail
        wLst$fStepTail <- stdMI$fStepTail[,1:5]

        return( wLst )
    }

    vpObj$getCodeW <- function( aZoidMtx ){
        aObj <- list( miNames=names(vpObj$stdMILst) )

        miIdStr <- rep("N/A",nrow(aZoidMtx))

        aZw <- apply( aZoidMtx ,1 ,function(aZoid){ aZoid[6]-aZoid[1] })

        fStepMtx <- matrix( 0 ,nrow=nrow(aZoidMtx) ,ncol=6 )
        for( nIdx in names(vpObj$stdMILst) ){
            stdMI <- vpObj$stdMILst[[nIdx]]
            curWorkSpan <- aZw==stdMI$zw
            
            mtx <- apply( aZoidMtx[curWorkSpan ,,drop=F] ,1 ,function(aZoid){ aZoid-stdMI$lastZoid })
            fStepMtx[curWorkSpan ,] <- t(mtx)

            miIdStr[curWorkSpan] <- nIdx
        }

        aObj$miIdStr <- miIdStr
        aObj$aZoidMtx <- aZoidMtx[,1:5,drop=F]
        aObj$cStepMtx <- aZoidMtx[,2:6,drop=F] - aZoidMtx[,1:5,drop=F]
        aObj$fStepMtx <- fStepMtx[,1:5,drop=F]

        rownames(aObj$aZoidMtx) <- miIdStr
        rownames(aObj$cStepMtx) <- miIdStr
        rownames(aObj$fStepMtx) <- miIdStr

        return( aObj )
    }

    return(vpObj)

}

bS.vp_cSCVal <-function( gEnv ,aZoidMtx ,fixCol ){  # ColVal for cStep
    vpObj <- list( idStr=sprintf("cSCVal%d",fixCol)
                    ,mInfo = c(fixCol=fixCol)
    )

    cStepMtx <- aZoidMtx[,2:6,drop=F] - aZoidMtx[,1:5,drop=F]
    colVal <- sort(unique(cStepMtx[,fixCol]))
    stdMILst <- list()
    cStepH <- gEnv$zhF[,2:6]-gEnv$zhF[,1:5]
    for( vIdx in colVal ){
        zMtx <- gEnv$zhF[ cStepH[,fixCol]==vIdx ,,drop=F]
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

        wLst$cStepTail <- stdMI$cStepTail[,-vpObj$mInfo["fixCol"],drop=F]
        wLst$fStepTail <- stdMI$fStepTail[,-vpObj$mInfo["fixCol"],drop=F]
        return( wLst )
    }

    vpObj$getCodeW <- function( aZoidMtx ){
        aObj <- list( miNames=names(vpObj$stdMILst) )

        miIdStr <- rep("N/A",nrow(aZoidMtx))

        cStepMtx <- aZoidMtx[,2:6,drop=F]-aZoidMtx[,1:5,drop=F]
        fStepMtx <- matrix( 0 ,ncol=ncol(aZoidMtx) ,nrow=nrow(aZoidMtx) )
        for( nIdx in names(vpObj$stdMILst) ){
            stdMI <- vpObj$stdMILst[[nIdx]]
            if(0==nrow(stdMI$rawTail))  next

            curWorkArea <- which(cStepMtx[,stdMI$mInfo["fixCol"]]==stdMI$mInfo["vIdx"])
            miIdStr[curWorkArea] <- nIdx

            mtx <- apply(aZoidMtx[curWorkArea,,drop=F],1,function(p){ p-stdMI$lastZoid})
            fStepMtx[curWorkArea,] <-t(mtx)
        }

        aObj$miIdStr <- miIdStr
        aObj$aZoidMtx <- aZoidMtx[,-vpObj$mInfo["fixCol"],drop=F]
        aObj$cStepMtx <- cStepMtx[,-vpObj$mInfo["fixCol"],drop=F]
        aObj$fStepMtx <- fStepMtx[,-vpObj$mInfo["fixCol"],drop=F]

        rownames(aObj$aZoidMtx) <- miIdStr
        rownames(aObj$cStepMtx) <- miIdStr
        rownames(aObj$fStepMtx) <- miIdStr
        return( aObj )
    }

    return(vpObj)
}

bS.vp_fStepBin <- function( gEnv, aZoidMtx ){   cat("폐기. stdMI가 만들기도전에, stdMI$lastZoid으로부터 fStep이 나와야 하는 모순 발생") }
