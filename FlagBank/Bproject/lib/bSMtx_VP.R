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

    # leftFlag <- sapply( pairInfoLst ,function(p){
    #     if( length(p)==0 )   return( FALSE )   # pair 자체가 없으면 손봐줄 필요조차 없으니.

    #     minLen <- 3
    #     for( lIdx in 1:length(p) ){
    #         if( minLen>length(p[[lIdx]]$idx) ){
    #             minLen <- length(p[[lIdx]]$idx)
    #             break
    #         }
    #     }
    #     if( 3<=minLen ) return( FALSE )

    #     return( TRUE )
    # })

    leftFlag <- rep( T ,nrow(aZoidMtx) )
    workSpanLst <- list()
    for( grpIdx in names(idStrTbl) ){   # 실행효율보다, 로직이 보이는 쪽으로 코딩한다. ^^;
        fndFlag <- sapply( pairIdStrLst ,function(pairId){ grpIdx %in% pairId })
        workFlag <- leftFlag & fndFlag
        workSpanLst[[grpIdx]] <- which( workFlag )

        leftFlag[fndFlag] <- FALSE
    }

    # 발견된 pair group 별로 stdMILst 생성.
    # QQE working
    # stdMILst <- list

    vpObj$stdMILst <- stdMILst



    return(vpObj)
}

