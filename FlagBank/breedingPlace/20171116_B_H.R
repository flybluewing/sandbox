# 20171116_B_H.R

flt.sStepMax <- function( pZh ){
    # side step - max

    rObj <- list( idStr="sStepMax" )

    getStepMtx <- function( pZh ){

        codeMtx <- pZh[,2:6] - pZh[,1:5]
        maxIdx <- apply( codeMtx ,1 ,which.max )

        allStepMtx <- matrix( 0 ,nrow=nrow(codeMtx) ,ncol=3 )
        for( rIdx in 1:nrow(codeMtx) ){
            allStepMtx[rIdx,1] <- codeMtx[rIdx ,maxIdx[rIdx] ]
            allStepMtx[rIdx,2] <- pZh[ rIdx ,maxIdx[rIdx] ]
            allStepMtx[rIdx,3] <- maxIdx[rIdx]
        }
        colnames(allStepMtx) <- c("step","val","col")

        step <- sort(unique(allStepMtx[,"step"]))
        cName <- c("maxStep","val")
        stepMtx <- matrix(0,nrow=length(step),ncol=length(cName))
        colnames( stepMtx ) <- cName
        stepMtx[,"maxStep"] <- step
        for( rIdx in 1:nrow(allStepMtx) ){
            idx <- which( allStepMtx[rIdx,1]==stepMtx[,"maxStep"] )
            stepMtx[idx,"val"] <- allStepMtx[rIdx,"val"]
        }

        return( stepMtx )
    }

    rObj$stepMtx <- getStepMtx( pZh )

    rObj$byLate <- function( pZoidMtx ,pDebugInfo=F ){

        stepMtx <- pZoidMtx[,2:6] - pZoidMtx[,1:5]

        filted <- rep( 0 ,nrow(stepMtx) )
        for( rIdx in 1:nrow(stepMtx) ){
            maxCol <- which.max( stepMtx[rIdx,] )

            hrIdx <- which( rObj$stepMtx[,1] == stepMtx[rIdx,maxCol] )            
            if( 0==length(hrIdx) ){
                filted[rIdx] <- NA
                next
            }

            if( rObj$stepMtx[hrIdx,"val"]==pZoidMtx[rIdx,maxCol] ){
                filted[rIdx] <- stepMtx[rIdx,maxCol]
            } else {
                filted[rIdx] <- NA
            }
        }

        if( pDebugInfo ){   return( filted)
        } else {
            return( is.na(filted) )
        }
    }

    return( rObj )
} # flt.sStepMax()



flt.rebLen2 <- function( pZh ,pThld=15 ,pUpper=T ){
    # flt.rebLen() 는 단순히 rebLen값을 비교함과 달리,
    # flt.rebLen2()는 pThld보다 큰 rebLen의 수와 col위치를 따진다.

    rObj <- list( idStr="rebLen2" )
    rObj$thld <- pThld
    rObj$upper <- pUpper

    getRebLst <- function( pZh ,pThld ,pUpper ){
        rebMtx <- matrix( 0 ,nrow=nrow(pZh) ,ncol=ncol(pZh) )
        rebMtx[1,] <- NA
        rebMax <- rep( F ,nrow(rebMtx) )
        for( hIdx in 2:nrow(pZh) ){
            ml <- getReboundLst( pZh[hIdx,] ,pZh[1:(hIdx-1),,drop=F] ,pSearchFirst=T )
            rebMtx[hIdx,] <- 
                sapply( ml ,function(p){ifelse(0==length(p$fIdx),NA,hIdx-p$fIdx[1])} )
            rebMax[hIdx] <- max(rebMtx[(hIdx-1),]) %in% rebMtx[hIdx,]
        } # for(hIdx)
        naIndices <- which(apply(rebMtx,1,function(p){any(is.na(p))}))
        rebMtx <- rebMtx[-naIndices,]
        allLst <- if( pUpper ){
                        apply( rebMtx ,1 ,function(p){which(p>=pThld)} )
                    } else {
                        apply( rebMtx ,1 ,function(p){which(p<=pThld)} )
                    }

        rebLst <- list()
        for( idx in 1:6 )
            rebLst[[idx]] <- list( colIdx=NULL )

        for( idx in 1:length(allLst) ){
            cnt <- length(allLst[[idx]])
            if( 0==cnt )
                next
            
            rebLst[[cnt]]$colIdx <- allLst[[idx]]
        }

        return( rebLst )
    }

    rObj$rebLst <- getRebLst( pZh ,pThld ,pUpper )
        # 0의 연속발생은 꽤 흔하다.. 따라서 방생.
    rbLst <- getReboundLst( pZh[nrow(pZh),] ,pZh[1:(nrow(pZh)-1),,drop=F] ,pSearchFirst=T )
    reb <- sapply( rbLst ,function(p){nrow(pZh)-p$fIdx})
    rObj$lastColIdx <- if( pUpper ) which(reb>=pThld) else which(reb<=pThld)
    rObj$lastColLen <- length(rObj$lastColIdx)

    rObj$rebLen <- getRebLen( 1:45 ,pZh )

    rObj$byLate <- function( pZoidMtx ,pDebugInfo=F ){

        zRebMtx <- t(apply( pZoidMtx ,1 ,function(p){ rObj$rebLen[p] }))
        filted <- rep( 0 ,nrow(zRebMtx) )
        for( idx in 1:nrow(zRebMtx) ){
            colIdx <- if( rObj$upper ) { which(zRebMtx[idx,]>=pThld)
                        } else which(zRebMtx[idx,]<=pThld)
            
            iLen <- length(colIdx) 
            if( 0==rObj$lastColLen || 0==iLen ){   # 0 연속발생은 꽤 많아서.. 방생하자.
                filted[idx] <- NA
            } else if( is.null(rObj$rebLst[[iLen]]$colIdx) ) {
                filted[idx] <- iLen # 이전에 발생한 적 없다면 앞으로도 발생 불가겠지.
            } else {
                if( all(colIdx==rObj$rebLst[[iLen]]$colIdx) ) {
                    filted[idx] <- iLen
                } else {
                    filted[idx] <- NA
                }
            }

        } # for(idx)

        if( pDebugInfo ){   return( filted)
        } else {
            return( is.na(filted) )
        }
    }

    return( rObj )
} # flt.rebLen2()

flt.rebLen <- function( pZh ){

    rObj <- list( idStr="rebLen" )

    getRebMtx <- function( pZh ){
        rebMtx <- matrix( 0 ,nrow=nrow(pZh) ,ncol=ncol(pZh) )
        rebMtx[1,] <- NA
        rebMax <- rep( F ,nrow(rebMtx) )
        for( hIdx in 2:nrow(pZh) ){
            ml <- getReboundLst( pZh[hIdx,] ,pZh[1:(hIdx-1),,drop=F] ,pSearchFirst=T )
            rebMtx[hIdx,] <- 
                sapply( ml ,function(p){ifelse(0==length(p$fIdx),NA,hIdx-p$fIdx[1])} )
            rebMax[hIdx] <- max(rebMtx[(hIdx-1),]) %in% rebMtx[hIdx,]
        } # for(hIdx)
        naIndices <- which(apply(rebMtx,1,function(p){any(is.na(p))}))
        rebMtx <- rebMtx[-naIndices,]
        valMtx <- pZh[-naIndices,]

        maxPos <- apply( rebMtx ,1 ,which.max )
        maxReb <- rep(0,length(maxPos)) ;valReb <- rep(0,length(maxPos))
        for( idx in 1:length(maxPos) ){
            maxReb[idx] <- rebMtx[idx,maxPos[idx]]
            valReb[idx] <- valMtx[idx,maxPos[idx]]
        }

        cName <- c( "reb" ,"val" )
        rebMtx <- matrix( 0 ,ncol=length(cName) ,nrow=length(unique(maxReb)) )
        colnames(rebMtx) <- cName
        rebMtx[,"reb"] <- sort(unique(maxReb))
        for( idx in 1:length(maxReb) ){
            rIdx <- which( rebMtx[,"reb"] == maxReb[idx] )
            rebMtx[rIdx,"val"] <- valReb[idx]
        }

        return( rebMtx )
    }

    rObj$rebMtx <- getRebMtx( pZh )
    rbLst <- getReboundLst( pZh[nrow(pZh),] ,pZh[1:(nrow(pZh)-1),,drop=F] ,pSearchFirst=T )
    rObj$lastMax <- max(sapply( rbLst ,function(p){nrow(pZh)-p$fIdx}))

    rObj$rebLen <- getRebLen( 1:45 ,pZh )

    rObj$byLate <- function( pZoidMtx ,pDebugInfo=F ){

        zRebMtx <- t(apply( pZoidMtx ,1 ,function(p){ rObj$rebLen[p] }))
        filted <- rep( 0 ,nrow(zRebMtx) )
        for( idx in 1:nrow(zRebMtx) ){
            maxCol <- which.max(zRebMtx[idx,])
            curRebLen <- zRebMtx[idx,maxCol]
            curRebVal <- pZoidMtx[idx,maxCol]

            if( rObj$lastMax==curRebLen ){
                filted[idx] <- 0    # 값이 똑같으면 제로거리 사격 킬.
                next
            }

            lastRebIdx <- which(rObj$rebMtx[ ,"reb" ] == curRebLen)
            if( 0==length(lastRebIdx) ){    # 발생한 적 없다면 방생.
                filted[idx] <- NA
                next
            }

            if( rObj$rebMtx[lastRebIdx,"val"]==curRebVal ) {
                filted[idx] <- curRebLen
            } else {
                filted[idx] <- NA
            }
        } # for(idx)

        if( pDebugInfo ){   return( filted)
        } else {
            return( is.na(filted) )
        }
    }

    rObj$report <- function( pZh ){
        rebMtx <- matrix( 0 ,nrow=nrow(pZh) ,ncol=ncol(pZh) )
        rebMtx[1,] <- NA
        rebMax <- rep( F ,nrow(rebMtx) )
        for( hIdx in 2:nrow(pZh) ){
            ml <- getReboundLst( pZh[hIdx,] ,pZh[1:(hIdx-1),,drop=F] ,pSearchFirst=T )
            rebMtx[hIdx,] <- 
                sapply( ml ,function(p){ifelse(0==length(p$fIdx),NA,hIdx-p$fIdx[1])} )
            rebMax[hIdx] <- max(rebMtx[(hIdx-1),]) %in% rebMtx[hIdx,]
        } # for(hIdx)
        naIndices <- which(apply(rebMtx,1,function(p){any(is.na(p))}))
        rebMtx <- rebMtx[(max(naIndices)+1):nrow(rebMtx),]
        maxReb <- apply( rebMtx ,1 ,max )
            # table(cnt)
            #     1   2   3 
            #     716  18   1 
    }

    return( rObj )
} # flt.rebLen()



flt.width <- function( pZh ,pBuf=3 ){

    rObj <- list( idStr="width" )
    rObj$buf <- pBuf
    
    code <- pZh[,5] - pZh[,2]
    rObj$lastCode <- code[ (length(code)-rObj$buf+1):length(code) ]
    rObj$lastCode <- unique(rObj$lastCode)

    rObj$byLate <- function( pZoidMtx ){
        zWidth <- pZoidMtx[,5] - pZoidMtx[,2]
        rFlag <- !(zWidth %in% rObj$lastCode)
        return( rFlag )
    }

    return( rObj )
} # flt.width()



flt.reb <- function( pZh ,pBackDepth=2 ){
    rObj <- list( idStr=sprintf("reb%d",pBackDepth) )
    rObj$bDepth <- pBackDepth

    matValLst <- list()
    for( hIdx in (rObj$bDepth+1):nrow(pZh) ){
        code <- unique(as.vector( pZh[(hIdx-1):(hIdx-rObj$bDepth),] ))
        matValLst[[1+length(matValLst)]] <- pZh[hIdx, pZh[hIdx,]%in%code ]
    }

    # 마지막 재발 갯수만 가지고 비교하면, 비교대상이 하나뿐이브로
    #   재발 갯수와 동일한 가장 최근 재발 갯수에서의 값이 동일한지 비교하자.
    lstCnt <- sapply( matValLst ,length )
    stdValLst <- list() ;foundLoc <- rep( NA ,ncol(pZh) )
    for( idx in 1:ncol(pZh) ){
        i <- which( lstCnt==idx )
        matObj <- list()
        if( 0==length(i) ){
            matObj$val <- NULL
        } else {
            matObj$loc <- i[length(i)]
            matObj$val <- matValLst[[ matObj$loc ]]
        }
        stdValLst[[idx]] <- matObj
    }
    # - 매칭 숫자가 0인 경우는 값 비교의 필요가 없다.
    # - stdValLst에서 NULL이 배정된 경우, 매치 값 비교없이 숫자만 다르면 합격.

    rObj$stdValLst <- stdValLst
    rObj$lastCode <- unique(as.vector(pZh[ (nrow(pZh)-rObj$bDepth+1):nrow(pZh),]))
    rObj$lastReb <- lstCnt[length(lstCnt)]
        # 재발 갯수가 0인 경우에 대해서는, zh의 마지막 재발생도 0인 경우에만 FALSE 처리하자.

    rObj$byLate <- function( pZoidMtx ){
        rFlag <- rep( FALSE ,nrow(pZoidMtx) )
        matNum <- rep( 0 ,nrow(pZoidMtx) )
        for( rIdx in 1:nrow(pZoidMtx) ){
            mtch <- pZoidMtx[rIdx,] %in% rObj$lastCode
            mtch.len <- sum(mtch)
            if( 0==rObj$lastReb || 0==mtch.len ){
                rFlag[rIdx] <- rObj$lastReb != mtch.len
            } else if( is.null(rObj$stdValLst[[mtch.len]]$val) ){
                # 비교 대상이 되는 최근 발생 케이스가 없다면
                rFlag[rIdx] <- rObj$lastReb != mtch.len
            } else {
                rFlag[rIdx] <- !all(rObj$stdValLst[[mtch.len]]$val==pZoidMtx[rIdx,mtch])
            }
            matNum[rIdx] <- mtch.len
        }
        return( rFlag )
    } # rObj$byLate()

    return( rObj )
} # flt.reb


