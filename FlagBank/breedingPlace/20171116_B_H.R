# 20171116_B_H.R





flt.width <- function( pZh ,pBuf=10 ){

    rObj <- list( idStr="width" )
    rObj$buf <- pBuf
    
    code <- zhF[,5] - zhF[,2]
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


