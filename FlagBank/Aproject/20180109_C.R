# 20180109_C.R 교차모델
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

tStmp <- Sys.time()
saveId <- "Z802"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

stdFiltedCnt <- sapply( fRstLst ,length )
stdFilted.tbl <- table(stdFiltedCnt)
stdFilted.per <- sprintf( "%.1f" ,100*stdFilted.tbl / length(fRstLst) )
names(stdFilted.per) <- names(stdFilted.tbl)

allZoid.lst <- vector("list",nrow(gEnv$allZoidMtx))
for( nIdx in attributes(remLst)$name ){
    for( aIdx in remLst[[nIdx]] ){
        allZoid.lst[[aIdx]][1+length(allZoid.lst[[aIdx]])] <- nIdx
    }
}


allZoid.fltCnt <- getAllZoidIdx.FltCnt( gEnv ,remLst )
# allZoidMtx <- gEnv$allZoidMtx[(allZoid.fltCnt==0),]

lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
# 가정 : allZoid.fltCnt 는 1~3이다.
allZoid.idx0 <- which(allZoid.fltCnt==0)
allZoid.idx1 <- which(allZoid.fltCnt==1)
allZoid.idx2 <- which(allZoid.fltCnt==2)



# ================================================================================================
# 가정 : allZoid.fltCnt의 1영역에서, 바로이전 필터링 결과는 포함되지 않는다.
#   단, 과거에 일어난 필터링만 사용한다.
#   단 1영역에서 "A0110.A" 반복은 제외(반복 발생량의 60%)
stdFiltedCnt.idx1 <- which(stdFiltedCnt==1)
lastFlt <- fRstLst[[ stdFiltedCnt.idx1[[length(stdFiltedCnt.idx1)]] ]]
fName <- do.call( c ,allZoid.lst[allZoid.idx1] )
allZoid.idx1 <- allZoid.idx1[fName!=lastFlt]

neverFnd <- setdiff( attributes(remLst)$name ,unique(do.call(c,fRstLst[stdFiltedCnt==1])) )
fName <- do.call( c ,allZoid.lst[allZoid.idx1] )
flag <- sapply( allZoid.lst[allZoid.idx1] ,function(p){ p%in%neverFnd })
allZoid.idx1 <- allZoid.idx1[!flag]


# 가정 : allZoid.fltCnt의 2영역에서, 바로이전 필터링 결과는 포함되지 않는다.
#   단, 과거에 일어난 필터링 조합만 사용한다.
stdFiltedCnt.idx2 <- which(stdFiltedCnt==2)
lastFlt <- fRstLst[[ stdFiltedCnt.idx2[[length(stdFiltedCnt.idx2)]] ]]
lastFlt.name <- paste(sort(lastFlt),collapse="_")
flag <- sapply( allZoid.lst[allZoid.idx2] ,function(p){
                    lastFlt.name != paste(sort(p),collapse="_")
                })
allZoid.idx2 <- allZoid.idx2[flag]

hnt.name <- unique(sapply(fRstLst[stdFiltedCnt.idx2] ,function(p){ paste(sort(p),collapse="_") } ))
flag <- sapply( allZoid.lst[allZoid.idx2] ,function(p){
                    return( paste(sort(p),collapse="_") %in% hnt.name )
                })
allZoid.idx2 <- allZoid.idx2[flag]

allIdxLst <- list( allZoid.idx0=allZoid.idx0 ,allZoid.idx1=allZoid.idx1 ,allZoid.idx2=allZoid.idx2 )
allIdxLst$saveId <- saveId
save( allIdxLst, file="Obj_allIdxLst.save" )

#   allIdx <- allZoid.idx0  ;allIdx.bak <- allIdx
cutEadge <- function( gEnv ,allIdx ){

    allZoidMtx <- gEnv$allZoidMtx[allIdx,]
    colValLst <- apply( gEnv$zhF ,2 ,function(p){
                        val <- sort(unique(p))
                        tbl <- table(p)
                        mtx <- matrix( 0 ,ncol=length(val) ,nrow=2 )
                        mtx[1,] <- val
                        mtx[2,] <- tbl[as.character(val)]
                        rownames(mtx) <- c("val","freq")
                        return(mtx)
                    })

    rstObj <- cutEadge.colValCut( gEnv ,allIdx ,colValLst )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.dup3Col( gEnv ,allIdx ,colValLst ,pThld=5 )  # pThld^6 에 비해 효과는 좋음.
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getCFltObj( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.remLstHard( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getColSeq( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getBanPtn( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getBanPtnColVal( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getBanSym( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getBanGrad( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.banDupSeq( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]


    rstObj <- cutEadge.getBanRebBin( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.banDupSeqBin( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.getBanSymBin( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getBanRebDiff( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.banDupSeqDiff( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.getBanSymDiff( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    # code step 너무 빈번한 듯 함.
    rstObj <- cutEadge.banSeqRebCStep( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.getBanSymCStep( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.getBanStepRebCStep( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.getBanGradCStep( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    # zoid[,c(1,6)] 은 피해야 할 듯. 빈번할 수 밖에 없음.
    rstObj <- cutEadge.getBanSeqRebWidth( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.getBanSymWidth( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.getBanStepRebWidth( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.getBanGradWidth( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]


    rstObj <- cutEadge.barReb3( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    rstObj <- cutEadge.banSeq3Twice( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    # -----------------------------------------

    # rebind cnt

    # QQE : %%10에 대한 3연속 이상 금지 # 취소. 4연속도 엄청 많음..

    allIdx.bak <- allIdx

} # cutEadge()


cutEadge.getBanGradColValReb <- function( gEnv ,allIdx ){

    scanPtn <- getPtnScanner()$grad

    allIdx.len <- length(allIdx)
    rebMtx <- getRebCnt.colVal( gEnv )$rebMtx

    # QQE

    flagLst.base <- vector("list",allIdx.len)
    banPtn <- scanPtn( stdWidth )
    for( idx in seq_len(allIdx.len) ){
        if( allWidth[idx]%in%banPtn ){
            flagLst.base[[idx]][[1+length(flagLst.base[[idx]])]] <- allWidth[idx]
        }
    }

    flagLst.cv <- vector("list",allIdx.len)
    widthSpan <- sort(unique(allWidth))
    for( wIdx in widthSpan ){
        tMtx <- gEnv$zhF[stdWidth==wIdx ,,drop=F]
        for( azColIdx in 2:5 ){ # (1:6)[-c(1,6)]
            banPtn <- scanPtn( tMtx[,azColIdx] )
            for( idx in seq_len(allIdx.len) ){
                if( wIdx!=allWidth[idx] ){
                    next
                }
                if( gEnv$allZoidMtx[allIdx[idx],azColIdx]%in%banPtn ){
                    flagLst.cv[[idx]][[1+length(flagLst.cv[[idx]])]] <- c( azColIdx ,wIdx )
                }
            } # idx
        } # azColIdx
    }

    rObj <- list( idStr="cutEadge.getBanGradColValReb" )
    rObj$flag <- sapply( seq_len(allIdx.len) ,function(idx){
                        (length(flagLst.base[[idx]])==0) && (length(flagLst.cv[[idx]])==0)
                    })

    return( rObj )

} # cutEadge.getBanGradColValReb( )




cutEadge.XXXX <- function( gEnv ,allIdx ){

    rObj <- list( idStr="cutEadge.XXXX" )
    rObj$flag <- QQE # 
    return( rObj )

} # cutEadge.XXXX()


# allIdx <- allZoid.idx1
finalCut <- function( gEnv ,allIdx ){

    allIdxF <- allIdx
    azColValLst <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,2 ,function(p){sort(unique(p))})
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # code step이 1인거 2개 이상은 없는 것으로.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
            cnt <- sum( 1==(aZoid[2:6]-aZoid[1:5]) )
            return( cnt<2 )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zoid[,5:6]%/%10 은 c(2,4)아님.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
            rem10 <- aZoid[5:6]%/%10
            return( !all(rem10==c(2,4)) )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 최근 40번대 너무 많았다.
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
            cnt <- sum( 4 == (aZoid%/%10) )
            return( cnt<2 )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 10번대도 너무 많았다. 2개 이상보유 금지
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
            cnt <- sum( 1 == (aZoid%/%10) )
            return( cnt<=2 )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # 1개 이상 재발 거부.
    lastZoid <- gEnv$zhF[nrow(gEnv$zhF),]
    flag <- apply( gEnv$allZoidMtx[allIdxF,,drop=F] ,1 ,function( aZoid ){
                cnt <- sum( aZoid %in% lastZoid )
                return( cnt<2 )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    firstCode <- c(4)
    flag <- gEnv$allZoidMtx[allIdxF,1] %in% c(firstCode) # zoid[1]은 1:5 사이에서 가장 오래 안 나온 값.
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))
    if( 0==length(allIdxF) ){
        cat("None of them....\n")
    }

    localHisMtx <- gEnv$zhF[gEnv$zhF[,1]%in%firstCode ,]
    #         740  4  8  9 16 17 19
    #         752  4 16 20 33 40 43
    #         761  4  7 11 24 42 45
    #         785  4  6 15 25 26 33

    # aZoid4[1:2]에 대해 (4,7) (4,6) ... and (4,5)? no
    flag <- gEnv$allZoidMtx[allIdxF,2]!=5
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # aZoid4 %%10 에서 5가 동일 연속 거부.
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                    idx <- which( 5==(aZoid%%10) )
                    if( 2<length(idx) ) return( FALSE )
                    if( 2==length(idx) && 1==(idx[2]-idx[1]) ) return( FALSE )
                    return( TRUE )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # aZoid4 %/% 10 값이 같은 거 거부.
    lastQuo <- localHisMtx[nrow(localHisMtx),] %/% 10
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                return( !all(aZoid%/%10==lastQuo) )
        })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # zhF[730,]  4 10 14 15 18 22.. (10 많음)
    #   (10,14) (14,15) (15,18) 패턴은 피하자. lastZoid에서도 10 많음.
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){

                    idx <- which( aZoid %in% c(10,14) )
                    if( 2==length(idx) && 1==(idx[2]-idx[1]) ) return( FALSE )

                    idx <- which( aZoid %in% c(14,15) )
                    if( 2==length(idx) && 1==(idx[2]-idx[1]) ) return( FALSE )

                    idx <- which( aZoid %in% c(15,18) )
                    if( 2==length(idx) && 1==(idx[2]-idx[1]) ) return( FALSE )

                    return( TRUE )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # aZoid4와 연속으로 3개 똑같은 발생은 피하자.(3개 초과는 아예 스킵.)
    lastLH <- localHisMtx[nrow(localHisMtx),]
    flag <- apply( gEnv$allZoidMtx[allIdxF,] ,1 ,function( aZoid ){
                    idx <- which( aZoid %in% lastLH )
                    if( 3<length(idx) ) return( FALSE )
                    if( 3==length(idx) && 2==(idx[3]-idx[1]) ) return( FALSE )

                    return( TRUE )
                })
    allIdxF <- allIdxF[flag]
    cat(sprintf("allIdxF %d\n",length(allIdxF)))

    # allIdx <- allIdxF

    for( idx in ){

    }
    fndLst <- fnd2SeqReb( gEnv$zhF ,gEnv$allZoidMtx[allIdx[idx],] ,pSrcNum=2  )

    fndLst <- fnd2SeqReb( gEnv$zhF ,gEnv$allZoidMtx[2994332,] ,pSrcNum=2  )

    

    rebCnt <- sapply( 2:nrow(localHisMtx) ,function(idx){
                        sum( localHisMtx[(idx-1),] %in% localHisMtx[idx,] )
                    })
    rebMtxLst <- lapply( which(rebCnt>1) ,function( idx ){
                        return( localHisMtx[idx+0:1,] )
                    })

    return( allIdxF )

} # finalCut()


# ===============================================================================


cutEadgeLst <- function( ){

    rLst <- list()

    rLst[[1+length(rLst)]] <- cutEadge.getCFltObj
    rLst[[1+length(rLst)]] <- cutEadge.getCFltObj
    rLst[[1+length(rLst)]] <- cutEadge.remLstHard
    rLst[[1+length(rLst)]] <- cutEadge.getColSeq
    rLst[[1+length(rLst)]] <- cutEadge.getBanPtn
    rLst[[1+length(rLst)]] <- cutEadge.getBanPtnColVal
    rLst[[1+length(rLst)]] <- cutEadge.getBanSym
    rLst[[1+length(rLst)]] <- cutEadge.getBanGrad
    rLst[[1+length(rLst)]] <- cutEadge.banDupSeq
    rLst[[1+length(rLst)]] <- cutEadge.getBanRebBin
    rLst[[1+length(rLst)]] <- cutEadge.banDupSeqBin
    rLst[[1+length(rLst)]] <- cutEadge.getBanSymBin
    rLst[[1+length(rLst)]] <- cutEadge.getBanRebDiff
    rLst[[1+length(rLst)]] <- cutEadge.banDupSeqDiff
    rLst[[1+length(rLst)]] <- cutEadge.getBanSymDiff

    rLst[[1+length(rLst)]] <- cutEadge.banSeqRebCStep
    rLst[[1+length(rLst)]] <- cutEadge.getBanSymCStep
    rLst[[1+length(rLst)]] <- cutEadge.getBanStepRebCStep
    rLst[[1+length(rLst)]] <- cutEadge.getBanGradCStep

    rLst[[1+length(rLst)]] <- cutEadge.getBanSeqRebWidth
    rLst[[1+length(rLst)]] <- cutEadge.getBanSymWidth
    rLst[[1+length(rLst)]] <- cutEadge.getBanStepRebWidth
    rLst[[1+length(rLst)]] <- cutEadge.getBanGradWidth

    rLst[[1+length(rLst)]] <- cutEadge.barReb3

    rLst[[1+length(rLst)]] <- cutEadge.banSeq3Twice

    return( rLst )

}


cutEadge.test <- function( gEnv ,allIdx ){

    rstObj <- list()

    colValLst <- apply( gEnv$zhF ,2 ,function(p){
                        val <- sort(unique(p))
                        tbl <- table(p)
                        mtx <- matrix( 0 ,ncol=length(val) ,nrow=2 )
                        mtx[1,] <- val
                        mtx[2,] <- tbl[as.character(val)]
                        rownames(mtx) <- c("val","freq")
                        return(mtx)
                    })

    rstObj <- cutEadge.colValCut( gEnv ,allIdx ,colValLst )

    rstObj <- cutEadge.dup3Col( gEnv ,allIdx ,colValLst ,pThld=5 )  # pThld^6 에 비해 효과는 좋음.
    

    return( rstObj )

}




