# 20180109_C.R 교차모델
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

saveId <- "Z798"
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

# 가정 : zoid[1]==1
flag <- sapply( allZoid.idx0 ,function( p ){ gEnv$allZoidMtx[p,1] == 1 })
allZoid.idx0 <- allZoid.idx0[flag]
flag <- sapply( allZoid.idx1 ,function( p ){ gEnv$allZoidMtx[p,1] == 1 })
allZoid.idx1 <- allZoid.idx1[flag]
flag <- sapply( allZoid.idx2 ,function( p ){ gEnv$allZoidMtx[p,1] == 1 })
allZoid.idx2 <- allZoid.idx2[flag]

# 가정 : 22,32는 포함되지 않는다.
flag <- sapply( allZoid.idx0 ,function( p ){ !any(c(22,23) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx0 <- allZoid.idx0[flag]
flag <- sapply( allZoid.idx1 ,function( p ){ !any(c(22,23) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx1 <- allZoid.idx1[flag]
flag <- sapply( allZoid.idx2 ,function( p ){ !any(c(22,23) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx2 <- allZoid.idx2[flag]

# 가정 : zoid[4]%%10은 2가 아님.
flag <- sapply( allZoid.idx0 ,function(p){ 2!=gEnv$allZoidMtx[p,4]%%10 } )
allZoid.idx0 <- allZoid.idx0[flag]
flag <- sapply( allZoid.idx1 ,function(p){ 2!=gEnv$allZoidMtx[p,4]%%10 } )
allZoid.idx1 <- allZoid.idx1[flag]
flag <- sapply( allZoid.idx2 ,function(p){ 2!=gEnv$allZoidMtx[p,4]%%10 } )
allZoid.idx2 <- allZoid.idx2[flag]

# 가정 : 연속 값은 1개이다.
flag <- sapply( allZoid.idx0 ,function( p ){ 1==sum(c(2,10,14,36) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx0 <- allZoid.idx0[flag]
flag <- sapply( allZoid.idx1 ,function( p ){ 1==sum(c(2,10,14,36) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx1 <- allZoid.idx1[flag]
flag <- sapply( allZoid.idx2 ,function( p ){ 1==sum(c(2,10,14,36) %in%gEnv$allZoidMtx[p,]) })
allZoid.idx2 <- allZoid.idx2[flag]

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


#   allIdx <- allZoid.idx1  ;allIdx.bak <- allIdx
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

    rstObj <- cutEadge.getCFltObj( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.remLstHard( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getColSeq( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]

    rstObj <- cutEadge.getBanPtn( gEnv ,allIdx )
    allIdx <- allIdx[rstObj$flag]
    # cutEadge.getBanPtnColVal() 컬럼 값 별,
    # cutEadge.getBanPtnRem10() %% 10 에 대한 ptn

    allIdx.bak <- allIdx

} # cutEadge()



cutEadge.getBanPtn <- function( gEnv ,allIdx ,pThldChk=1 ){

    banObj <- getBanPtn( gEnv$zhF )
    chkCnt <- sapply(banObj$ptnLst ,function(p){p$chkCnt})
    thldPtnIdx <- which(chkCnt<pThldChk)    # chkCnt에 대한 갯수 기준.

    banRst <- banObj$chkMatchAny( gEnv$allZoidMtx[allIdx,,drop=F] ,pDebug=T )
    rstLst <- banRst$rstLst
    #   chkMatchAny() 가 사용된다면 pThldChk는 의미없다.
    # rstLst <- lapply( banRst$rstLst ,function(p){ setdiff(p,thldPtnIdx) })

    rObj <- list( idStr="cutEadge.getBanPtn" )
    rObj$flag <- ( 0==sapply(rstLst,length) )
    return( rObj )

} # cutEadge.getBanPtn()


cutEadge.getBanSym <- function( gEnv ,allIdx ){

    rObj <- list( idStr="cutEadge.getBanSym" )
    rObj$flag <- apply( surviveMtx ,1 ,all )
    return( rObj )

} # cutEadge.getBanSym()



cutEadge.XXXX <- function( gEnv ,allIdx ){

    rObj <- list( idStr="cutEadge.XXXX" )
    rObj$flag <- apply( surviveMtx ,1 ,all )
    return( rObj )

} # cutEadge.XXXX()





    # 짝 패턴 : [787:790,1] ,홀 패턴 : [789:793,3]
	# 	787  5  6 13 16 27 28
	# 	788  2 10 11 19 35 39
	# 	789  2  6  7 12 19 45
	# 	790  3  8 19 27 30 41
	# 	791  2 10 12 31 33 42
	# 	792  2  7 19 25 29 36
	# 	793 10 15 21 35 38 43

#   컬럼 값에서 연속으로 대칭발생 ban
getBanSeqSym <- function( pValMtx ,pMaxDepth=5 ,pDepbug=F ){

    # QQE working
    # col.fndSymEven(), col.fndSymOdd() 각각에 대해서 테스트


	rObj <- list( stdVal=stdVal ,flag=matFlag ,depth=pDepth )
	return( rObj )

} # getBanSeqSym()

#   3,2,1,1,2,? 패턴
col.fndSymEven <- function( pVal ,pMaxDepth=5 ){
    
    val.len <- length(pVal)
    if( 3 > val.len ){   # 2,1,1,?
        return(NULL)
    }

    srcPtn <- integer(0)
    eadgeVal <- NA
    for( dIdx in 1:pMaxDepth ){
        srcPtn <- pVal[val.len:(val.len-dIdx+1)]
        matSpan <- (val.len-dIdx-dIdx+1):(val.len-dIdx)
        if( 2 > matSpan[1] ){ # 최소한 pVal[1]은 존재해야 ? 매칭값 할당가능
            break
        }

        if( all(srcPtn==pVal[matSpan]) ){
            eadgeVal <- pVal[ matSpan[1]-1 ]
            break
        }
    } # for(dIdx)

    if( is.na(eadgeVal) ){  return( NULL )
    } else {
        return( list(banVal=eadgeVal ,ptn=ptn) )
    }

} # col.fndSymEven()


# pVal <- c( 30,20,10,20 )
# pVal <- c( 40,30,20,10,20,30 )
#   3,2,1,2,? 패턴
col.fndSymOdd <- function( pVal ,pMaxDepth=5 ){

    val.len <- length(pVal)
    if( 4>length(pVal) ){   # 3,2,1,2,?
        return(NULL)
    }

    srcPtn <- integer(0)
    eadgeVal <- NA
    for( dIdx in 1:pMaxDepth ){
        srcPtn <- pVal[val.len:(val.len-dIdx+1)]
        matSpan <- (val.len-dIdx-1-dIdx+1):(val.len-dIdx-1)
        if( 2 > matSpan[1] ){ # 최소한 pVal[1]은 존재해야 ? 매칭값 할당가능
            break
        }

        if( all(srcPtn==pVal[matSpan]) ){
            eadgeVal <- pVal[ matSpan[1]-1 ]
            break
        }
    } # for(dIdx)

    if( is.na(eadgeVal) ){  return( NULL )
    } else {
        return( list(banVal=eadgeVal ,ptn=ptn) )
    }

} # col.fndSymOdd()


# ===============================================================================



