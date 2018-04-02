# 20180109_C.R 교차모델
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

tStmp <- Sys.time()
saveId <- "Z800"
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



    allIdx.bak <- allIdx

} # cutEadge()




cutEadge.XXXX <- function( gEnv ,allIdx ){

    rObj <- list( idStr="cutEadge.XXXX" )
    rObj$flag <- QQE # 
    return( rObj )

} # cutEadge.XXXX()





# ===============================================================================


k <- gEnv$zhF[,6] - gEnv$zhF[,1]
seqCntMtx <- k.seq(k)$seqCntMtx

seqCntMtx[seqCntMtx[,"cnt"]>1 ,]

