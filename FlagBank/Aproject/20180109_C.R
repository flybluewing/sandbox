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

# 가정 : zoid[4]%%10은 2가 아님.

#   allIdx <- allZoid.idx1
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

    allIdx.bak <- allIdx

} # cutEadge()





cutEadge.getColSeq <- function( gEnv ,allIdx ){

    colValLst <- apply( gEnv$allZoidMtx[allIdx,,drop=F] ,2 ,function(p){ sort(unique(p)) })


    for( colIdx in 1:length(colValLst) ){
        for( valIdx in colValLst[[colIdx]] ){
            valMtx <- gEnv$zhF[gEnv$zhF[,colIdx]==valIdx ,]
            seqObj <- getColSeq( valMtx ,pDepth=2 )

            fndSeqIdx <- setdiff( which(seqObj$flag) ,colIdx )
            if( 0==length(fndSeqIdx) ){
                next
            }

            # QQE working
        }
    } # colIdx

    # zhF 에 대해서도 getColSeq() 적용.

    seqLst[[1+length(seqLst)]] <- getColSeq( valMtx ,pDepth=depth )

    rObj <- list( idStr="cutEadge.getColSeq" )
    rObj$flag <- apply( surviveMtx ,1 ,all )
    return( rObj )

} # cutEadge.getColSeq()


# > tail(valMtx)
#         E1 E2 E3 E4 E5 E6
#     745  1  2  3  9 12 23
#     750  1  2 15 19 24 36
#     762  1  3 12 21 26 41
#     765  1  3  8 12 42 43
#     770  1  9 12 23 39 43
#     796  1 21 26 36 40 41



cutEadge.XXXX <- function( gEnv ,allIdx ){

    rObj <- list( idStr="cutEadge.XXXX" )
    rObj$flag <- apply( surviveMtx ,1 ,all )
    return( rObj )

} # cutEadge.XXXX()












fRstLst.cnt <- sapply( fRstLst ,length )
kMtx <- do.call(rbind, fRstLst[fRstLst.cnt==1] )
dupCnt <- sum( kMtx[2:nrow(kMtx),]==kMtx[1:(nrow(kMtx)-1),] )
cat(sprintf("dup : %d of %d\n",dupCnt,nrow(kMtx)))
fRstLst.cnt <- sapply( fRstLst ,length )
kMtx <- do.call(rbind, fRstLst[fRstLst.cnt==2] )
dupCnt <- sum( all(kMtx[2:nrow(kMtx),]==kMtx[1:(nrow(kMtx)-1),]) )
cat(sprintf("dup : %d of %d\n",dupCnt,nrow(kMtx)))


tStmp <- Sys.time()
tDiff <- Sys.time() - tStmp
cat(sprintf("time cost %.1f%s \n",tDiff,units(tDiff)))

# ===============================================================================





