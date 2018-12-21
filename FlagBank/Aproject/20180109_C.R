# 20180109_C.R 교차모델
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")
source("20180109_C_HRad.R")
source("20180109_C_HUnit.R")
source("20180109_D_H.R")
source("./lib/fCutU_H.R")
source("./lib/u0_H.R")  ;source("./lib/u1_H.R")
source("./lib/ff0_H.R")

tStmp <- Sys.time()
saveId <- "Z837"
# myObj <- load( sprintf("Obj_allIdxLst%s.save",saveId) )
load(sprintf("./save/Obj_gEnv%s.save",saveId))

load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

# # A0110.A 의 효율이 안 좋은 듯 해서 테스트.
# fRstLst <- lapply( fRstLst ,function(fRst){ return(fRst[fRst!="A0110.A"]) })
# remLst[["A0110.A"]] <- integer(0)

stdFiltedCnt <- sapply( fRstLst ,length )   ;names(stdFiltedCnt) <- ( nrow(gEnv$zhF)-length(stdFiltedCnt)+1 ):nrow(gEnv$zhF)
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


# rebCnt
rebCnt <- sapply( 2:nrow(gEnv$zhF) ,function(hIdx){ sum(gEnv$zhF[hIdx,] %in% gEnv$zhF[(hIdx-1),]) } )
rebCnt <- c( 0 ,rebCnt )    ;names(rebCnt) <- 1:nrow(gEnv$zhF)

allIdxLst <- list( allZoid.idx0=allZoid.idx0 ,allZoid.idx1=allZoid.idx1 ,allZoid.idx2=allZoid.idx2 )
allIdxLst$saveId <- saveId
allIdxLst$stdFiltedCnt <- stdFiltedCnt
allIdxLst$stdFiltedCnt.n0 <- names(stdFiltedCnt)[stdFiltedCnt==0]
allIdxLst$stdFiltedCnt.n1 <- names(stdFiltedCnt)[stdFiltedCnt==1]
allIdxLst$infoMtx <- cbind( stdFiltedCnt ,rebCnt[names(stdFiltedCnt)] )
colnames( allIdxLst$infoMtx ) <- c("stdFiltedCnt","rebCnt")

save( allIdxLst, file=sprintf("Obj_allIdxLst%s.save",saveId) )


























#=[Final Course]=========================================================================
tStmp <- Sys.time()
#   source("./toFinal/to20180428.R")
source("./toFinal/to20180505.R")

allIdxObj.0428.1 <- finalCut( gEnv ,allIdx=allIdxLst$allZoid.idx1 )

curEadgeObj <- cutEadge( gEnv ,allIdxObj.0428.1$allIdxF.4 )
allIdxObj.0428.1$allIdxF.4.eadge <- curEadgeObj$allIdx

curEadgeObj <- cutEadge( gEnv ,allIdxObj.0428.1$allIdxF.7 )
allIdxObj.0428.1$allIdxF.7.eadge <- curEadgeObj$allIdx

save( allIdxObj.0428.1 ,file="./toFinal/Obj_allIdxObj.0428.1.save" )
tDiff <- Sys.time() - tStmp
sapply( allIdxObj.0428.1 ,length )













chkEadge <- function( gEnv ,allIdx ){

    allZoidMtx <- gEnv$allZoidMtx[allIdx,]
	chkLst <- list()

    rstObj <- chkEadge.xxx( gEnv ,allIdx )
    chkLst[[rstObj$idStr]] <- rstObj$flag

	# ----------------------------------------------------
	# chkLst 에 대한 결과 분석 후 return.
	
}


# ===============================================================================


cutEadgeLst <- function( ){

    rLst <- list()

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

    rLst[[1+length(rLst)]] <- cutEadge.banReb3
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




