# 20180109_C.R 교차모델
source("../breedingPlace/20171116_A_H.R")
source("../breedingPlace/20171116_A_H_cliper.R")
source("../breedingPlace/20171116_B_H.R")
source("../breedingPlace/20171116_C_H.R")
source("../breedingPlace/20171116_D_H.R")
source("20180109_A_H.R")
source("20180109_C_H.R")

saveId <- "0127_23"
load(sprintf("./save/Obj_gEnv%s.save",saveId))
load(sprintf("./save/Obj_fRstLst%s.save",saveId))
load(sprintf("./save/Obj_remLst%s.save",saveId))

stdFiltedCnt <- sapply( fRstLst ,length )
stdFilted.tbl <- table(stdFiltedCnt)
stdFilted.per <- sprintf( "%.1f" ,100*stdFilted.tbl / length(fRstLst) )
names(stdFilted.per) <- names(stdFilted.tbl)


banObj <- getCFltObj( gEnv )
allZoidMtx <- gEnv$zhF
codeLst <- banObj$getCodeLst( allZoidMtx )
# 개발 샘플 시점.

allZoid.fltCnt <- getAllZoidIdx.FltCnt( gEnv ,remLst )
allZoidMtx <- gEnv$allZoidMtx[(allZoid.fltCnt==0),]

tStmp <- Sys.time()
filtedIdxObj <- banObj$getFiltedIdx( allZoidMtx )
filtedIdx <- unique( filtedIdxObj$filtedIdx.dupRow ,filtedIdxObj$filtedIdx.cf1 )
    # filtedIdxObj$filtedIdx.dupRow 에게 모두 파묻히는 거 같은데.. 뭔가 수상타?
tDiff <- Sys.time() - tStmp
cat(sprintf("time cost %.1f%s \n",tDiff,units(tDiff)))

# ===============================================================================


#   pDimCnt : 동일한 dim이 n개 이상 존재하는 것은 자른다.
#   	pBanObj<-banObj ;pZoidMtx<-gEnv$zhF ;pInitZIdx=NULL ;pCodeLst=codeLst   ;pDebug=F
ban.multiDim <-function( pBanObj ,pZoidMtx ,pCodeLst ,pInitZIdx=NULL ,pDebug=F ){
}


