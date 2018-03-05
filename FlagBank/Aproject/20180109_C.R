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
#   	pBanObj<-banObj ;pZoidMtx<-gEnv$zhF ;pInitZIdx=NULL ;pCodeLst=codeLst   ;pDimThld=2  ;pDepth=2
ban.multiDim <-function( pBanObj ,pZoidMtx ,pCodeLst ,pInitZIdx=NULL ,pDimThld=2 ,pDepth=2 ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

    # --------------------------------------------------
    #	filtLst ,filtedIdx
    filtLst <- list()
    dbgLst <- list()
    for( zIdx in 1:nrow(pZoidMtx) ){
        dbgObj <- list( cfNameFndLst=list() )
        for( nIdx in pBanObj$cfNames ){
            fndIdxLst <- list( )
            for( eIdx in pBanObj$encVal.len:1 ){
                dCnt <- pBanObj$cfObjLst[[nIdx]]$diffCnt( 
                                pCodeLst[[nIdx]][[zIdx]]
                                ,pBanObj$encValLst[[nIdx]][[eIdx]]
                            )
                if(0==dCnt){
                    fndIdxLst[[1+length(fndIdxLst)]] <- eIdx
                }
            }

            fndLen <- length(fndIdxLst)
            if( 0==fndLen ){
                dbgObj$cfNameFndLst[[nIdx]] <- integer(0)
            } else {
                fndLen <- ifelse( fndLen>pDepth ,pDepth ,fndLen )
                dbgObj$cfNameFndLst[[nIdx]] <- do.call( c ,fndIdxLst[1:fndLen] )
            }

            sapply( dbgObj$cfNameFndLst[[nIdx]] ,function(p){
                
            })
        } # nIdx


    } # for(zIdx)


    lapply( pCodeLst ,function(p){p[[75]]})
    lapply( pBanObj$encValLst ,function(p){p[[26]]})

    a10Lst <- pBanObj$encValLst[["A0010_o3"]]
    a30Lst <- pBanObj$encValLst[["A0030_o3"]]

    # filtedIdx <- pInitZIdx[pDimCnt<sapply(filtLst ,length)]

    rstObj <- list( idStr="hntCrossDim" )
    rstObj$filtLst      <- filtLst
    rstObj$filtedIdx    <- filtedIdx
    # 디버깅용 -------

} # ban.multiDim()



