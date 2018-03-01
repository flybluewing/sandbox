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

allZoid.fltCnt <- getAllZoidIdx.FltCnt( gEnv ,remLst )
allZoidMtx <- gEnv$allZoidMtx[(allZoid.fltCnt==0),]

tStmp <- Sys.time()
filtedIdxObj <- banObj$getFiltedIdx( allZoidMtx )
filtedIdx <- unique( filtedIdxObj$filtedIdx.dupRow ,filtedIdxObj$filtedIdx.cf1 )
    # filtedIdxObj$filtedIdx.dupRow 에게 모두 파묻히는 거 같은데.. 뭔가 수상타?
tDiff <- Sys.time() - tStmp
cat(sprintf("time cost %.1f%s \n",tDiff,units(tDiff)))

# ===============================================================================


#   동일 패턴이 그대로 재발.
ban.hntSameRow <- function( pBanObj ,pZoidMtx ,pInitZIdx=NULL ,pCodeLst=NULL ){

    if( is.null(pInitZIdx) ){
        pInitZIdx <- 1:nrow(pZoidMtx)
    }

    codeLst <- pCodeLst
    if( is.null(codeLst) ){
        codeLst <- list()
        for( nIdx in pBanObj$cfNames ){
            codeLst[[nIdx]] <- pBanObj$cfObjLst[[nIdx]]$enc(pZoidMtx)
        }
    }

    # --------------------------------------------------
    #	filtLst.dupRow ,filtedIdx.dupRow
    excBan=c("A0070_o3")
    filtLst <- lapply( 1:nrow(pZoidMtx) ,function(pIdx){
            # 어느 banLst.dupRow에서 걸렸는지의 flag
            flag <- sapply(pBanObj$cfNames ,function(pName){
                            if( pName %in% excBan ){
                                return( FALSE )
                            }
                            cfObj <- pBanObj$cfObjLst[[pName]]
                            dCnt <- cfObj$diffCnt( codeLst[[pName]][[pIdx]] ,pBanObj$banLst.dup[[pName]] )
                            return( dCnt==0 )
                        })
            return( pBanObj$cfNames[flag] )
        })
    filtedIdx <- pZIdx[0<sapply(filtLst ,length)]



} # ban.hntSameRow()





