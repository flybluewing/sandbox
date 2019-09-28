#   주로 Final Cutting 테스트 작업
source("header.r")
source("B_H.R")
source("C_H.R")

hSpan <- as.integer(names(allIdxLst$stdFiltedCnt))

if( FALSE ){
    stdIdxLst <- list()
    for( hIdx in hSpan ){
        idStr <- sprintf("H%d",hIdx)
        stdIdxLst[[idStr]] <- k.getIdx_AllZoidMtx( gEnv, gEnv$zhF[hIdx,] )
    }
    save( stdIdxLst ,file="Obj_stdIdxLst.save" )
} else {
    load( "Obj_stdIdxLst.save" )
}

if( TRUE ){

    filtedLst <- list()
    for( hIdx in hSpan ){
        idStr <- sprintf("H%d",hIdx)
        gEnv.w <- gEnv
        gEnv.w$zhF <- gEnv$zhF[1:(hIdx-1),]
        allIdxF <- c( stdIdxLst[[idStr]] ,1 )
        filtedLst[[idStr]] <- FC.primaryCut.static( allIdxF, gEnv.w, filtTest=T )
    }
    names(filtedLst) <- sprintf("%d_%d",hSpan,allIdxLst$stdFiltedCnt)

}


