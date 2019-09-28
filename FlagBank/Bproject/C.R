#   주로 Final Cutting 테스트 작업
source("header.r")
source("B_H.R")
source("C_H.R")

lastH <- 860    # 최종 데이터의 로딩 기준일 뿐, 작업시점(workH)은 다를 수 있다.
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))




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
    filtedLst.b <- list()
    for( hIdx in hSpan ){
        idStr <- sprintf("H%d",hIdx)
        gEnv.w <- gEnv
        gEnv.w$zhF <- gEnv$zhF[1:(hIdx-1),]
        allIdxF <- c( stdIdxLst[[idStr]] ,stdIdxLst[[idStr]] )
        filtedLst[[idStr]] <- FC.primaryCut.static( allIdxF, gEnv.w, filtTest=T )
        filtedLst.b[[idStr]] <- is.na( FC.primaryCut.static(allIdxF,gEnv.w,filtTest=F)[1] )
    }
    names(filtedLst) <- sprintf("%d_%d",hSpan,allIdxLst$stdFiltedCnt)

    #   filtedLst[0<sapply(filtedLst,length)]
    surMtx <- cbind( allIdxLst$stdFiltedCnt , 0<sapply(filtedLst,length) ,do.call(c,filtedLst.b) )
    colnames(surMtx) <- c("sfc","filted","filted.b")    # 코드 오류가 없다면 "filted","filted.b"은 항상 같아야 한다.
    tapply( surMtx[,"filted"] ,surMtx[,"sfc"] ,function(val){ 
            sprintf("%d/%d(%3.1f%%)",sum(val),length(val),100*sum(val)/length(val) ) 
    })

    allFF <- do.call( c ,filtedLst )
    table( allFF )

    fIdx <- which( sapply(filtedLst,function(cutId){ any(cutId %in% c("04.3.n")) }) )
    hSpan[fIdx]

    stdFiltedCnt.num <- 0
    grpIdx <- which(surMtx[,"sfc"]==stdFiltedCnt.num)
    fLst <- filtedLst[grpIdx]
    fLst[ sapply(fLst,length)>0 ]

}


