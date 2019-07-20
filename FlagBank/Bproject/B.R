source("B_H.R")
lastH <- 859
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("./save/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("./save/Obj_fRstLstZ%d.save",lastH) )
load(sprintf("./save/Obj_gEnvZ%d.save",lastH))


# Remove before flight
#   B_H.R   -  경고! 실제 동작에서는 Q_RBF 해제할 것.
hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst )
byFCol <- B.getHMtxLst_byFCol( hMtxLst )

save( hMtxLst ,file=sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",hMtxLst$lastH) )
#   load(sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",lastH))

B.rptHMtxLst( hMtxLst )

if(FALSE){
    for( hIdx in (lastH-10:0) ){
        hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=hIdx )
        save( hMtxLst ,file=sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",hMtxLst$lastH) )
    }
}



if(FALSE){  # working code

    scoreMtx <- hMtxLst[["scoreMtxLst"]][["sfcLate"]][["basic"]][["score2"]]$scoreMtx
    # > scoreMtx
    #         rebV.r rebL rebR rebC.r rebC.c rebC.f rebC2.r rebC2.c rebC2.f inc.r inc.c inc.f inc.r2 inc.c2 inc.f2 inc.r3 inc.c3
    #     849      1    0    0      1      0      0       0       0       0     0     0     0      1      0      3      0      0
    #     850      1    0    0      1      1      1       1       0       0     1     0     0      0      0      0      1      0
    #     851      0    0    0      0      3      0       0       2       0     0     1     0      0      0      0      0      0
    #     852      0    0    0      0      0      0       0       0       1     0     0     0      0      1      0      0      0
    #     853      0    0    1      0      1      0       2       0       0     0     1     0      0      0      0      0      0
    #     854      0    0    0      0      0      0       0       0       0     0     0     0      1      0      0      0      0
    #     855      1    1    0      0      0      0       1       0       0     0     0     0      1      1      0      0      0
    #     856      2    0    0      2      1      0       0       1       0     0     0     0      0      0      0      0      0
    #     857      1    0    0      0      0      0       0       0       1     0     0     0      0      0      0      0      0
    #     858      1    0    0      0      2      0       0       0       0     0     0     0      0      0      0      1      0
    #     859      2    0    0      2      1      0       0       0       0     0     0     0      0      0      2      0      0

    hVal <- scoreMtx[,"rebC.c"]

}

getCtrlVal <- function( hVal ){
    vUnq <- sort(unique(hVal),decreasing=T)
    vTbl <- table(hVal)[as.character(vUnq)]
    vTbl.len <- length(vTbl)

    maxMin <- vUnq[c(1,vTbl.len)] # valRange 범위 내에서만 허용.(2개 모두 같은 값일수도 있다.)
    evtVal <- integer(0)    # event로서 다룰 값.(주로 maxMin값이지만 발생빈도가 낮은 값.)
    extVal <- integer(0)    # min,max값이었으나, 발생 빈도가 1개라 maxMin에서 제외된 값.

    if( 2==vTbl.len){
        # extVal 존재여부 확인.
        # evtVal 존재여부 확인.
    } else if( 3==vTbl.len ){

    } else if( 4==vTbl.len ){

    } else {

    }

} # getCtrlVal( )
