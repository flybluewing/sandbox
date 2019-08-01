source("header.r")
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
    #     855      1    1    0      0      0      0       1       0       0     0     0     0      1      1      0      0      0
    #     856      2    0    0      2      1      0       0       1       0     0     0     0      0      0      0      0      0
    #     857      1    0    0      0      0      0       0       0       1     0     0     0      0      0      0      0      0
    #     858      1    0    0      0      2      0       0       0       0     0     0     0      0      0      0      1      0
    #     859      2    0    0      2      1      0       0       0       0     0     0     0      0      0      2      0      0

    # 기본 필터링 설정값 형성.
    #   TODO : 할 거 졸라 많음.
    stdCtrlCfgGrp <- bUtil.makeStdCtrlCfgGrp(hMtxLst)

    # ctrlCfgGrp를 이용한 필터링 객체 설정.
    #   TODO : 최근 hMtxLst도 넣어줘야 한다.
    #          사용자 정의 filt 대체 적용 필요.
    cut.grp <- bFCust.getFCustGrp( stdCtrlCfgGrp )


    # TODO : mtxLst의 가장 최근 값( pattern rebound 체크용) 가져오는 루틴 작성.

    # TODO : aZoid,stdZoid에 대한 필터링 코드 작성.
    #       예시코드 : ctrlCfg를 사용한 필터링.
    #               flagMtx <- bUtil.filtByCtrlCfg( hVal ,ctrlCfg )

    fHName <- c( "sfcLate",   "NGD0000.A")  # fHName 분석하는 루틴 필요.

    stdZoid <-  c( 4, 8,18,25,27,32)    # H860
    #   stdZoid <- gEnv$zhF[nrow(gEnv$zhF),]
    stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )
    allIdxF <- stdIdx + (0:10*200000)
    # bUtil.cutAZoidMtx( gEnv ,allIdxF ,cutGrp )

    wScoreMtx.grp <- NULL
    if( TRUE ){
        wStdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst )
        wFilter.grp <- getFilter.grp( wStdMI.grp )
        wScoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,wFilter.grp )

        cutRst <- bUtil.cut( wScoreMtx.grp ,cut.grp ,fHName ,anaOnly=T )

        # report example =================================================
        # B.rptStdMI.grp( wStdMI.grp )
        # B.rptScoreMtx.grp( wScoreMtx.grp )
        # B.rptCut.grp( cut.grp )
    }

    
    # TODO : stdZoid 필터링 결과 리뷰
    #   aZoid에 대한 scoreMtx 생성
    #


}

#   source("header.r")