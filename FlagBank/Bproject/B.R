source("header.r")
source("B_H.R")
lastH <- 859    # 최종 데이터의 로딩기준이지, 작업시점(workH)은 다를 수 있다.
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("./save/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("./save/Obj_fRstLstZ%d.save",lastH) )
load(sprintf("./save/Obj_gEnvZ%d.save",lastH))

# Remove before flight
#   B_H.R   -  경고! 실제 동작에서는 Q_RBF 해제할 것.

if( FALSE ){ # report sample
    workH <- lastH-3    # workH 는 그때그때 필요에 따라.
    hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=workH )

    save( hMtxLst ,file=sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",hMtxLst$lastH) )
    #   load(sprintf("./save/HMtxLst/Obj_hMtxLst_%d.save",lastH))
    
    B.rptHMtxLst( hMtxLst )
    #   B.getHMtxLst_byFCol( ), B.getHMtxLst_byHIdx( ) 사용은
    #   report 함수 내 코드 참조.
}

if( FALSE ){    # stdZoid에 대한 cutting 시뮬레이션 예제 코드

    configH <- lastH-20    # configH는 기본 cutting값을 얻기 위하는 시점에 따라 조절.
    hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=configH )
    stdCtrlCfgGrp <- bUtil.makeStdCtrlCfgGrp(hMtxLst)
    save( stdCtrlCfgGrp ,file=sprintf("./save/HMtxLst/Obj_stdCtrlCfgGrp_%d.save",configH) )
    #   load(sprintf("./save/HMtxLst/Obj_stdCtrlCfgGrp_%d.save",configH))
    
    for( curHIdx in (lastH- 15:0) ){

        #   gEnv.w, allIdxLst.w, fRstLst.w 모두 curHIdx 시점에 맞게 변경 요.
        gEnv.w <- gEnv
        allIdxLst.w <- allIdxLst
        fRstLst.w <- fRstLst

        curHMtxLst <- B.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w, lastH=curHIdx )
        cut.grp <- bFCust.getFCustGrp( stdCtrlCfgGrp )  # curHMtxLst 적용 추가 필요.

        stdZoid <-  c( 4, 8,18,25,27,32)    # H860
        #   stdZoid <- gEnv$zhF[nrow(gEnv$zhF),]
        stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )
        fHName <- c( "sfcLate",   "NGD0000.A")  # fHName 분석하는 루틴 필요.

        # wStdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst )
        # wFilter.grp <- getFilter.grp( wStdMI.grp )
        # wScoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,wFilter.grp )

        # cutRst <- bUtil.cut( wScoreMtx.grp ,cut.grp ,fHName ,anaOnly=T )


        # report example =================================================
            # B.rptStdMI.grp( wStdMI.grp )
            # B.rptScoreMtx.grp( wScoreMtx.grp )
            # B.rptCut.grp( cut.grp )
            # B.rptCutRst( cutRst )

    } # curHIdx

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
        # B.rptCutRst( cutRst )
    }

    # custom ctrlCfg
    # stdColCut/sfcLate/score2/basic/inc.f/inc.f  (typ/hName/mName/pName/fcName/fCol)
    
    # TODO : stdZoid 필터링 결과 리뷰
    #   aZoid에 대한 scoreMtx 생성
    #


}

#   source("header.r")