source("header.r")
source("B_H.R")
lastH <- 860    # 최종 데이터의 로딩 기준일 뿐, 작업시점(workH)은 다를 수 있다.
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))

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

    names(fRstLst) <- names(allIdxLst$stdFiltedCnt)

    tgt.scMtx <-        # default : NULL   하도 실수가 잦아서 일부러 문법 오류로 놔둔다.. -_-;
        #   "bScr01"

    configH <- lastH-20    # configH는 기본 cutting값을 얻기 위하는 시점에 따라 조절.
    hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=configH, tgt.scMtx )
    stdCtrlCfgGrp <- bUtil.makeStdCtrlCfgGrp(hMtxLst)
    save( stdCtrlCfgGrp ,file=sprintf("./save/HMtxLst/Obj_stdCtrlCfgGrp_%d.save",configH) )
    #   load(sprintf("./save/HMtxLst/Obj_stdCtrlCfgGrp_%d.save",configH))


    testSpan <- (lastH - 18:0)   # configH 보다는 큰 시점에서 시작해야 함을 유의.
    if( TRUE ){ # stdFiltedCnt 0~3내에서만 테스트
        sfc.InTest <- allIdxLst$stdFiltedCnt[as.character(testSpan)]
        testSpan <- testSpan[sfc.InTest %in% 0:3]
    }
    testData.grp <- B.get_testData.grp.old( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx)  # 반복 테스트를 위한 속도향상
    #   save( testData.grp ,file="Obj_testData.grp.save" )
    #   load( "Obj_testData.grp.save" )

    cutRstLst <- list()
    for( curHIdx in testSpan ){    # curHIdx <- testSpan[1] # 860

        wLastH <-curHIdx-1
        wLastSpan <- 1:which(names(fRstLst)==wLastH)

        # ------------------------------------------------------------------------
        # cut.grp : cutter grp 을 얻어내자.
        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
        allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                    allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
        fRstLst.w <- fRstLst[wLastSpan]

        curHMtxLst <- testData.grp$curHMtxLst.grp[[as.character(curHIdx)]]
            # B.makeHMtxLst() 의 lastH는 allIdxLst.w$stdFiltedCnt에 의존한다.

        cut.grp <- bFCust.getFCustGrp( stdCtrlCfgGrp ,curHMtxLst )  # curHMtxLst 적용 추가 필요.
            #   B.rptCut.grp( cut.grp )

        # ------------------------------------------------------------------------
        # 이제, 현재 stdZoid의 특성(sfcHLst, scoreMtx)을 얻자.
        stdZoid <- gEnv$zhF[curHIdx,]
        stdIdx <- testData.grp$stdIdx[[as.character(curHIdx)]]
        curStdFilted <- fRstLst[[as.character(curHIdx)]]    #   평가가 아닌 실제에선, remLst 으로부터 가져올 것.
        fHName <- bUtil.getSfcLstName( fRstLst.w[[length(fRstLst.w)]] ,curStdFiltedCnt=length(curStdFilted) ,cut.grp )

        stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst.w )
        filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
        scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
            #   평가용이므로 getScoreMtx.grp.4H() 가 사용됨.   .4H !

        cutRst <- bUtil.cut( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,anaOnly=T ) 
            #   anaOnly=TRUE 에서, cutRst$surFlag는 항상 TRUE임을 유의.
            # report example =================================================
                # B.rptHMtxLst( curHMtxLst )
                # B.rptStdMI.grp( stdMI.grp )
                # B.rptScoreMtx.grp( scoreMtx.grp )
                # B.rptCut.grp( cut.grp )
                # B.rptCutRst( cutRst )

        # ------------------------------------------------------------------------
        cutRstLst[[1+length(cutRstLst)]] <- cutRst

        cat(sprintf("  - %d test done. \n",curHIdx))
    } # curHIdx
    names(cutRstLst) <- paste("H",testSpan,sep="")
    names(cutRstLst) <- paste( names(cutRstLst) ,allIdxLst$stdFiltedCnt[as.character(testSpan)] ,sep="_" )

    save( cutRstLst ,file=sprintf("./save/HMtxLst/Obj_cutRstLst%d.save",configH) )
        # load("./save/HMtxLst/Obj_cutRstLst840.save")

    rptFile <- ifelse(1==length(tgt.scMtx),sprintf("cutRstLst_%d",length(tgt.scMtx)),"cutRstLst")
    if( 1==length(tgt.scMtx) ){
        rptFile <- ifelse(1==length(tgt.scMtx),sprintf("cutRstLst_%s",tgt.scMtx),"cutRstLst")
        rptFile
    }
    B.rptCutRstLst( cutRstLst ,file=rptFile )

}



if( FALSE ){    # 실전 추출 예제 코드
    # bFMM.getMetaScore.grp() in bFMtxMeta.R
    load( "Obj_testData.grp.All.save" )

    testSpan <- (lastH - 18:0)   # configH 보다는 큰 시점에서 시작해야 함을 유의.
    testData.grp <- B.get_testData.grp.old( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx ,get.scoreMtx.grp=T )


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