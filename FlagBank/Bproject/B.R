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
    get_testData.grp <- function( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx ){

        curHMtxLst.grp <- list( )
        stdIdx.grp <- list()

        tStmp <- Sys.time()
        for( curHIdx in testSpan ){    # curHIdx <- testSpan[1] # 842

            wLastH <-curHIdx-1
            wLastSpan <- 1:which(names(fRstLst)==wLastH)

            # ------------------------------------------------------------------------
            # curHMtxLst.grp
            gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
            allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                        allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
            fRstLst.w <- fRstLst[wLastSpan]

            curHMtxLst <- B.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w, tgt.scMtx )   

            curHMtxLst.grp[[as.character(curHIdx)]] <- curHMtxLst

            # ------------------------------------------------------------------------
            # stdIdx.grp
            stdZoid <- gEnv$zhF[curHIdx,]
            stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )
            stdIdx.grp[[as.character(curHIdx)]] <- stdIdx
        }
        tDiff <- Sys.time() - tStmp
        cat(sprintf("time : %.1f,%s   \n",tDiff,units(tDiff)))

        return( list(curHMtxLst.grp=curHMtxLst.grp ,stdIdx.grp=stdIdx.grp) )
    }


    tgt.scMtx <- NULL       # default : NULL

    configH <- lastH-20    # configH는 기본 cutting값을 얻기 위하는 시점에 따라 조절.
    hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=configH, tgt.scMtx )
    stdCtrlCfgGrp <- bUtil.makeStdCtrlCfgGrp(hMtxLst)
    save( stdCtrlCfgGrp ,file=sprintf("./save/HMtxLst/Obj_stdCtrlCfgGrp_%d.save",configH) )
    #   load(sprintf("./save/HMtxLst/Obj_stdCtrlCfgGrp_%d.save",configH))


    testSpan <- (lastH - 18:0)   # configH 보다는 큰 시점에서 시작해야 함을 유의.
    testData.grp <- get_testData.grp( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx)  # 반복 테스트를 위한 속도향상
    #   save( testData.grp ,file="Obj_testData.grp.save" )
    #   load( "Obj_testData.grp.save" )

    cutRstLst <- list()
    for( curHIdx in testSpan ){    # curHIdx <- testSpan[1] # 842

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

    B.rptCutRstLst( cutRstLst )
}



if( FALSE ){    # 실전 추출 예제 코드

    # remLst
    load(sprintf("./save/Obj_remLstZ%d.save",lastH) )
    logger <- k.getFlogObj( "./log/cutLog.txt" )

    configH <- 854  # 지정된 지점을 반복사용하므로..
    stdZoid <- gEnv$zhF[configH,]
    stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )


    # stdCtrlCfgGrp
    load("./save/HMtxLst/Obj_stdCtrlCfgGrp_840.save")

    tgt.scMtx <- NULL       # default : NULL        ; c("score2","score3")
    hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, tgt.scMtx=tgt.scMtx )
    cut.grp <- bFCust.getFCustGrp( stdCtrlCfgGrp ,hMtxLst )

    stdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst ) # B.rptStdMI.grp( stdMI.grp )
    stdMI.grp$anyWarn( )

    filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx )

    # ====================================================================
    #   Cutting
    # --------------------------------------------------------------------
    stdFilted.NG <- c("D0000.A","A0100.A","AP000.E")
        #   B.makeHMtxLst() 의 sfcHLst 생성코드 참고.(변수 stdFilter)
    # --------------------------------------------------------------------
    # stdFiltedCnt 그룹에 따라서 각각 시행.
    #   - "allZoid.idx0","allZoid.idx1","allZoid.idx2","allZoid.idx3"
    #   - 그룹 특성에 따라 stdFilted.NG 에서
    #       하나 소속되지 않은 aZoid, 혹은 다수가 소속된 aZoid도 있을 수 있다.
    # --------------------------------------------------------------------
    #   시간 소요 8.8min/20k, 39min/20k
    curStdFiltedCnt <- 1
    allIdx  <- allIdxLst[[sprintf("allZoid.idx%d",curStdFiltedCnt)]]

    allIdxF <- c( stdIdx ,allIdx[sample(1:length(allIdx),200000)] ) 
    fHName <- bUtil.getSfcLstName( fRstLst[[length(fRstLst)]] ,curStdFiltedCnt=curStdFiltedCnt ,cut.grp )

    tStmp <- Sys.time()
    Rprof(filename="Work_Rprof.scoreMtx.out", append=FALSE,  interval=0.02 )
    scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )
    Rprof( NULL )
    tDiff <- Sys.time() - tStmp     ;cat( sprintf("cost : %.1f%s \n",tDiff,units(tDiff)) )

    Rprof(filename="Work_Rprof.out", append=FALSE,  interval=0.02 )
    cutRst <- bUtil.cut( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,logger=logger )
    Rprof( NULL )
    tDiff <- Sys.time() - tStmp     ;cat( sprintf("cost : %.1f%s \n",tDiff,units(tDiff)) )

    # logger$fLogStr("\n\n= Performance Prof======================================")
    # logger$fLog( summaryRprof("Work_Rprof.out") )

    allIdxF <- allIdxF[cutRst$surFlag]
    rptStr <- sprintf( "Initial cut : %d -> %d \n" ,length(allIdx) ,length(allIdxF) )
    cat( rptStr )
    allIdx <- allIdxF

    # --------------------------------------------------------------------
    # * stdFiled 한가지씩 적용
    for( sfIdx in stdFilted.NG ){   # sfIdx <- stdFilted.NG[1]
        allIdxF <- intersect( allIdxF ,remLst[[sfIdx]] )

        scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF,,drop=F] ,filter.grp )
        fHName <- bUtil.getSfcLstName( fRstLst[[length(fRstLst)]] ,curStdFilted=sfIdx ,cut.grp )
        cutRst <- bUtil.cut( scoreMtx.grp ,cut.grp ,fHName )
        allIdxF <- allIdxF[cutRst$surFlag]
        rptStr <- sprintf( "        left : %d (for stdFilted %s)\n" ,length(allIdxF),sfIdx )
        cat( rptStr )
    }

    # --------------------------------------------------------------------
    # * stdFilted 가 없는 aZoid들.
    allIdxF.0 <- allIdxF
    for( sfIdx in stdFilted.NG ){
        allIdxF.0 <- setdiff( allIdxF.0 ,remLst[[sfIdx]] )
    }
    scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF.0,,drop=F] ,filter.grp )
    fHName <- bUtil.getSfcLstName( fRstLst[[length(fRstLst)]] ,curStdFilted=sfIdx ,cut.grp )
    cutRst <- bUtil.cut( scoreMtx.grp ,cut.grp ,fHName )
    allIdxF <- allIdxF[cutRst$surFlag]



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