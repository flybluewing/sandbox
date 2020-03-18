source("header.r")
source("B_H.R")
lastH <- 820    # 최종 데이터의 로딩 기준일 뿐, 작업시점(workH)은 다를 수 있다.
#source(sprintf("./toFinal/toZ%d_H.R",workH))	# working

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))


#-[Parallel init work]-------------------------------------------------------------
prllNum <- 1     # 실수가 잦아서 그냥 오류 코드로 놔둔다.
prllLog <- k.getFlogObj( "./log/parallel_log.txt" )
prll.initHeader <- function( ){
    k <- sfLapply(1:prllNum,function(prllId){
        curWd <- getwd()
        setwd("..")
        source("hCommon.R")

        setwd( curWd )
        source("header.r")
        source("B_H.R")
    })
}

sfInit( parallel=T, cpus=prllNum )
sfExport("prllLog") ;sfExport("lastH")
sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst")
prll.initHeader( )
prllLog$fLogStr("parallel init", pTime=T ,pAppend=F )
cat(sprintf("* Parallel ready... see log : %s \n",prllLog$fileName))



if( FALSE ){    # stdZoid에 대한 cutting 시뮬레이션 예제 코드

    tgt.scMtx <- c("score5")       # default : NULL   하도 실수가 잦아서 일부러 문법 오류로 놔둔다.. -_-;
        #   "bScr01"

    configH <- lastH-20    # configH는 기본 cutting값을 얻기 위하는 시점에 따라 조절.
    testSpan <- (lastH - 18:0)   # configH 보다는 큰 시점에서 시작해야 함을 유의.
    if( TRUE ){ # stdFiltedCnt 0~2내에서만 테스트
        sfc.InTest <- allIdxLst$stdFiltedCnt[as.character(testSpan)]
        testSpan <- testSpan[sfc.InTest %in% 0:2]
    }

    hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=configH, tgt.scMtx )
    testData.grp <- B.get_testData.grp( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx)
    save( testData.grp ,file=sprintf("Obj_testData.grp.%d.%s.save",lastH,ifelse(is.null(tgt.scMtx),"all",tgt.scMtx) ) )
    #   save( testData.grp ,file="Obj_testData.grp.save" )
    #   load( "Obj_testData.grp820.save" )

    sfExport("testData.grp")    ;sfExport("tgt.scMtx")
    prll.initHeader( )              #   source("FCust_configBasic.R")
    prllLog$fLogStr("- bUtil.cut() ----------------------------",pTime=T)
    resultLst <- sfLapply( testSpan ,function( curHIdx ){
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

        cut.grp <- bFCust.getFCustGrp( curHMtxLst ,tgt.scMtx )  # curHMtxLst 적용 추가 필요.
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

        prllLog$fLogStr(sprintf("    curHIdx:%d done.",curHIdx),pTime=T)
        if( TRUE ){ # debug info
            dbgFileName <- sprintf("Dbg_H%d.stdMI",curHIdx)

            B.rptStdMI.grp( stdMI.grp ,file=dbgFileName )

            log.c <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",dbgFileName) )
            stdZoid.str <- sprintf("stdZoid : %s",paste( sprintf("%2d",stdZoid) ,collapse=" ") )
            log.c$fLogStr( stdZoid.str )

            scMtx <- getScoreMtx.grp_byHIdx( scoreMtx.grp )[[1]][[1]]
            colnames(scMtx) <- bUtil.getShortPhaseName( colnames(scMtx) )
            log.c$fLog( scMtx )

        }

        return( list(hIdx=curHIdx ,cutRst=cutRst) )
    })
    names( resultLst ) <- sapply( resultLst ,function(p){p$hIdx})
    cutRstLst <- lapply( resultLst ,function(p){p$cutRst})
    names(cutRstLst) <- paste("H",testSpan,sep="")
    names(cutRstLst) <- paste( names(cutRstLst) ,allIdxLst$stdFiltedCnt[as.character(testSpan)] ,sep="_" )


    save( cutRstLst ,file=sprintf("./save/HMtxLst/Obj_cutRstLst%d.save",configH) )
        # load("./save/HMtxLst/Obj_cutRstLst840.save")

    rptFile <- ifelse(1==length(tgt.scMtx),sprintf("cutRstLst_%d",length(tgt.scMtx)),"cutRstLst")
    if( 1==length(tgt.scMtx) ){
        rptFile <- ifelse(1==length(tgt.scMtx),sprintf("cutRstLst_%s",tgt.scMtx),"cutRstLst")
        rptFile
    }
    # rptBanTyp <- c(   "rawFCol" ,"rowE" ,"rawReb"
    #                   ,"scMtx.sz.cut rebCnt" ,"scMtx.sz.cut rebCnt.e.sum"
    #               )
    rptBanTyp <- NULL
    B.rptCutRstLst( cutRstLst ,file=rptFile ,rptBanTyp=rptBanTyp )

}


