#   BProject 디렉토리에서 실행한다고 전제.
source("header.r")
source("B_H.R")


lastH <- 860
testMode <- TRUE

stdFiltedCnt <- 0:2
scoreMtx.name <- c("score2","score3","score4","score5","score6","score6")
stdFilted.NG <- c("D0000.A","A0100.A","AP000.E")

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))
load(sprintf("./save/Obj_remLstZ%d.save",lastH) )

# -----------------------------------------------------------------------------------------
tStmp <- Sys.time()
stdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst )     ;stdMI.grp$anyWarn( )

hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=lastH, scoreMtx.name )
stdCtrlCfgGrp <- bUtil.makeStdCtrlCfgGrp(hMtxLst)
cut.grp <- bFCust.getFCustGrp( stdCtrlCfgGrp ,hMtxLst )

tDiff <- Sys.time() - tStmp
sprintf("Time cost : %.1f%s",tDiff,units(tDiff))
# -----------------------------------------------------------------------------------------

for( curStdFiltedCnt in stdFiltedCnt ){   # curStdFiltedCnt <- stdFiltedCnt[1]

    aZoidGrp <- sprintf("allZoid.idx%d",curStdFiltedCnt)
    fHName <- bUtil.getSfcLstName( fRstLst[[length(fRstLst)]] ,curStdFiltedCnt=curStdFiltedCnt ,cut.grp )

    logger <- k.getFlogObj( sprintf("./log/FinalCut_%d_%s.txt",lastH,aZoidGrp) )
    logger$fLogStr(sprintf("start %s",aZoidGrp),pAppend=F,pTime=T)


    allIdxF <- allIdxLst[[aZoidGrp]]
    if( testMode ){
        allIdxF <- allIdxF[sample(1:length(allIdxF),5000)]
    }
    logger$fLogStr(sprintf("Initial size :%7d",length(allIdxF)),pTime=T)


    #   primary cut --------------------------------------------------------------------
    allIdxF <- FC.primaryCut.static( allIdxF ,gEnv )
    #   allIdxF <- FC.primaryCut.cust( allIdxF ,gEnv )
    logger$fLogStr(sprintf("FC.primaryCut :%7d",length(allIdxF)),pTime=T)

    #   primary cut .byScoreMtx --------------------------------------------------------
    for( tgt.scMtx in scoreMtx.name ){   # tgt.scMtx <- scoreMtx.name[1]

        filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )

        # FC.primaryCut.bySC <- function( allIdxF ,gEnv ,filter.grp ,fHName ,cut.grp ,logFile="primaryCut.bySC.txt" )
        logFile <- sprintf("primaryCut.bySC_H%d_%s_%s.txt",lastH,aZoidGrp,tgt.scMtx)
        allIdxF <- FC.primaryCut.bySC( allIdxF ,gEnv ,filter.grp ,fHName ,cut.grp ,logFile=logFile)

        logger$fLogStr(sprintf("FC.primaryCut.bySC - size :%7d  tgt.scMtx:%s",length(allIdxF),tgt.scMtx),pTime=T)
    }
    gc()

    #   bUtil.cut( ) --------------------------------------------------------------------
    for( tgt.scMtx in scoreMtx.name ){   # tgt.scMtx <- scoreMtx.name[1]

        logger$fLogStr(sprintf("bUtil.cut( ) - tgt.scMtx:%s",tgt.scMtx),pTime=T)

        filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )

        surFlag <- rep( T ,length(allIdxF) )
        bLst <- k.blockLst( length(allIdxF) ,1000*ifelse(testMode,1,100) )
        for( bName in names(bLst) ){    # bName <- names(bLst)[1]
            tStmp <- Sys.time()
            span <- bLst[[bName]]["start"]:bLst[[bName]]["end"] 

            logger.cut <- k.getFlogObj( sprintf("./log/FinalCut_%d_%s_bUtil.cut.txt",lastH,aZoidGrp) )

            scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[span],,drop=F] ,filter.grp )
            cutRst <- bUtil.cut( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,logger=logger.cut )
            surFlag[span] <- cutRst$surFlag

            tDiff <- Sys.time() - tStmp
            logStr <- sprintf("  %s block finished.(remove:%d/%d) %5.1f%s"
                                ,bName,sum(!cutRst$surFlag),length(cutRst$surFlag)
                                ,tDiff  ,units(tDiff)
                        )
        }
        allIdxF <- allIdxF[surFlag]
        logger$fLogStr(sprintf("   - final size :%7d",length(allIdxF)),pTime=T)
    }


    #   QQE 다중 scoreMtx 필터링 추가.

    rptFile <- sprintf("./report/FinalCut_H%d%s_%d.txt",lastH,aZoidGrp,length(allIdxF) )
    zoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]     ;rownames(zoidMtx) <- sprintf("%3d:",1:nrow(zoidMtx))
    finalRpt <- k.getFlogObj( rptFile )
    finalRpt$fLogMtx( zoidMtx ,pIndent=" " )
    logger$fLogStr( sprintf("      reported in %s",rptFile) ,pConsole=T )
    logger$fLogStr( "Mission complete." ,pConsole=T ,pTime=T )


}   # for( stdFiltedCnt )
