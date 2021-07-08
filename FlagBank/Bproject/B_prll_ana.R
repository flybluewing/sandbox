setwd("c:/zproject/ISLRgit/FlagBank")   ;source("hCommon.R")    ;setwd("./Bproject")
source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")  ;source("B_prll_Hrpt.R")
lastH <- 900
if( TRUE ){
    load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
    load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )    ;names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
    load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))
    crScrH <- crScrHTool$getData( )     ;crScrH <- crScrHTool$bySpan(crScrH,lastH)

    prllLog <- k.getFlogObj( "./log/parallel_log_Hist.txt" )
    prll.initHeader <- function( ){
        k <- sfLapply(1:prllNum,function(prllId){
            curWd <- getwd();setwd("..");source("hCommon.R")
            setwd( curWd );source("header.r");source("B_H.R");source("B_prll_H.R")
        })
        source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")  ;source("B_prll_Hrpt.R")# for debug work
    }
    prllNum <- 8    ;sfInit( parallel=T, cpus=prllNum )        ;prll.initHeader( )

    sfExport("prllLog")     ;sfExport("lastH")   ;sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst") ;sfExport("crScrH")  ;sfExport("crScrHTool")
    prllLog$fLogStr("parallel init", pTime=T ,pAppend=F )

    tgt.scMtx <- NULL   ;tgt.scMtx <- c( tgt.scMtx  ,Bprll.getTgtScMtx( ) )
    configH <- lastH-20
    testSpan <- (lastH - 19:0)  ;sfc.InTest <- allIdxLst$stdFiltedCnt[as.character(testSpan)]   ;testSpan <- testSpan[sfc.InTest %in% 0:2]
    load( sprintf("Obj_testData.grp.%d.%s.save",lastH,"all") )
    load( sprintf("Obj_testData_HCR.grp.%d.%s.save",lastH,"all") )
}
myObj <- load("./save/Obj_hMtxLstBig.save")     # hMtxLstBig    < HBuild_HMtxLstBig.R >
logTemp <- k.getFlogObj( "./log/dbg_Temp.txt" )
logTemp$fLogStr( sprintf("log start : %s",Sys.time()) ,pAppend=F )  ;fLogger<-logTemp



#   다음은 이미 로딩되어 있다고 가정.
#   lastH ,testData.grp ,tgt.scMtx
curHIdx <- 892  ;cutRst <- list( surFlag=T ,cutInfoLst=list() )
if( TRUE ){
    wLastH <- curHIdx-1 ;wLastSpan <- 1:which(names(fRstLst)==wLastH)
    gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
    allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]  ;allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
    fRstLst.w <- fRstLst[wLastSpan]

    stdZoid <- gEnv$zhF[curHIdx,]   ;stdIdx <- testData.grp$stdIdx[[as.character(curHIdx)]]
    stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst.w )
    filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
    scoreMtx.grp <- getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,makeInfoStr=F )    # makeInfoStr=T

    curStdFilted <- fRstLst[[as.character(curHIdx)]]    #   평가가 아닌 실제에선, remLst 으로부터 가져올 것.
    curHMtxLst <- testData.grp$curHMtxLst.grp[[as.character(curHIdx)]]
    cut.grp <- bFCust.getFCustGrp( curHMtxLst ,tgt.scMtx )
    fHName <- bUtil.getSfcLstName( fRstLst.w[[length(fRstLst.w)]] ,curStdFiltedCnt=length(curStdFilted) ,cut.grp )
}

# std ----------------------------------
if( TRUE ){ }

# bS  ----------------------------------
if( TRUE ){ }

# HCR ----------------------------------
if( TRUE ){
    curHIdxStr <- as.character(curHIdx)
    crScrA <- list( stdIdx=crScrH$stdIdx[curHIdxStr] ,std.grp=crScrH$std.grp[curHIdxStr] ,bS.grp=crScrH$bS.grp[curHIdxStr] )  # crScr of aZoid

    crScrW <- crScrHTool$bySpan(crScrH,wLastH)
    filterLst <- HCR.getFilter.grp( tgt.scMtx ,crScrW )
    scoreMtx.grp <- HCR.getScoreMtx.grp( crScrA ,filterLst ,tgt.scMtx=tgt.scMtx )
    hMtxLst_HCR <- testData_HCR.grp$curHMtxLst_HCR.grp[[as.character(curHIdx)]]

    cut.grp <- HCR.getCutterGrp( hMtxLst_HCR ,fHName ,tgt.scMtx )   # bFMtx,bSMtx에서도 cut.grp 생성 시 fHName을 적용하도록 개선 요.
    cutRst1 <- HCR.cut1( scoreMtx.grp ,cut.grp ,anaOnly=T ) 
    cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,cutRst1$cutInfoLst )
}


cutInfo <- cutRst1$cutInfoLst[[1]]

logHCR <- k.getFlogObj( sprintf("./log/dbg_%dHCR.txt",curHIdx) )
logHCR$fLogStr( sprintf("log start : %s",Sys.time()) ,pAppend=F )
Bprll_lastMtxHCR( cutInfo ,scoreMtx.grp ,hMtxLst_HCR ,hNameAll=T ,fLogger=logHCR )

logSzInspec <- k.getFlogObj( sprintf("./log/dbg_%dSzInspec.txt",curHIdx) )
logSzInspec$fLogStr( sprintf("log start : %s",Sys.time()) ,pAppend=F )
Bprll_inspecSZ_std( lastH ,curHIdxSet=curHIdx ,fLogger=logSzInspec ,gEnv,allIdxLst,fRstLst,crScrH,hMtxLstBig )

