setwd("c:/zproject/ISLRgit/flagBank")   ;source("hCommon.R")    ;setwd("./Bproject")
source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")

#-[Parallel init work]-------------------------------------------------------------
prllNum <- 8     # 실수가 잦아서 그냥 오류 코드로 놔둔다.
prllLog <- k.getFlogObj( "./log/parallel_log_Hist.txt" )
prll.initHeader <- function( ){
    k <- sfLapply(1:prllNum,function(prllId){
        curWd <- getwd();setwd("..");source("hCommon.R")
        setwd( curWd );source("header.r");source("B_H.R");source("B_prll_H.R")
    })
    source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")  # for debug work
}
sfInit( parallel=T, cpus=prllNum )  ;prll.initHeader( ) ;sfExport("prllLog") 


for( lastH in c(800,820,840,860,880,900,920,940,960) ){

    load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
    load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
    names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
    load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))
    crScrH <- crScrHTool$getData( )     ;crScrH <- crScrHTool$bySpan(crScrH,lastH)

    sfExport("lastH")   ;sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst") ;sfExport("crScrH")  ;sfExport("crScrHTool")
    prllLog$fLogStr("parallel init", pTime=T ,pAppend=F )
    cat(sprintf("* Parallel ready... see log : %s \n",prllLog$fileName))

    tgt.scMtx <- NULL   ;configH <- lastH-20
    testSpan <- (lastH - 19:0)  ;sfc.InTest <- allIdxLst$stdFiltedCnt[as.character(testSpan)]   ;testSpan <- testSpan[sfc.InTest %in% 0:2]
    load( sprintf("Obj_testData.grp.%d.%s.save",lastH,"all") )  ;load( sprintf("Obj_testData_HCR.grp.%d.%s.save",lastH,"all") )

    cutRstObj <- list()
    cutRstLst <- NULL
    cutRstLst <- Bprll.stdCutTest( testData.grp ,tgt.scMtx ,testSpan ,exportObj=TRUE )
    cutRstObj$cutRstLst <- cutRstLst
    B.rptCutRstLst( cutRstLst ,file=sprintf("cutRstLst_%d_A",lastH) ,rptBanTyp=NULL ,rptBanM=NULL ,removeDup=T )


    sfExport("tgt.scMtx")       ;sfExport("testData.grp")   ;sfExport("testData_HCR.grp")
    # HCR -------------------------------------------------------------------------
    prll.initHeader( )
    prllLog$fLogStr("- HCR ----------------------------",pTime=T)

    resultLst <- sfLapply( testSpan ,function( curHIdx ){
        # curHIdx <- testSpan[1]

        wLastH <- curHIdx-1
        wLastSpan <- 1:which(names(fRstLst)==wLastH)
        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
        allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                    allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
        fRstLst.w <- fRstLst[wLastSpan]

        curHMtxLst <- testData.grp$curHMtxLst.grp[[as.character(curHIdx)]]
        cut.grp <- bFCust.getFCustGrp( curHMtxLst ,tgt.scMtx )
        curStdFilted <- fRstLst[[as.character(curHIdx)]]
        fHName <- bUtil.getSfcLstName( fRstLst.w[[length(fRstLst.w)]] ,curStdFiltedCnt=length(curStdFilted) ,cut.grp )

        # =============================================================================
        #  HCR.cut1( )
        cutRst <- list( surFlag=T ,cutInfoLst=list() )      # cutRst$surFlag는 의미없다. anaOnly=T 이므로

        curHIdxStr <- as.character(curHIdx)
        crScrA <- list( stdIdx=crScrH$stdIdx[curHIdxStr] ,std.grp=crScrH$std.grp[curHIdxStr] ,bS.grp=crScrH$bS.grp[curHIdxStr] )  # crScr of aZoid

        crScrW <- crScrHTool$bySpan(crScrH,wLastH)
        filterLst <- HCR.getFilter.grp( tgt.scMtx ,crScrW )
        scoreMtx.grp <- HCR.getScoreMtx.grp( crScrA ,filterLst ,tgt.scMtx=tgt.scMtx )
        hMtxLst_HCR <- testData_HCR.grp$curHMtxLst_HCR.grp[[as.character(curHIdx)]]
        cut.grp <- HCR.getCutterGrp( hMtxLst_HCR ,fHName ,tgt.scMtx )   # bFMtx,bSMtx에서도 cut.grp 생성 시 fHName을 적용하도록 개선 요.
        cutRst1 <- HCR.cut1( scoreMtx.grp ,cut.grp ,anaOnly=T ) 
        cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,cutRst1$cutInfoLst )

        resultObj <- list( hIdx=curHIdx ,cutRst=cutRst)
        if( TRUE ){ # for later inspection...
            resultObj$fHName <- fHName
            resultObj$hMtxLst_HCR <- hMtxLst_HCR
            resultObj$scoreMtx.grp <- scoreMtx.grp 
        }

        return( resultObj )
    })
    names( resultLst ) <- sapply( resultLst ,function(p){p$hIdx})

    if( is.null(cutRstLst) ){
        cutRstLst <- lapply( resultLst ,function(p){p$cutRst})
        names(cutRstLst) <- paste("H",testSpan,sep="")
        names(cutRstLst) <- paste( names(cutRstLst) ,allIdxLst$stdFiltedCnt[as.character(testSpan)] ,sep="_" )
    } else {
        cutRstLstHCR <- lapply( resultLst ,function(p){p$cutRst})
        names(cutRstLstHCR) <- paste("H",testSpan,sep="")
        names(cutRstLstHCR) <- paste( names(cutRstLstHCR) ,allIdxLst$stdFiltedCnt[as.character(testSpan)] ,sep="_" )
        B.rptCutRstLst( cutRstLstHCR ,file=sprintf("cutRstLst_%d_B",lastH) ,rptBanTyp=NULL ,rptBanM=NULL ,removeDup=T )
        cutRstObj$cutRstLstHCR <- cutRstLstHCR

        cutRstLst <- lapply( names(cutRstLstHCR) ,function(nIdx){ cutRst <- cutRstLst[[nIdx]] 
                    cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,cutRstLstHCR[[nIdx]]$cutInfoLst )
                    return(cutRst)
        })
        names(cutRstLst) <- names(cutRstLstHCR)
    }

    B.rptCutRstLst( cutRstLst ,file=sprintf("cutRstLst_%d_Z",lastH) ,rptBanTyp=NULL ,rptBanM=NULL ,removeDup=T )
    save( cutRstObj ,file=sprintf("./report/workRpt/Obj_cutRstObj_%d.save",lastH) )
    lastH   ;tgt.scMtx

}

