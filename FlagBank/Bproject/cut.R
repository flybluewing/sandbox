source("header.r")
source("B_H.R")
lastH <- 913
tgt.scMtx <- NULL
testMode <- TRUE        # check

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )    ;names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))

#-[Parallel init work]-------------------------------------------------------------
prllNum <- 3     # ½Ç¼ö°¡ Àæ¾Æ¼­ ±×³É ¿À·ù ÄÚµå·Î ³öµÐ´Ù.
prllLog <- k.getFlogObj( "./log/parallel_log_Cut.txt" )
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


tStmp <- Sys.time()

# ----------------------------------------------------------------------------------
stdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst )     ;stdMI.grp$anyWarn( )
hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=lastH, tgt.scMtx )
cut.grp <- bFCust.getFCustGrp( hMtxLst ,tgt.scMtx )
filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
sfExport("tgt.scMtx")   ;sfExport("hMtxLst")    ;sfExport("cut.grp")    ;sfExport("filter.grp")

tDiff <- Sys.time() - tStmp
sprintf("hMtxLs,cut.grp    Time cost : %.1f%s",tDiff,units(tDiff))



# ----------------------------------------------------------------------------------

for( sfcIdx in 0 ){ # 0:2

    aZoidGrpName <- sprintf("allZoid.idx%d",sfcIdx)
    fHName <- bUtil.getSfcLstName( fRstLst[[length(fRstLst)]] ,curStdFiltedCnt=sfcIdx ,cut.grp )

    logger <- k.getFlogObj( sprintf("./log/FinalCut_%d_%s.txt",lastH,aZoidGrpName) )
    logger$fLogStr(sprintf("start %s",aZoidGrpName),pAppend=F,pTime=T)

    allIdxF <- allIdxLst[[aZoidGrpName]]
    if( testMode ){
        allIdxF <- allIdxF[sample(1:length(allIdxF),500)]
    }
    save( allIdxF ,file=sprintf("Obj_allIdxF%d_cutInit.save",sfcIdx) )
    logger$fLogStr(sprintf("Initial size :%7d",length(allIdxF)),pTime=T)

    #   primary cut --------------------------------------------------------------------
    # allIdxF <- FC.primaryCut.static( allIdxF ,gEnv )
    # allIdxF <- FC.primaryCut.cust( allIdxF ,gEnv )
    # logger$fLogStr(sprintf("FC.primaryCut :%7d",length(allIdxF)),pTime=T)


    # bUtil.cut1() ----------------------------------------------------------------------
    surFlag <- rep( T ,length(allIdxF) )
    bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,2,1000) )

    sfExport("fHName")  ;sfExport("allIdxF")
    resultLst <- sfLapply( bLst ,function( blk ){
        tStmp <- Sys.time()
        span1nd <- blk["start"]:blk["end"]
        scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[span1nd],,drop=F] ,filter.grp )

        cutRst <- bUtil.cut1( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,anaOnly=F )

        tDiff <- Sys.time() - tStmp
        logStr <- sprintf("  block finished for cut1. %d/%d  %5.1f%s for %d~%d "
                            ,sum(!cutRst$surFlag),length(cutRst$surFlag)
                            ,tDiff  ,units(tDiff)
                            ,blk["start"] ,blk["end"]
                    )
        prllLog$fLogStr( logStr )
        
        return( list( surFlag=cutRst$surFlag ,blk=blk ) )
    })
    for( idx in seq_len(length(resultLst)) ){
        blk <- resultLst[[idx]]$blk
        surFlag[ blk["start"]:blk["end"] ] <- resultLst[[idx]]$surFlag
    }
    allIdxF <- allIdxF[surFlag]
    logger$fLogStr(sprintf("   - bUtil.cut1()   final size :%7d",length(allIdxF)),pTime=T)
    save( allIdxF ,file=sprintf("Obj_allIdxF%d_cut1.save",sfcIdx) )

    # bUtil.cut2() ----------------------------------------------------------------------
    surFlag <- rep( T ,length(allIdxF) )
    bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,2,1000) )

    sfExport("allIdxF")
    resultLst <- sfLapply( bLst ,function( blk ){

        tStmp <- Sys.time()
        span1nd <- blk["start"]:blk["end"]
        scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[span1nd],,drop=F] ,filter.grp )

        cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )
        cut2Rst <- bUtil.cut2( cutRst1Score ,fHName ,tgt.scMtx=tgt.scMtx ,anaOnly=F ) 

        tDiff <- Sys.time() - tStmp
        logStr <- sprintf("  block finished for cut2. %d/%d  %5.1f%s for %d~%d "
                            ,sum(!cut2Rst$surFlag),length(cut2Rst$surFlag)
                            ,tDiff  ,units(tDiff)
                            ,blk["start"] ,blk["end"]
                    )
        prllLog$fLogStr( logStr )
        
        return( list( surFlag=cut2Rst$surFlag ,blk=blk ) )
    })
    for( idx in seq_len(length(resultLst)) ){
        blk <- resultLst[[idx]]$blk
        surFlag[ blk["start"]:blk["end"] ] <- resultLst[[idx]]$surFlag
    }
    allIdxF <- allIdxF[surFlag]
    logger$fLogStr(sprintf("   - bUtil.cut2()   final size :%7d",length(allIdxF)),pTime=T)
    save( allIdxF ,file=sprintf("Obj_allIdxF%d_cut2.save",sfcIdx) )



}   # sfcIdx

