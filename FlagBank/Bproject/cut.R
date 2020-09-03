source("header.r")  ;source("B_H.R")    ;source("cut_H.R")

lastH <- 926
tgt.scMtx <- NULL
testMode <- F            #check
prllNum <- 5

QQE:Trouble      # 실수 방지를 위해 의도된 오류코드

# lastH <- 914
# tgt.scMtx <- NULL
# testMode <-         # check

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )    ;names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))

#-[Parallel init work]-------------------------------------------------------------
# prllNum <- 4     # 실수가 잦아서 그냥 오류 코드로 놔둔다.
prllLog <- k.getFlogObj( "./log/parallel_log_Cut.txt" )
prll.initHeader <- function( ){
    k <- sfLapply(1:prllNum,function(prllId){
        curWd <- getwd()
        setwd("..")
        source("hCommon.R")

        setwd( curWd )
        source("header.r")
        source("B_H.R")
        source("cut_H.R")
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
save( hMtxLst ,file=sprintf("Obj_cut_hMtxLst_%d.save",lastH) )
    #   load( sprintf("Obj_cut_hMtxLst_%d.save",lastH) )
cut.grp <- bFCust.getFCustGrp( hMtxLst ,tgt.scMtx )
filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
sfExport("tgt.scMtx")   ;sfExport("hMtxLst")    ;sfExport("cut.grp")    ;sfExport("filter.grp")

tDiff <- Sys.time() - tStmp
sprintf("hMtxLs,cut.grp    Time cost : %.1f%s",tDiff,units(tDiff))  # 16 min



# ----------------------------------------------------------------------------------

for( sfcIdx in 0 ){ # 0:2

    aZoidGrpName <- sprintf("allZoid.idx%d",sfcIdx)
    fHName <- bUtil.getSfcLstName( fRstLst[[length(fRstLst)]] ,curStdFiltedCnt=sfcIdx ,cut.grp )

    logger <- k.getFlogObj( sprintf("./log/FinalCut_%d_%s.txt",lastH,aZoidGrpName) )
    logger$fLogStr(sprintf("start %s",aZoidGrpName),pAppend=F,pTime=T)

    allIdxF <- allIdxLst[[aZoidGrpName]]
    if( testMode ){
        allIdxF <- allIdxF[sample(1:length(allIdxF),1000)]
    }
    save( allIdxF ,file=sprintf("Obj_allIdxF%d_cutInit.save",sfcIdx) )
    logger$fLogStr(sprintf("Initial size :%7d",length(allIdxF)),pTime=T)

    #   primary cut --------------------------------------------------------------------
    # allIdxF <- FC.primaryCut.static( allIdxF ,gEnv )
    # allIdxF <- FC.primaryCut.cust( allIdxF ,gEnv )
    # logger$fLogStr(sprintf("FC.primaryCut :%7d",length(allIdxF)),pTime=T)

    # bUtil.cut1( byMethod ) --------------------------------------------------------
    #   prllNum 2개 에서 1.8 mins 소모(bLst[[1]] : 1~50000)
    surFlag <- rep( T ,length(allIdxF) )
    bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,5,400) )
    prllLog$fLogStr( sprintf("start cut0 for group%d. bLst size %d",sfcIdx,length(bLst)), pTime=T)

    sfExport("fHName")  ;sfExport("allIdxF")    ;sfExport("cutH.InitialCut")
    resultLst <- sfLapply( bLst ,function( blk ){

        tStmp <- Sys.time()
        cutRst <- cutH.InitialCut( gEnv ,allIdxF ,blk ,filter.grp ,cut.grp ,fHName=fHName ,logger=NULL )

        tDiff <- Sys.time() - tStmp
        logStr <- sprintf("  block finished for bUtil.cut0( byMethod Only ). %d/%d  %5.1f%s for %d~%d "
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
    save( allIdxF ,file=sprintf("Obj_allIdxF%d_cut0_byM_%d.save",sfcIdx,lastH) )
    rptStr <- sprintf( "allIdxF size : %dk" ,length(allIdxF) %/% 1000 )
    prllLog$fLogStr( rptStr, pTime=T)   ;rptStr

    # bUtil.cut1( mfName ) ----------------------------------------------------------------------
    for( mfName in names(bFMtxMFltLst) ){
        surFlag <- rep( T ,length(allIdxF) )
        bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,5,10) )
        prllLog$fLogStr( sprintf("start bUtil.cut1(%s) for group%d. bLst size %d",mfName,sfcIdx,length(bLst)), pTime=T)

        sfExport("mfName") ;sfExport("allIdxF")    #    ;sfExport("cutH.bC.Cut")
        resultLst <- sfLapply( bLst ,function( blk ){
            tStmp <- Sys.time()
            mfObj <- bFMtxMFltLst[[mfName]]()

            blkSpan <- blk["start"]:blk["end"]
            scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[blkSpan],,drop=F] ,filter.grp ,tgt.scMtx=mfObj$fltMNames )

            cutRst <- bUtil.cut1( scoreMtx.grp ,cut.grp ,hName ,tgt.scMtx=mfObj$fltMNames ,anaOnly=F )

            tDiff <- Sys.time() - tStmp
            logStr <- sprintf("  block finished for bUtil.cut1(%s). %d/%d  %5.1f%s for %d~%d "
                                ,mfName    ,sum(!cutRst$surFlag)   ,length(cutRst$surFlag)
                                ,tDiff      ,units(tDiff)           ,blk["start"] ,blk["end"]
            )
            prllLog$fLogStr( logStr )

            return( list( surFlag=cutRst$surFlag ,blk=blk ) )
        })

        for( idx in seq_len(length(resultLst)) ){
            blk <- resultLst[[idx]]$blk
            surFlag[ blk["start"]:blk["end"] ] <- resultLst[[idx]]$surFlag
        }
        allIdxF <- allIdxF[surFlag]
        logger$fLogStr(sprintf("   - bUtil.cut1(%s)   survival size :%7d",mfName,length(allIdxF)),pTime=T)
        save( allIdxF ,file=sprintf("Obj_allIdxF%d_cut0_%s_%d.save",sfcIdx,mfName,lastH) )
        rptStr <- sprintf( "allIdxF size : %dk" ,length(allIdxF) %/% 1000 )
        prllLog$fLogStr( rptStr, pTime=T)   ;rptStr
    }
    



    # bC.cut() ----------------------------------------------------------------------
    for( crMName in names(bCMtxCfg) ){  # bUtil.cut2() 대체
        surFlag <- rep( T ,length(allIdxF) )
        bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,5,10) )
        prllLog$fLogStr( sprintf("start bC.cut(%s) for group%d. bLst size %d",crMName,sfcIdx,length(bLst)), pTime=T)

        sfExport("crMName") ;sfExport("allIdxF")    #    ;sfExport("cutH.bC.Cut")
        resultLst <- sfLapply( bLst ,function( blk ){
            tStmp <- Sys.time()

            blkSpan <- blk["start"]:blk["end"]
            scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[blkSpan],,drop=F] ,filter.grp ,tgt.scMtx=tgt.scMtx )

            crCutRst <- bC.cut( crMName ,scoreMtx.grp ,cut.grp ,anaOnly=F )

            tDiff <- Sys.time() - tStmp
            logStr <- sprintf("  block finished for bC.cut(%s). %d/%d  %5.1f%s for %d~%d "
                                ,crMName    ,sum(!crCutRst$surFlag)   ,length(crCutRst$surFlag)
                                ,tDiff      ,units(tDiff)           ,blk["start"] ,blk["end"]
            )
            prllLog$fLogStr( logStr )

            return( list( surFlag=crCutRst$surFlag ,blk=blk ) )
        })
        for( idx in seq_len(length(resultLst)) ){
            blk <- resultLst[[idx]]$blk
            surFlag[ blk["start"]:blk["end"] ] <- resultLst[[idx]]$surFlag
        }
        allIdxF <- allIdxF[surFlag]
        logger$fLogStr(sprintf("   - bC.cut(%s)   survival size :%7d",crMName,length(allIdxF)),pTime=T)
        save( allIdxF ,file=sprintf("Obj_allIdxF%d_bCCut_%s_%d.save",sfcIdx,crMName,lastH) )
        rptStr <- sprintf( "allIdxF size : %dk" ,length(allIdxF) %/% 1000 )
        prllLog$fLogStr( rptStr, pTime=T)   ;rptStr

    }



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
    save( allIdxF ,file=sprintf("Obj_allIdxF%d_cut2_%d.save",sfcIdx,lastH) )



}   # sfcIdx

