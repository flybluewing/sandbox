source("header.r")  ;source("B_H.R")    ;source("cut_H.R")
if( FALSE ){    # document
    # lastH <- 893(for 894_1 stdIdx:7913455 )   16
    # lastH <- 898(for 899_1 stdIdx:5739750 )
    # lastH <- 933(for 934_1 stdIdx: 233713 )   26  
}

lastH <- 952    # for H899_1
tgt.scMtx <- NULL
testMode <- F            #check
prllNum <- 5

QQE:Trouble      # 실수 방지를 위해 의도된 오류코드

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )    ;names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))
sapply( allIdxLst ,length )

#-[Parallel init work]-------------------------------------------------------------
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
#       stdIdx <- k.getIdx_AllZoidMtx( gEnv, stdZoid )
stdMI.grp <- bUtil.getStdMILst( gEnv ,fRstLst )     ;stdMI.grp$anyWarn( )
crScrH <- crScrHTool$getData( )     ;crScrH <- crScrHTool$bySpan(crScrH,lastH)
if( TRUE ){     #   hMtxLst ,hMtxLst_bS
    load( sprintf("./save/finalCut/Obj_cut_hMtxLst_%d.save",lastH)      )   # hMtxLst
    load( sprintf("./save/finalCut/Obj_cut_hMtxLst_bS_%d.save",lastH)   )   # hMtxLst_bS
    load( sprintf("./save/finalCut/Obj_cut_hMtxLst_bN_%d.save",lastH)   )   # hMtxLst_bN
    load( sprintf("./save/finalCut/Obj_cut_hMtxLst_HCR_%d.save",lastH)  )   # hMtxLst_HCR
} else {
    crScrHTool$addData( lastH - 5:0 )
    hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=lastH, tgt.scMtx )
    hMtxLst_bS <- bS.makeHMtxLst( gEnv, allIdxLst, fRstLst ,tgt.scMtx )
    hMtxLst_bN <- bN.makeHMtxLst( gEnv, allIdxLst, fRstLst ,tgt.scMtx )
    hMtxLst_HCR <- HCR.makeHCRMtxLst( crScrH ,allIdxLst ,fRstLst ,lastH=NULL ,tgt.scMtx=tgt.scMtx )
    save( hMtxLst       ,file=sprintf("./save/finalCut/Obj_cut_hMtxLst_%d.save",lastH)      )
    save( hMtxLst_bS    ,file=sprintf("./save/finalCut/Obj_cut_hMtxLst_bS_%d.save",lastH)   )
    save( hMtxLst_bN    ,file=sprintf("./save/finalCut/Obj_cut_hMtxLst_bN_%d.save",lastH)   )
    save( hMtxLst_HCR   ,file=sprintf("./save/finalCut/Obj_cut_hMtxLst_HCR_%d.save",lastH)  )
}
cut.grp <- bFCust.getFCustGrp( hMtxLst ,tgt.scMtx )
filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
sfExport("tgt.scMtx")   ;sfExport("hMtxLst")    ;sfExport("cut.grp")    ;sfExport("filter.grp")

tDiff <- Sys.time() - tStmp
sprintf("hMtxLs,cut.grp    Time cost : %.1f%s",tDiff,units(tDiff))  # 17 min, 59GB RAM

# ----------------------------------------------------------------------------------

for( sfcIdx in 0 ){ # 0:2

    cutH.reportScoreHistory( hMtxLst ,rmEmptyCol=TRUE )
    # cutH.reportScoreHistory_bS( hMtxLst_bS ,rmEmptyCol=TRUE )

    saveMidResult <- FALSE
    aZoidGrpName <- sprintf("allZoid.idx%d",sfcIdx)
    fHName <- bUtil.getSfcLstName( fRstLst[[length(fRstLst)]] ,curStdFiltedCnt=sfcIdx ,cut.grp )

    logger <- k.getFlogObj( sprintf("./log/FinalCut_%d_%s.txt",lastH,aZoidGrpName) )
    logger$fLogStr(sprintf("start %s",aZoidGrpName),pAppend=F,pTime=T)

    allIdxF <- allIdxLst[[aZoidGrpName]]
    if( testMode ){
        allIdxF <- allIdxF[sample(1:length(allIdxF),1000)]
    }
    rptStr <- sprintf( "Initial allIdxF size : %dk" ,length(allIdxF) %/% 1000 )
    prllLog$fLogStr( rptStr, pTime=T)   ;rptStr

    # save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_cutInit.save",sfcIdx) )
    # logger$fLogStr(sprintf("Initial size :%7d",length(allIdxF)),pTime=T)

    #   primary cut --------------------------------------------------------------------
    # allIdxF <- FC.primaryCut.static( allIdxF ,gEnv )
    # allIdxF <- FC.primaryCut.cust( allIdxF ,gEnv )
    # logger$fLogStr(sprintf("FC.primaryCut :%7d",length(allIdxF)),pTime=T)

    # bUtil.cut1( byMethod 1st ) --------------------------------------------------------
    #   
    surFlag <- rep( T ,length(allIdxF) )
    bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,5,400) )
    prllLog$fLogStr( sprintf("start cut0 for group%d 1st. bLst size %d",sfcIdx,length(bLst)), pTime=T)

    sfExport("fHName")  ;sfExport("allIdxF")    ;sfExport("cutH.InitialCut")
    resultLst <- sfLapply( bLst ,function( blk ){

        tStmp <- Sys.time()

        timeCost <- c( "score1"=26 ,"score2"=23 ,"score3"=19 ,"score4"=59 ,"score5"=94 ,"score6"=54 ,"score7"=61 ,"score8"=21 ,"score9"=30 )
        timeCost <- c( timeCost ,"bScr01"=1 ,"bScr02"=1 )
        timeCost <- c( timeCost ,"scoreA"=29 ,"scoreB"=24 ,"scoreC"=24 ,"scoreD"=26 )

        cutRst <- cutH.InitialCut( gEnv ,allIdxF ,blk ,filter.grp ,cut.grp ,timeCost=timeCost ,fHName=fHName ,logger=NULL )

        tDiff <- Sys.time() - tStmp

        logStr <- sprintf("  block finished for bUtil.cut0( byMethod Only 1st ). %d/%d  %5.1f%s for %d~%d "
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
    logger$fLogStr(sprintf("   - bUtil.cut1( byMethod 1st )   final size :%7d",length(allIdxF)),pTime=T)
    if( saveMidResult ) save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_cut0_byM1st_%d.save",sfcIdx,lastH) )
    rptStr <- sprintf( "allIdxF size : %dk" ,length(allIdxF) %/% 1000 )
    prllLog$fLogStr( rptStr, pTime=T)   ;rptStr

    # bUtil.cut1( byMethod 2st ) --------------------------------------------------------
    #   
    surFlag <- rep( T ,length(allIdxF) )
    bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,5,50) )
    prllLog$fLogStr( sprintf("start cut0 for group%d 2nd. bLst size %d",sfcIdx,length(bLst)), pTime=T)

    sfExport("fHName")  ;sfExport("allIdxF")    ;sfExport("cutH.InitialCut")
    resultLst <- sfLapply( bLst ,function( blk ){

        tStmp <- Sys.time()

        timeCost <- c( "scoreE"=20 ,"scoreF"=3 )  # 측정필요.
        timeCost <- c( timeCost ,"scoreLAr13"=70 ,"scoreLAr24"=70 ,"scoreLVr13"=70 ,"scoreLVr24"=70 )
        timeCost <- c( timeCost ,"scoreLAe13"=70 ,"scoreLAe24"=70 ,"scoreLVe13"=70 ,"scoreLVe24"=70 )
        timeCost <- c( timeCost ,"scoreLAc13"=70 ,"scoreLAc24"=70 ,"scoreLVc13"=70 ,"scoreLVc24"=70 )
        timeCost <- c( timeCost ,"scoreLAf13"=70 ,"scoreLAf24"=70 ,"scoreLVf13"=70 ,"scoreLVf24"=70 )
        timeCost <- c( timeCost ,"scoreFV"=80 )

        cutRst <- cutH.InitialCut( gEnv ,allIdxF ,blk ,filter.grp ,cut.grp ,timeCost=timeCost ,fHName=fHName ,logger=NULL )

        tDiff <- Sys.time() - tStmp

        logStr <- sprintf("  block finished for bUtil.cut0( byMethod Only 2nd ). %d/%d  %5.1f%s for %d~%d "
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
    logger$fLogStr(sprintf("   - bUtil.cut1( byMethod 2st )   final size :%7d",length(allIdxF)),pTime=T)
    if( saveMidResult ) save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_cut0_byM2nd_%d.save",sfcIdx,lastH) )
    rptStr <- sprintf( "allIdxF size : %dk" ,length(allIdxF) %/% 1000 )
    prllLog$fLogStr( rptStr, pTime=T)   ;rptStr

    # bC.cut() ----------------------------------------------------------------------
    #   ClM 효과가 좋아 먼저 시행.
    for( crMName in names(bCMtxCfg) ){  # bUtil.cut2() 대체
        surFlag <- rep( T ,length(allIdxF) )
        bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,5,20) )
        prllLog$fLogStr( sprintf("start bC.cut(%s) for group%d. bLst size %d",crMName,sfcIdx,length(bLst)), pTime=T)

        sfExport("crMName") ;sfExport("allIdxF")    ;sfExport("fHName")    #    ;sfExport("cutH.bC.Cut")
        resultLst <- sfLapply( bLst ,function( blk ){
            tStmp <- Sys.time()

            blkSpan <- blk["start"]:blk["end"]
            scoreMtx.grp <- getScoreMtx.grp( gEnv$allZoidMtx[allIdxF[blkSpan],,drop=F] ,filter.grp ,tgt.scMtx=tgt.scMtx )

            crCutRst <- bC.cut( crMName ,scoreMtx.grp ,cut.grp ,fHName ,anaOnly=F )

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
        if( saveMidResult ) save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_bCCut_%s_%d.save",sfcIdx,crMName,lastH) )
        rptStr <- sprintf( "allIdxF size : %dk" ,length(allIdxF) %/% 1000 )
        prllLog$fLogStr( rptStr, pTime=T)   ;rptStr

    }   # sprintf("./save/cutResult/Obj_allIdxF%d_bCCut_%s_%d.save",sfcIdx,crMName,lastH)

    # bUtil.cut1( mfName ) ----------------------------------------------------------------------
    for( mfName in names(bFMtxMFltLst) ){
        surFlag <- rep( T ,length(allIdxF) )
        bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,5,20) )
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
        if( saveMidResult ) save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_cut0_%s_%d.save",sfcIdx,mfName,lastH) )
        rptStr <- sprintf( "allIdxF size : %dk" ,length(allIdxF) %/% 1000 )
        prllLog$fLogStr( rptStr, pTime=T)   ;rptStr
    }

    # bUtil.chkStdMIPair() ------------------------------------------------------------------
    if( TRUE ){
        surFlag <- rep( T ,length(allIdxF) )
        bLst <- k.blockLst( length(allIdxF) ,100*ifelse(testMode,5,20) )
        prllLog$fLogStr( sprintf("start bUtil.chkStdMIPair(%s) for group%d. bLst size %d",crMName,sfcIdx,length(bLst)), pTime=T)

        sfExport("gEnv")    ;sfExport("allIdxF")
        resultLst <- sfLapply( bLst ,function( blk ){
            tStmp <- Sys.time()

            blkSpan <- blk["start"]:blk["end"]
            pairRebLst <- bUtil.chkStdMIPair( gEnv ,aZoidMtx=gEnv$allZoidMtx[allIdxF[blkSpan],,drop=F] )

            cName <- c("max","cnt")
            cutMtx <- matrix( 0, nrow=length(pairRebLst) ,ncol=length(cName) 
                        ,dimnames=list( sprintf("a%d",allIdxF[blkSpan]) ,cName )
            )
            for( idx in seq_len(length(pairRebLst)) ){
                pairReb <- pairRebLst[[idx]]
                if( 0== length(pairReb) ) next


                fndPairLen <- sapply( pairReb ,function(p){ length(p$fndPair) })
                if( 0<length(fndPairLen) ){
                    cutMtx[idx,"cnt"] <- length(fndPairLen) 
                }
                if( 1<max(fndPairLen) ){
                    cutMtx[idx,"max"] <- max(fndPairLen)
                }
            }

            surFlag <- apply( cutMtx ,1 ,function(p){all(p==0)})
            tDiff <- Sys.time() - tStmp
            logStr <- sprintf("  block finished for bUtil.chkStdMIPair(). %d/%d  %5.1f%s for %d~%d "
                                ,sum(!surFlag)   ,length(surFlag)
                                ,tDiff      ,units(tDiff)           ,blk["start"] ,blk["end"]
            )
            prllLog$fLogStr( logStr )

            return( list( surFlag=surFlag ,blk=blk ) )
        })
        for( idx in seq_len(length(resultLst)) ){
            blk <- resultLst[[idx]]$blk
            surFlag[ blk["start"]:blk["end"] ] <- resultLst[[idx]]$surFlag
        }
        allIdxF <- allIdxF[surFlag]
        logger$fLogStr(sprintf("   - bUtil.chkStdMIPair()   survival size :%7d",length(allIdxF)),pTime=T)
        if( saveMidResult ) save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_chkStdMIPair_%d.save",sfcIdx,lastH) )
        rptStr <- sprintf( "chkStdMIPair - allIdxF size : %d" ,length(allIdxF) )
        prllLog$fLogStr( rptStr, pTime=T)   ;rptStr
    }   # sprintf("./save/cutResult/Obj_allIdxF%d_chkStdMIPair_%d.save",sfcIdx,lastH)


    # bS.cut() ---<2 hours>---------------------------------------------------------------
    if( TRUE ){
        # cutH.bS.Cut() ------------------------------------------------------------------
        allIdxF <- cutH.bS.Cut( gEnv ,allIdxF ,hMtxLst_bS ,fHName ,tgt.scMtx=tgt.scMtx ,prllLog )

        prllLog$fLogStr(    sprintf("   - cutH.bS.Cut( )   final survival size :%7d",length(allIdxF))
                            ,pTime=T
        )
        if( saveMidResult ) save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_bScut_%d.save",sfcIdx,lastH) )
    }

    # bN.cut() ---<N hours>---------------------------------------------------------------
    if( FALSE ){
        # 개발 중...
        allIdxF <- cutH.bN.Cut( gEnv ,allIdxF ,hMtxLst_bN ,fHName ,tgt.scMtx=NULL ,prllLog )
        prllLog$fLogStr(    sprintf("   - cutH.bS.Cut( )   final survival size :%7d",length(allIdxF))
                            ,pTime=T
        )
        if( saveMidResult ) save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_bScut_%d.save",sfcIdx,lastH) )
    }

    # HCR.cut() ------------------------------------------------------------------
    if( FALSE ){

        aZoidMtx <- gEnv$allZoidMtx[allIdxF,,drop=F]
        scoreMtx.grp <- getScoreMtx.grp( aZoidMtx ,filter.grp ,makeInfoStr=T )
        std.grp <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

        phVP.grp <- bS.getPhVPGrp( gEnv ,aZoidMtx )
        scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx=tgt.scMtx )
        cut.grp <- bS.getCutGrp( hMtxLst_bS ,tgt.scMtx )
        bS.grp <- bS.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

        crScrA <- list( stdIdx=allIdxF ,std.grp=std.grp$aLst ,bS.grp=bS.grp$aLst )  # crScr of aZoid
        filterLst <- HCR.getFilter.grp( tgt.scMtx ,crScrH )
        scoreMtx.grp <- HCR.getScoreMtx.grp( crScrA ,filterLst ,tgt.scMtx=tgt.scMtx )
        cut.grp <- HCR.getCutterGrp( hMtxLst_HCR ,fHName ,tgt.scMtx )
        cutRst1 <- HCR.cut1( scoreMtx.grp ,cut.grp ,anaOnly=F ) 
        allIdxF <- allIdxF[cutRst1$surFlag]

        prllLog$fLogStr(    sprintf("   - cutH.HCR.Cut( )   final survival size :%7d",length(allIdxF))
                            ,pTime=T
        )
        if( saveMidResult ) save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_HCRcut_%d.save",sfcIdx,lastH) )
    }

    # less Effective cutters -----------------------------------------------------
    # allIdxF <- curH.LECut_bFMtx( gEnv ,allIdxF ,hMtxLst ,fHName ,tgt.scMtx ,EMN ,pllLLog )
    # allIdxF <- curH.LECut_bSMtx( gEnv ,allIdxF ,hMtxLst ,fHName ,tgt.scMtx ,EMN ,pllLLog )


    rptStr <- sprintf( "allIdxF size : %d" ,length(allIdxF) )
    prllLog$fLogStr( rptStr, pTime=T)   ;rptStr
    finalSaveFile <- sprintf("./save/finalCut/Obj_allIdxF_%d_grp%d.save",lastH,sfcIdx)
    save( allIdxF ,file=finalSaveFile )             ;cat(sprintf("saved in %s\n",finalSaveFile))

    logFileName <- sprintf("allIdxF_%d_grp%d.txt",lastH,sfcIdx)
    cutH.logAllIdxF( allIdxF ,gEnv ,logFileName=logFileName )

    # -- End of Cut ---------------------------------------------------------------------
    # ===================================================================================


    # ===================================================================================
    # -- Inspection on survivor ---------------------------------------------------------
    load( sprintf("./save/finalCut/Obj_allIdxF_%d_grp%d.save",lastH,sfcIdx) )
    cutH.reportSurvivor( sfcIdx ,aZoidMtx=gEnv$allZoidMtx[allIdxF,,drop=F] )

    CRpt.cutRst1Score( gEnv$allZoidMtx[sort(allIdxF),,drop=F] ,filter.grp ,cut.grp ,fHName )

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
    save( allIdxF ,file=sprintf("./save/cutResult/Obj_allIdxF%d_cut2_%d.save",sfcIdx,lastH) )



}   # sfcIdx

