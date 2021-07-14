

Bprll_lastMtxHCR <- function( cutInfo ,scoreMtx.grp ,hMtxLst_HCR ,hNameAll=F ,fLogger ){

    fLogger$fLog( cutInfo )
    mtx <- scoreMtx.grp$basic[[ cutInfo["mName"] ]]     ;rownames(mtx) <- "scoreMtx.grp"
    fLogger$fLogMtx( mtx )

    for( hName in names(hMtxLst_HCR$scoreMtxLst) ){
        if( !hNameAll && (hName!=cutInfo["hName"]) )     next

        ptrHName <- if( hName==cutInfo["hName"] ) sprintf( "<*%s>",hName )  else sprintf( "<%s>",hName )
        fLogger$fLogStr( ptrHName )
        mtx <- hMtxLst_HCR$scoreMtx[[ hName ]]$basic[[cutInfo["mName"]]]
        fLogger$fLogMtx( mtx )
    }

}

Bprll_inspecSZ_std <- function( lastH ,curHIdxSet ,fLogger ,gEnv,allIdxLst,fRstLst,crScrH,hMtxLstBig ){
    #   crScrHTool$addData( ) 참고
    if( FALSE ){    # 빈번작업 코드
        fLogger$fLogStr("",pAppend=F,pTime=T)
    }

    searchKeyStr <- character(0)
    szMtxLst_H <- list()
    for( curHIdx in curHIdxSet ){   # curHIdx <- 891

        fLogger$fLogStr( sprintf("curHIdx : %d",curHIdx) )

        std.grp <- crScrH$std.grp[[as.character(curHIdx)]]
        hNameSet <- names(std.grp)
        mNameSet <- names(std.grp[[ hNameSet[1] ]]$basic)
        for( hName in hNameSet ){
            for( mName in mNameSet ){
                summ <- std.grp[[ hName ]]$basic[[ mName ]]$summ
                keyStr <- sprintf("crScrH$std.grp[[%s]]$%s$basic$%s",curHIdx,hName,mName)
                fLogger$fLogStr( keyStr )   ;searchKeyStr <- c(searchKeyStr ,keyStr)
                fLogger$fLogMtx( summ$scMtx.sz ,pIndent="  " )
            }
        }

        wLastH <- curHIdx-1 ;wLastSpan <- 1:which(names(fRstLst)==wLastH)
        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
        allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]  ;allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
        fRstLst.w <- fRstLst[wLastSpan]

        # [hMtxLstBig 으로 대체 요]----------------
        stdIdx <- hMtxLstBig[[as.character(curHIdx)]]$stdIdx
        curHMtxLst <- hMtxLstBig[[as.character(curHIdx)]]$curHMtxLst
        # stdIdx <- testData.grp$stdIdx[[as.character(curHIdx)]]
        # curHMtxLst <- testData.grp$curHMtxLst.grp[[as.character(curHIdx)]]
        # ----------------

        stdZoid <- gEnv$zhF[curHIdx,]   ;
        stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst.w )
        filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
        scoreMtx.grp <- getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,makeInfoStr=F )    # makeInfoStr=T
        curStdFilted <- fRstLst[[as.character(curHIdx)]]    #   평가가 아닌 실제에선, remLst 으로부터 가져올 것.

        # scoreMtx      ;fLogger$fLogStr("",pAppend=F,pTime=T)
        fLogger$fLogStr( sprintf("curHIdx:%d/scoreMtx.grp",curHIdx) )
        mNameSet <- names(scoreMtx.grp$basic[[1]])
        mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
        for( mName in mNameSet ){
            scoreMtx <- mtxGrp[[mName]][[1]]
            colnames(scoreMtx) <- bUtil.getShortPhaseName( colnames(scoreMtx) )

            keyStr <- sprintf("curHIdx:%d/scoreMtx.grp$%s",curHIdx,mName)
            fLogger$fLogStr( keyStr )   ;searchKeyStr <- c(searchKeyStr ,keyStr)
            fLogger$fLogMtx( scoreMtx ,pIndent="  " )
        }


        hIdxObj <- B.getHMtxLst_byHIdx( curHMtxLst )    # hIdx <- names(hIdxObj[[hName]][[mName]])
        # Sz : hMtxLst
        #       참고 : FCust_stdCut.hIdx()
        fLogger$fLogStr( sprintf("curHIdx:%d/hMtxLst",curHIdx) )
        szMtxLst <- list()
        for( hName in hNameSet ){
            szLst <- list()
            for( mName in mNameSet ){
                mtxLst  <- hIdxObj[[hName]][[mName]]
                cfg     <- scoreMtxCfg[[mName]]
                szObj <- bFCust.getSkipZero_byHIdx( mtxLst ,cfg )
                szLst[[mName]] <- szObj
                
                keyStr <- sprintf("curHIdx:%d/szObj$ph$%s$%s",curHIdx,hName,mName)
                fLogger$fLogStr( keyStr )   ;searchKeyStr <- c(searchKeyStr ,keyStr)
                mtx <- szObj$raw$ph
                colnames(mtx) <- bUtil.getShortPhaseName( colnames(mtx) )
                fLogger$fLogMtx( mtx ,pIndent="  " )

                keyStr <- sprintf("curHIdx:%d/szObj$fCol$%s$%s",curHIdx,hName,mName)
                fLogger$fLogStr( keyStr )   ;searchKeyStr <- c(searchKeyStr ,keyStr)
                mtx <- szObj$raw$fCol
                colnames(mtx) <- bUtil.getShortPhaseName( colnames(mtx) )
                fLogger$fLogMtx( mtx ,pIndent="  " )

                # szObj$dblHpn
            }
            szMtxLst[[hName]] <- szLst
        }
        szMtxLst_H[[as.character(curHIdx)]] <- szMtxLst

        #   stdMI.grp
        for( pName in names(stdMI.grp$basic) ){
            stdMI <- stdMI.grp$basic[[pName]]$stdMI

            keyStr <- sprintf("curHIdx:%d/stdMI$%s",curHIdx,pName)
            fLogger$fLogStr( keyStr )   ;searchKeyStr <- c(searchKeyStr ,keyStr)

            dfStr <- capture.output( anaMtx(stdMI$rawTail,NULL) )
            dfStr <- paste( dfStr ,collapse="\n" )
            fLogger$fLogStr( sprintf("%s \n",dfStr) )
        }
    }

    fLogger$fLogStr("")
    fLogger$fLogStr( searchKeyStr )

    return( szMtxLst_H )
}

Bprll_inspecHMtxLst_HCR <- function( hMtxLst_HCR ,mName ,fLogger ,auxMsg="" ,opt=c(rebSeqAll=F,rebSeqOnly=T) ,dbgInfo=F ,pAppend=F ){
    fLogger$fLogStr( sprintf("inspecHMtxLst_HCR(mName:%s) %s",mName,auxMsg) ,pTime=T ,pAppend=pAppend )

    mtxLst <- list()
    for( hName in names(hMtxLst_HCR$scoreMtxLst) ){
        mtxLst[[hName]] <- hMtxLst_HCR$scoreMtxLst[[hName]]$basic[[mName]]
    }

    bUtil.rptRowReb_mtxLst( mtxLst ,pairSizes=2:ncol(mtxLst[[1]]) ,fLogger ,dbgInfo=dbgInfo ,opt=opt )
}

Bprll_inspec_lastH <- function( lastH ,mName ,mNameType ,oneLog=T ){

    logFileName <- "./report/tempRpt/rowReb"
    rptLog <- NULL
    if( oneLog ){
        rptLog <- k.getFlogObj( sprintf("%s_%s.txt",logFileName,mName) )
        rptLog$fLogStr( "OneLog start" ,pTime=T ,pAppend=F )
        print( sprintf("    writing %s",rptLog$fileName) )
    }

    load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
    load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
    names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
    load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))
    crScrH <- crScrHTool$getData( )     ;crScrH <- crScrHTool$bySpan(crScrH,lastH)

    tgt.scMtx <- mName
    configH <- lastH-20     ;testSpan <- (lastH - 19:0)
    sfc.InTest <- allIdxLst$stdFiltedCnt[as.character(testSpan)]
    testSpan <- testSpan[sfc.InTest %in% 0:2]

    load( sprintf("Obj_testData.grp.%d.%s.save",lastH,"all") )
    load( sprintf("Obj_testData_HCR.grp.%d.%s.save",lastH,"all") )

    for( curHIdx in testSpan ){

        curHMtxLst <- testData.grp$curHMtxLst.grp[[as.character(curHIdx)]]
        hMtxLst_HCR <- testData_HCR.grp$curHMtxLst_HCR.grp[[as.character(curHIdx)]]
        if( "HCR"==mNameType ){
            if( !oneLog ){
                rptLog <- k.getFlogObj( sprintf("%s_%s%d.txt",logFileName,mName,curHIdx) )
                print( sprintf("    writing %s",rptLog$fileName) )
            }
            rptRst <- Bprll_inspecHMtxLst_HCR( hMtxLst_HCR ,mName ,fLogger=rptLog 
                            ,auxMsg=sprintf("curHIdx %d",curHIdx) ,opt=c(rebSeqAll=T,rebSeqOnly=T) ,dbgInfo=T ,pAppend=oneLog
            )
        }

    }   # curHIdx

}


