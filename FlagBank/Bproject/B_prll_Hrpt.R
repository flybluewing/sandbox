

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
        for( hName in hNameSet ){
            for( mName in mNameSet ){
                mtxLst  <- hIdxObj[[hName]][[mName]]
                cfg     <- scoreMtxCfg[[mName]]
                szObj <- bFCust.getSkipZero_byHIdx( mtxLst ,cfg )
                
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
        }

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

}
