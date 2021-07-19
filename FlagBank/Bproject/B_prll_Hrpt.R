Bprll_lastMtxBF <- function( cutInfo ,scoreMtx.grp ,hMtxLst ,hNameAll=F ,fLogger ,pAppend=F ,pRotate=F ,asEvt=F ){
    # hMtxLst <- curHMtxLst
    # cutInfo <- cutRst1$cutInfoLst[[1]]
    # hNameAll=F ;pAppend=F ;pRotate=F ;asEvt=F
    fLogger$fLogStr("start",pTime=T,pAppend=pAppend)
    if( asEvt ){
        fLogger$fLogStr("  *** All is reported as Evt !!")
    }

    print( fLogger$fileName )
    cfg <- scoreMtxCfg[[ cutInfo["mName"] ]]

    fLogger$fLog( cutInfo )
    mtxGrp <- getScoreMtx.grp_byHIdx( scoreMtx.grp )
    mtx <- mtxGrp[[cutInfo["mName"]]][[1]]
    if( asEvt ){
        mtx <- bFCust.getEvtMtx( mtx ,cfg )$eLevMtx
    }
    colnames(mtx) <- bUtil.getShortPhaseName( colnames(mtx) )
    fLogger$fLogMtx( mtx ,pClearNA=T )

    for( hName in names(hMtxLst$scoreMtxLst) ){
        if( !hNameAll && (hName!=cutInfo["hName"]) )     next

        ptrHName <- if( hName==cutInfo["hName"] ) sprintf( "<*%s>",hName )  else sprintf( "<%s>",hName )
        fLogger$fLogStr( ptrHName )
        mtx <- hMtxLst$scoreMtx[[ hName ]]$basic[[cutInfo["mName"]]]$scoreMtx
        if( asEvt ){
            eLevMtx <- bFCust.getEvtMtx( t(mtx) ,cfg )$eLevMtx
            mtx <- t(eLevMtx)
        }

        if( pRotate )   mtx <- t(mtx)

        fLogger$fLogMtx( mtx ,pClearNA=T )
    }

}

Bprll_lastMtxHCR <- function( cutInfo ,scoreMtx.grp ,hMtxLst_HCR ,hNameAll=F ,fLogger ,pAppend=F ){

    fLogger$fLogStr("start",pTime=T,pAppend=pAppend)

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

    pairHpnLst <- bUtil.rptRowReb_mtxLst( mtxLst ,pairSizes=2:ncol(mtxLst[[1]]) ,fLogger ,dbgInfo=dbgInfo ,opt=opt )
    return( pairHpnLst )
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

    pairHpnLst_byH <- list()
    for( curHIdx in testSpan ){

        curHMtxLst <- testData.grp$curHMtxLst.grp[[as.character(curHIdx)]]
        hMtxLst_HCR <- testData_HCR.grp$curHMtxLst_HCR.grp[[as.character(curHIdx)]]

        if( "HCR"==mNameType ){
            if( !oneLog ){
                rptLog <- k.getFlogObj( sprintf("%s_%s%d.txt",logFileName,mName,curHIdx) )
                print( sprintf("    writing %s",rptLog$fileName) )
            }
            pairHpnLst <- Bprll_inspecHMtxLst_HCR( hMtxLst_HCR ,mName ,fLogger=rptLog 
                            ,auxMsg=sprintf("curHIdx %d",curHIdx) ,opt=c(rebSeqAll=T,rebSeqOnly=T) ,dbgInfo=T ,pAppend=oneLog
            )
            pairHpnLst_byH[[as.character(curHIdx)]] <- pairHpnLst
        }

    }   # curHIdx

    return( pairHpnLst_byH )
}

Bprll_inspec_lastH_smry <- function( pairHpnLst_byH ,fLogger ){
    # pairHpnLst_byH <- Bprll_inspec_lastH( lastH ,mName="HCRsz_bf01fCol" ,mNameTyp="HCR" ,oneLog=T )

    getRebSeqOnly <- function( pairHpnLst_byH ,pColIdx ,pVpIdx ){
        #   pColIdx="1_2" ;pVpIdx="2/1"
        #   rptObj <- getRebSeqOnly( pairHpnLst_byH ,pColIdx="1_2" ,pVpIdx="2/1" )
        #
        #                     hIdx  sizeIdx colIdx vpIdx dbgStr                          
        #                 [1,] "894" "2"     "1_2"  "2/1" "sfcLate:1, sfc2:1, NGAP000.E:1"
        #                     rIdx seqNum
        #                 [1,]   19      1
        #                 [2,]   20      1
        #
        # curHIdx <- "899"        ;hName<-"sfcLate"
        # hMtxLst_HCR <- testData_HCR.grp$curHMtxLst_HCR.grp[[curHIdx]]
        # hMtxLst_HCR$scoreMtxLst[[hName]]$basic[["HCRsz_bf01fCol"]]

        tDf <- data.frame( hIdx=character(0) ,sizeIdx=character(0) )

        cName <- c("hIdx","sizeIdx","colIdx","vpIdx","dbgStr")
        mtx <- matrix( "", nrow=0 ,ncol=length(cName) ,dimnames=list(NULL,cName) )
        rebSeqOnlyLst <- list()

        for( hIdx in names(pairHpnLst_byH) ){
            for( sizeIdx in names(pairHpnLst_byH[[hIdx]]) ){
                for( colIdx in names(pairHpnLst_byH[[hIdx]][[sizeIdx]]) ){
                    if( pColIdx!=colIdx ) next

                    for( vpIdx in names(pairHpnLst_byH[[hIdx]][[sizeIdx]][[colIdx]]$vpLst) ){
                        if( pVpIdx!=vpIdx ) next

                        vpInfo <- pairHpnLst_byH[[hIdx]][[sizeIdx]][[colIdx]]$vpLst[[vpIdx]]
                        if( 0==nrow(vpInfo$rebSeqOnly) )    next

                        if( !is.null(vpInfo) ){
                            # print(sprintf("%s %s %s %s",hIdx,sizeIdx,colIdx,vpIdx))
                            dbgStr <- paste( vpInfo$dbgStrLst$rebSeqOnly ,collapse=", ")
                            mtx <- rbind( mtx ,c(hIdx,sizeIdx,colIdx,vpIdx,dbgStr) )

                            rebSeqOnlyLst[[1+length(rebSeqOnlyLst)]] <- vpInfo$rebSeqOnly
                        }
                    }
                }
            }
        }

        return( list(mtx=mtx,rebSeqOnlyLst=rebSeqOnlyLst) )
    }

    print( fLogger$fileName )
    fLogger$fLogStr("start",pTime=T,pAppend=F)

    aPairHpnLst <- NULL
    for( hIdx in names(pairHpnLst_byH) ){
        pairHpnLst <- pairHpnLst_byH[[hIdx]]
        if( is.null(aPairHpnLst) ){
            aPairHpnLst <- pairHpnLst
            next
        }

        for( pairIdx in names(aPairHpnLst) ){
            for( colIdx in names(aPairHpnLst[[pairIdx]]) ){
                for( vpIdx in names(pairHpnLst[[pairIdx]][[colIdx]]$vpLst) ){

                    vpInfo <- pairHpnLst[[pairIdx]][[colIdx]]$vpLst[[vpIdx]]

                    if( is.null(aPairHpnLst[[pairIdx]][[colIdx]]$vpLst[[vpIdx]]) ){
                        aPairHpnLst[[pairIdx]][[colIdx]]$vpLst[[vpIdx]] <- vpInfo
                    } else {
                        aVpInfo <- aPairHpnLst[[pairIdx]][[colIdx]]$vpLst[[vpIdx]]
                        aVpInfo$hpnCnt <- aVpInfo$hpnCnt + vpInfo$hpnCnt
                        aVpInfo$rebSeqAll <- rbind( aVpInfo$rebSeqAll ,vpInfo$rebSeqAll )
                        aVpInfo$rebSeqOnly <- rbind( aVpInfo$rebSeqOnly ,vpInfo$rebSeqOnly )

                        aPairHpnLst[[pairIdx]][[colIdx]]$vpLst[[vpIdx]] <- aVpInfo
                    }
                }
            }
        }
    }

    # Data reorg
    for( pairIdx in names(aPairHpnLst) ){
        for( colIdx in names(aPairHpnLst[[pairIdx]]) ){
            for( vpIdx in names(aPairHpnLst[[pairIdx]][[colIdx]]$vpLst) ){
                vpInfo <- aPairHpnLst[[pairIdx]][[colIdx]]$vpLst[[vpIdx]]
                vpRpt <- vpInfo[c("val","hpnCnt")]
                vpRpt$rebSeqAll     <- if( 0==nrow(vpInfo$rebSeqAll) ) NULL else table(vpInfo$rebSeqAll[,"seqNum"])
                vpRpt$rebSeqOnly    <- if( 0==nrow(vpInfo$rebSeqOnly) ) NULL else table(vpInfo$rebSeqOnly[,"seqNum"])

                aPairHpnLst[[pairIdx]][[colIdx]]$vpLst[[vpIdx]] <- vpRpt
            }
        }

        if( 0==length(aPairHpnLst[[pairIdx]]) ){
            aPairHpnLst[[pairIdx]] <- NULL
        }
    }

    for( pairIdx in names(aPairHpnLst) ){
        for( colIdx in names(aPairHpnLst[[pairIdx]]) ){
            if( 0==length(aPairHpnLst[[pairIdx]][[colIdx]]$vpLst) ) next

            fLogger$fLogStr(sprintf("colIdx : %s",colIdx))
            for( vpIdx in names(aPairHpnLst[[pairIdx]][[colIdx]]$vpLst) ){
                vpRpt <- aPairHpnLst[[pairIdx]][[colIdx]]$vpLst[[vpIdx]]
                fLogger$fLogStr(sprintf( "<%s> %s    hpn:%d" ,vpIdx ,paste( sprintf("%s(%s)",names(vpRpt$val),vpRpt$val) ,collapse="-" ) ,vpRpt$hpnCnt ))
                if( !is.null(vpRpt$rebSeqAll) ){
                    tblStr <- paste( sprintf("%s(%d)",names(vpRpt$rebSeqAll),vpRpt$rebSeqAll) ,collapse="  " )
                    fLogger$fLogStr(sprintf("    seq(all) : %s   #  len(hpn)",tblStr))
                }
                if( !is.null(vpRpt$rebSeqOnly) ){
                    tblStr <- paste( sprintf("%s(%d)",names(vpRpt$rebSeqOnly),vpRpt$rebSeqOnly) ,collapse="  " )
                    fLogger$fLogStr(sprintf("    seq(only) : %s   #  len(hpn)",tblStr))
                }
            }
            fLogger$fLogStr("\n")
        }
    }

    if( FALSE ){    # 개발 테스트 코드
        pairIdx<-names(aPairHpnLst)[1]  ;colIdx<-names(aPairHpnLst[[pairIdx]])[1]   ;vpIdx<-names(aPairHpnLst[[pairIdx]][[colIdx]]$vpLst)[1]
        #   fLogger$fLogStr("start",pTime=T,pAppend=F)

    }

}
