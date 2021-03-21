Bprll.getTgtScMtx <- function( ){

    tgt.scMtx <- NULL
    tgt.scMtx <- c( tgt.scMtx  ,c("score1","score2","score3","score4","score5","score6","score7","score8","score9") )
    tgt.scMtx <- c( tgt.scMtx  ,c("bScr01","bScr02") )
    tgt.scMtx <- c( tgt.scMtx  ,c("scoreA","scoreB","scoreC","scoreD") )
    tgt.scMtx <- c( tgt.scMtx  ,c("scoreE","scoreF") )
    tgt.scMtx <- c( tgt.scMtx  ,c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("scoreLAc13","scoreLAc24","scoreLVc13","scoreLVc24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("scoreLAf13","scoreLAf24","scoreLVf13","scoreLVf24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("scoreFV") )
    tgt.scMtx <- c( tgt.scMtx  ,c("scoreGS","scoreGS3","scorePSh","scorePSrp","scorePSrpRaw") )

    #   tgt.scMtx <- c("score1")    ;tgt.scMtx <- c( tgt.scMtx  ,c("sScoreNew") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08","sScore09") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScore0LAr13","sScore0LAr24","sScore0LVr13","sScore0LVr24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScore0LAe13","sScore0LAe24","sScore0LVe13","sScore0LVe24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScore0LAc13","sScore0LAc24","sScore0LVc13","sScore0LVc24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScore0LAf13","sScore0LAf24","sScore0LVf13","sScore0LVf24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScore0GS","sScore0GS3","sScore0PSh","sScore0PSrp","sScore0PSrpRaw") )

    return( tgt.scMtx )
}


Bprll.bSCut <- function( gEnv.w ,stdZoid ,hMtxLst_bS ,fHName ,tgt.scMtx ){
    # stdIdx  ;stdZoid
    aZoidMtx <- matrix(stdZoid ,nrow=1)
    phVP.grp <- bS.getPhVPGrp( gEnv.w ,aZoidMtx )
    scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx=tgt.scMtx )

    cut.grp <- bS.getCutGrp( hMtxLst_bS ,tgt.scMtx )

    cutRst.bS <- bS.cut( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,anaOnly=T ) 
        #   anaOnly=TRUE 에서, cutRst$surFlag는 항상 TRUE임을 유의.

    for( crMName in names(bSMtxMCfg) ){  # bUtil.cut2() 대체
        crCutRst <- bS.cut_M( crMName ,scoreMtx.grp ,cut.grp ,fHName ,anaOnly=T )
        cutRst.bS$cutInfoLst <- append( cutRst.bS$cutInfoLst ,crCutRst$cutInfoLst )
    }


    cutRst1Score <- bS.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )
    return( list( cutRst=cutRst.bS ,cutRst1Score=cutRst1Score ) )
}


Bprll.resultLst <- function( resultLst ,tgt.scMtx ){

    mtx <- matrix( 0, nrow=length(resultLst) ,ncol=length(tgt.scMtx) 
            ,dimnames=list( names(resultLst) ,tgt.scMtx )
    )
    hpnCntLst <- list( rawHpn=mtx ,evtHpn=mtx )
    for( idx in 1:length(resultLst) ){
        auxObj <- resultLst[[idx]]$auxTest
        # zeroM <- resultLst[[idx]]$auxTest$zeroM

        cutRstScrSet <- auxObj$cutRstScrSet[["sfcLate"]]
        cntVal <- apply( cutRstScrSet$basic$hpnMtxEvt ,1 ,function(rVal){sum(rVal>0)})
        hpnCntLst[["evtHpn"]][idx,names(cntVal)] <- cntVal
        
        cntVal <- apply( cutRstScrSet$basic$hpnMtxRaw ,1 ,function(rVal){sum(rVal>0)})
        hpnCntLst[["rawHpn"]][idx,names(cntVal)] <- cntVal

        # mtx[ idx, intersect( colnames(mtx) ,zeroM ) ] <- 1
    }


    #   cutRstLst[[1]]$auxInfoLst$basic$score1
    #       "fColEvt"     "summMtx"     "summMtx.reb" "scMtx.sz"   
    names( resultLst ) <- sapply( resultLst ,function(p){p$hIdx})
    cutRstLst <- lapply( resultLst ,function(p){p$cutRst})
    names(cutRstLst) <- paste("H",testSpan,sep="")
    names(cutRstLst) <- paste( names(cutRstLst) ,allIdxLst$stdFiltedCnt[as.character(testSpan)] ,sep="_" )

    if( is.null(tgt.scMtx) ){
        tgt.scMtx <- names( cutRstLst[[1]]$auxInfoLst$basic )
    }

    hSummMtx <- matrix( 0 ,nrow=length(cutRstLst) ,ncol=length(tgt.scMtx) )
    rownames(hSummMtx) <- names(cutRstLst)   ;colnames(hSummMtx) <- tgt.scMtx
    for( idx in 1:length(cutRstLst) ){
        for( mName in tgt.scMtx ){
            rawObj <- 
            summMtx <- cutRstLst[[idx]]$auxInfoLst$basic[[mName]]$summMtx
            #     all ph fCol phReb xyCnt.fCol xyCnt.phase
            # raw   0  0    0     0          0           0
            # evt   0  0    0     0          0           0

            hSummMtx[ idx, mName ] <- summMtx["raw","ph"]

        }
    }



}


Bprll.stdCutTest <- function( testData.grp ,tgt.scMtx ,testSpan ,exportObj=TRUE ){

    tStmp1 <- Sys.time()
    if( exportObj ){
        sfExport("testData.grp")    ;sfExport("tgt.scMtx")
    }

    prll.initHeader( )          ;source("FCust_configBasic.R")  ;source("FCust_configExt.R")
    prllLog$fLogStr("- bUtil.cut() ----------------------------",pTime=T)
    resultLst <- sfLapply( testSpan ,function( curHIdx ){
        wLastH <- curHIdx-1
        wLastSpan <- 1:which(names(fRstLst)==wLastH)

        # ------------------------------------------------------------------------
        # cut.grp : cutter grp 을 얻어내자.
        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
        allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                    allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
        fRstLst.w <- fRstLst[wLastSpan]

        curHMtxLst <- testData.grp$curHMtxLst.grp[[as.character(curHIdx)]]
            # B.makeHMtxLst() 의 lastH는 allIdxLst.w$stdFiltedCnt에 의존한다.
            # curHIdx-1 시점까지의 scoreMtx가 curHMtxLst에 담겨있다.

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
        #   scoreMtx.grp <- getScoreMtx.grp.4H( stdZoid ,filter.grp )
        scoreMtx.grp <- getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,makeInfoStr=T )
            #   평가용이므로 getScoreMtx.grp.4H() 가 사용됨.   .4H !

        cutRst <- list( surFlag=T ,cutInfoLst=list() ) # 여기서 cutRst$surFlag는 의미없다. anaOnly=T 이므로

        # customCutRst <- Fin.customCut( lastH=curHIdx ,gEnv=gEnv.w ,allIdxF=stdIdx ,stdMI.grp=stdMI.grp ,anaOnly=T )
        # cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,customCutRst$cutInfoLst[[1]] )

        cutRst1 <- bUtil.cut1( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx ,anaOnly=T ) 
            #   anaOnly=TRUE 에서, cutRst$surFlag는 항상 TRUE임을 유의.
        cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,cutRst1$cutInfoLst )

        for( crMName in names(bCMtxCfg) ){  # 멀티 cutRst1Score 에 대한 필터링.
            # bUtil.cut2()의 발전형.
            #   - 멀티 scoreMtx에 대한 기준이 아님. 멀티 scoreMtx에 대한 기준은 bFMtxMulti.R에서 처리.
            #   - bC.cut()에서는 sfcLate에 대해서만 처리. ( bFMtxMulti.R에서는 hName별 처리도 이루어짐. ) <- 수정.
            crCutRst <- bC.cut( crMName ,scoreMtx.grp ,cut.grp ,fHName ,anaOnly=T )
            cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,crCutRst$cutInfoLst )
        }


        #   bUtil.chkStdMIPair() 적용.
        pairRebLst <- bUtil.chkStdMIPair( gEnv.w ,aZoidMtx=matrix(stdZoid,nrow=1) )
        if( 0<length(pairRebLst[[1]]) ){
            fndPairLen <- sapply( pairRebLst[[1]] ,function(p){ length(p$fndPair) })

            cutInfoLst <- list()
            if( 0<length(fndPairLen) ){
                infoStr <- paste(names(fndPairLen),fndPairLen,sep=":")
                infoStr <- paste( infoStr ,collapse="," )
                cutInfo <- c( typ="lastRawPair",hName="N/A",mName="bUtil.chkStdMIPair(cnt)",pName="N/A",info=infoStr )
                cutInfoLst[[1+length(cutInfoLst)]] <- cutInfo
            }
            if( 1<max(fndPairLen) ){
                infoStr <- paste(names(fndPairLen),fndPairLen,sep=":")
                infoStr <- paste( infoStr ,collapse="," )
                cutInfo <- c( typ="lastRawPair",hName="N/A",mName="bUtil.chkStdMIPair(max)",pName="N/A",info=infoStr )
                cutInfoLst[[1+length(cutInfoLst)]] <- cutInfo
            }

            cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,cutInfoLst )
        }

        if( TRUE ){ # aux cut : stdFiltedCnt
            fRst <- fRstLst[[as.character(curHIdx)]]

            if( 0 < length(fRst) ){
                sfCnt <- sapply(fRstLst.w,length)
                fndIdx <- which( sfCnt==length(fRst) )
                if( 0<length(fndIdx) ){
                    lastFRst <- fRstLst.w[[ fndIdx[length(fndIdx)] ]]
                    if( all(lastFRst==fRst) ){
                        infoStr <- sprintf("stdFiltedCnt rebound!! %s",paste( fRst ,collapse=" ,"))
                        auxCutInfoLst <- list( auxCut=c( typ="aux_sfc" ,hName="N/A" ,mName="N/A" ,pName="N/A" ,info=infoStr ) )
                        cutRst$cutInfoLst <- append( auxCutInfoLst ,cutRst$cutInfoLst )
                    }
                }
            }
        }

        # bS cutting.
        hMtxLst_bS=testData.grp$curHMtxLst_bS.grp[[as.character(curHIdx)]]
        cutRst.bS <- Bprll.bSCut( gEnv.w=gEnv.w ,stdZoid ,hMtxLst_bS=hMtxLst_bS ,fHName=fHName ,tgt.scMtx )
        cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,cutRst.bS$cutRst$cutInfoLst )

        # End of Cut Test
        # report example =================================================
            # B.rptHMtxLst( curHMtxLst )
            # B.rptStdMI.grp( stdMI.grp )
            # B.rptScoreMtx.grp( scoreMtx.grp )
            # B.rptCut.grp( cut.grp )
            # B.rptCutRst( cutRst )

        prllLog$fLogStr(sprintf("    curHIdx:%d done.",curHIdx),pTime=T)
        if( FALSE ){ # debug info
            dbgFileName <- sprintf("Dbg_H%d.stdMI",curHIdx)

            B.rptStdMI.grp( stdMI.grp ,file=dbgFileName )

            log.c <- k.getFlogObj( sprintf("./report/workRpt/%s.txt",dbgFileName) )
            stdZoid.str <- sprintf("stdZoid : %s",paste( sprintf("%2d",stdZoid) ,collapse=" ") )
            log.c$fLogStr( stdZoid.str )

            scMtx <- getScoreMtx.grp_byHIdx( scoreMtx.grp )[[1]][[1]]
            colnames(scMtx) <- bUtil.getShortPhaseName( colnames(scMtx) )
            log.c$fLog( scMtx )

        }

        auxTest <- list()
        resultObj <- list( hIdx=curHIdx ,cutRst=cutRst)
        if( TRUE ){ # for later inspection...
            #     "hpnMtxRaw"      "hpnMtxEvt"      "phRebMtxRaw"    "phRebMtxEvt"    "rebMtxRaw"      "rebMtxEvt"      
            #     "summMtxRaw"     "summMtxEvt"     "summMtx.RebRaw" "summMtx.RebEvt" "szMtxCnt"       "szMtxDup"       
            #     "sumMtx" 

            resultObj$fHName <- fHName

            # resultObj$cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )
            # resultObj$cut.grp <- cut.grp            # bFMtx
            # resultObj$scoreMtx.grp <- scoreMtx.grp

            # resultObj$cutRst1Score_bS <- cutRst.bS$cutRst1Score
            # aZoidMtx <- matrix(stdZoid ,nrow=1)     # bSMTX
            # phVP.grp <- bS.getPhVPGrp( gEnv.w ,aZoidMtx )
            # scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx=tgt.scMtx )
            # cut.grp <- bS.getCutGrp( hMtxLst_bS ,tgt.scMtx )

            # resultObj$scoreMtx.grp <- scoreMtx.grp
            # resultObj$cut.grp <- cut.grp

        }

        return( resultObj )
            #   ,auxTest=auxTest
    })
    names( resultLst ) <- sapply( resultLst ,function(p){p$hIdx})
    cutRstLst <- lapply( resultLst ,function(p){p$cutRst})
    names(cutRstLst) <- paste("H",testSpan,sep="")
    names(cutRstLst) <- paste( names(cutRstLst) ,allIdxLst$stdFiltedCnt[as.character(testSpan)] ,sep="_" )

    if( is.null(resultLst) || 0==length(resultLst) ){
        cat("    Warning!! resultLst is empty. \n")
    }


    tDiff1 <- Sys.time() - tStmp1  ;tDiff1     # 2.1min / 7prllNum
    cat( sprintf("    resultLst %d (time cost : %.1f%s)  \n",length(resultLst),tDiff1,units(tDiff1)) )

    return( cutRstLst )
}


Bprll.buildTestData_CR <- function( hSpan ){    # working
    # hSpan <- 798:800  ;tgt.scMtx=c("score1","score2","sScore01","sScore02")

    lastH <- hSpan[length(hSpan)]
    load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
    load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )    ;names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
    load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))


    for( curHIdx in hSpan ){    # curHIdx <- hSpan[1]
        wLastH <- curHIdx-1
        wLastSpan <- 1:which(names(fRstLst)==wLastH)

        # ------------------------------------------------------------------------
        # cut.grp : cutter grp 을 얻어내자.
        gEnv.w <- gEnv              ;gEnv.w$zhF <- gEnv$zhF[1:wLastH,]
        allIdxLst.w <- allIdxLst    ;allIdxLst.w$stdFiltedCnt <- allIdxLst$stdFiltedCnt[wLastSpan]
                                    allIdxLst.w$infoMtx <- allIdxLst$infoMtx[wLastSpan,]
        fRstLst.w <- fRstLst[wLastSpan]
        stdZoid <- gEnv$zhF[curHIdx,]
        curStdFilted <- fRstLst[[as.character(curHIdx)]]    #   평가가 아닌 실제에선, remLst 으로부터 가져옴.


        hMtxLst <- B.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w, tgt.scMtx )
        stdMI.grp <- bUtil.getStdMILst( gEnv.w ,fRstLst.w )     ;stdMI.grp$anyWarn( )
        cut.grp <- bFCust.getFCustGrp( hMtxLst ,tgt.scMtx )
        fHName <- bUtil.getSfcLstName( fRstLst.w[[length(fRstLst.w)]] ,curStdFiltedCnt=length(curStdFilted) ,cut.grp )
        filter.grp <- getFilter.grp( stdMI.grp ,tgt.scMtx=tgt.scMtx )
        scoreMtx.grp <- getScoreMtx.grp( matrix(stdZoid,nrow=1) ,filter.grp ,makeInfoStr=F )
        cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )


        hMtxLst_bS <- bS.makeHMtxLst( gEnv.w, allIdxLst.w, fRstLst.w ,tgt.scMtx )
        aZoidMtx <- matrix(stdZoid ,nrow=1) # Bprll.bSCut() 참고
        phVP.grp <- bS.getPhVPGrp( gEnv.w ,aZoidMtx )
        scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx=tgt.scMtx )
        cut.grp <- bS.getCutGrp( hMtxLst_bS ,tgt.scMtx )
        cutRst1Score <- bS.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )

    }

}

if( FALSE ){ # text

    #   raw hpn zero
    #   그룹 : {score1,score3,score8} <- raw zero는 1이하, evt 1 발생은 2 이하.
    #     $rawHpn     score1 score2 score3 score4 score5 score6 score7 score8 score9 bScr01 bScr02 scoreA scoreB scoreC scoreD
    #             862      3     13      2      0      5      2      0      2     13      0      0      0      2      2      1
    #             863      3     11      1      0      6      2      0      3     12      0      0      0      2      3      1
    #             864      4     13      1      0      5      1      0      5     12      0      0      1      0      0      0
    #             865      5     13      3      0      6      1      0      4     13      0      0      0      1      2      0
    #             866      2     12      3      1      1      0      0      3     11      0      0      0      1      1      0
    #             867      5     12      2      0      6      0      0      1     11      0      0      0      0      0      0
    #             868      4     12      1      1      6      1      0      1     13      0      0      0      0      2      0
    #             869      2     13      5      2     10      2      0      6     11      0      0      0      0      1      1
    #             870      3     12      3      0      4      1      0      1     11      0      0      1      0      1      0
    #             873      1     12      1      0      3      2      0      4     13      0      0      0      0      0      1
    #             875      6     13      6      0      3      2      0      2     10      0      0      0      0      2      0
    #             876      4     13      2      1      2      1      0      5     10      0      0      1      3      3      1
    #             878      6     12      1      0      6      0      0      4     13      0      0      1      2      3      2
    #             879      7     12      0      0      6      3      0      4     11      0      0      2      1      1      3
    #             880      7     13      2      2      9      1      0      0     13      0      0      0      0      1      0
    #
    #     $evtHpn     score1 score2 score3 score4 score5 score6 score7 score8 score9 bScr01 bScr02 scoreA scoreB scoreC scoreD
    #             862      3     10      2      0      1      2      0      2      2      0      0      0      1      2      1
    #             863      3     10      1      0      0      1      0      3      6      0      0      0      2      1      1
    #             864      4     10      1      0      2      1      0      5      2      0      0      0      0      0      0
    #             865      5      8      3      0      2      1      0      4      4      0      0      0      1      1      0
    #             866      2      8      3      0      1      0      0      3      2      0      0      0      1      1      0
    #             867      5     10      2      0      0      0      0      1      0      0      0      0      0      0      0
    #             868      4     10      1      1      1      1      0      1      3      0      0      0      0      2      0
    #             869      2     13      5      2      4      2      0      6      3      0      0      0      0      0      1
    #             870      3      8      3      0      1      1      0      1      5      0      0      1      0      1      0
    #             873      1     11      1      0      0      2      0      4      3      0      0      0      0      0      1
    #             875      6      8      6      0      0      2      0      2      4      0      0      0      0      2      0
    #             876      4     12      2      1      1      1      0      5      4      0      0      0      3      0      1
    #             878      6     10      1      0      1      0      0      4      4      0      0      1      2      0      2
    #             879      7      6      0      0      1      3      0      4      2      0      0      0      1      1      2
    #             880      7     12      2      2      1      1      0      0      3      0      0      0      0      0      0

}


