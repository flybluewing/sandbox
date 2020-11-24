source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")
lastH <- 880

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))


#-[Parallel init work]-------------------------------------------------------------
prllNum <- 7     # 실수가 잦아서 그냥 오류 코드로 놔둔다.
prllLog <- k.getFlogObj( "./log/parallel_log_Hist.txt" )
prll.initHeader <- function( ){
    k <- sfLapply(1:prllNum,function(prllId){
        curWd <- getwd()
        setwd("..")
        source("hCommon.R")

        setwd( curWd )
        source("header.r")
        source("B_H.R")
        source("B_prll_H.R")
    })
}

sfInit( parallel=T, cpus=prllNum )
sfExport("prllLog") ;sfExport("lastH")
sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst")
prll.initHeader( )
prllLog$fLogStr("parallel init", pTime=T ,pAppend=F )
cat(sprintf("* Parallel ready... see log : %s \n",prllLog$fileName))



if( FALSE ){    # stdZoid에 대한 cutting 시뮬레이션 예제 코드

    tgt.scMtx <- NULL
    tgt.scMtx <- c( tgt.scMtx  ,Bprll.getTgtScMtx( ) )

    configH <- lastH-20    # configH는 기본 cutting값을 얻기 위하는 시점에 따라 조절.
    testSpan <- (lastH - 19:0)   # configH 보다는 큰 시점에서 시작해야 함을 유의.
    if( TRUE ){ # stdFiltedCnt 0~2내에서만 테스트
        sfc.InTest <- allIdxLst$stdFiltedCnt[as.character(testSpan)]
        testSpan <- testSpan[sfc.InTest %in% 0:2]
    }

    if( TRUE ){     # RM_B_prll.R
        load( sprintf("Obj_testData.grp.%d.%s.save",lastH,"all") )
    } else {
        tStmp <- Sys.time()
        # hMtxLst <- B.makeHMtxLst( gEnv, allIdxLst, fRstLst, lastH=configH, tgt.scMtx )
        testData.grp <- B.get_testData.grp( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx)
        save( testData.grp ,file=sprintf("Obj_testData.grp.%d.%s.save",lastH,ifelse(is.null(tgt.scMtx),"all",tgt.scMtx) ) )
        #   save( testData.grp ,file="Obj_testData.grp.save" )
        #   load( sprintf("Obj_testData.grp.%d.%s.save",lastH,ifelse(is.null(tgt.scMtx),"all",tgt.scMtx) ) )
        #           Obj_testData.grp.900.w100.save : configH <- lastH-100 (4hours)
        tDiff <- Sys.time() - tStmp
    }

    #   B.get_cutRst1.grp()                 curHIdx <- 862
    sfExport("testData.grp")    ;sfExport("tgt.scMtx")
    prll.initHeader( )          ;source("FCust_configBasic.R")  ;source("FCust_configExt.R")
    prllLog$fLogStr("- bUtil.cut() ----------------------------",pTime=T)   ;tStmp1 <- Sys.time()
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

            # resultObj$cutRst1Score <- bUtil.getCut1Score( scoreMtx.grp ,cut.grp ,fHName ,tgt.scMtx=tgt.scMtx )
            # resultObj$cutRst1Score_bS <- cutRst.bS$cutRst1Score

            resultObj$fHName <- fHName

            # resultObj$cut.grp <- cut.grp            # bFMtx
            # resultObj$scoreMtx.grp <- scoreMtx.grp

            aZoidMtx <- matrix(stdZoid ,nrow=1)     # bSMTX
            phVP.grp <- bS.getPhVPGrp( gEnv.w ,aZoidMtx )
            scoreMtx.grp <- bS.getScoreMtx.grp( phVP.grp ,aZoidMtx ,tgt.scMtx=tgt.scMtx )
            cut.grp <- bS.getCutGrp( hMtxLst_bS ,tgt.scMtx )

            resultObj$scoreMtx.grp <- scoreMtx.grp
            resultObj$cut.grp <- cut.grp

        }

        return( resultObj )
            #   ,auxTest=auxTest
    })  ;tDiff1 <- Sys.time() - tStmp1  ;tDiff1     # 2.1min / 7prllNum
    names( resultLst ) <- sapply( resultLst ,function(p){p$hIdx})
    cutRstLst <- lapply( resultLst ,function(p){p$cutRst})
    names(cutRstLst) <- paste("H",testSpan,sep="")
    names(cutRstLst) <- paste( names(cutRstLst) ,allIdxLst$stdFiltedCnt[as.character(testSpan)] ,sep="_" )

    # zeroTot
    # score1 score3 score4 score6 score7 score8 <-- score2, score5, score9
    #      3     10     37     32     57      4 

    # save( resultLst ,file=sprintf("./save/HMtxLst/Obj_resultLst%d.save",configH) )
    # save( cutRstLst ,file=sprintf("./save/HMtxLst/Obj_cutRstLst%d.save",configH) )

    rptFile <- ifelse(1==length(tgt.scMtx),sprintf("cutRstLst_%d",length(tgt.scMtx)),"cutRstLst")
    if( 1==length(tgt.scMtx) ){
        rptFile <- ifelse(1==length(tgt.scMtx),sprintf("cutRstLst_%s",tgt.scMtx),"cutRstLst")
        rptFile
    }

    rptBanTyp <- NULL   ;rptBanM <- NULL
    if( FALSE ){    # 참고 코드
        rptBanTyp <- c(   "rawFCol" ,"rowE" ,"rawReb"   ,"fCol"
                          ,"scMtx.sz.cut rebCnt" ,"scMtx.sz.cut rebCnt.e.sum"
                          ,"summMtx.cut"
                      )
        rptBanM <- c("score1")
    }
    rptBanTyp <- ("lastRawPair")
    rptBanM <- c( "score1" )
    rptBanM <- c( rptBanM  ,c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08","sScore09") )
    rptBanM <- c( rptBanM  ,c("bSMScr02R") )
    B.rptCutRstLst( cutRstLst ,file=rptFile ,rptBanTyp=rptBanTyp ,rptBanM=rptBanM )
    lastH   ;tgt.scMtx





    if( FALSE ){    # inspection --------------------------------------------------------------------
        B.rptCutRst1Score(      resultLst ,file=sprintf("CutRst1Score_%d",lastH)    )
        B.rptCutRst1Score_bS(   resultLst ,file=sprintf("CutRst1Score_%d_bS",lastH) )

        if( FALSE ){    # bFMtx
            mNameSet <- c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
            for( mName in mNameSet ){
                rptFile <- sprintf("Inspec_H%d_cutRst1Score",lastH)
                B.rptCutRst1Score_byMtx( resultLst ,mName ,rptFile)
            }
            mNameSet <- c("score1","score2","scoreLAr13")      # names(bFMtxExtFltLst)
            for( mName in mNameSet ){
                for( extFltName in names(bFMtxExtFltLst[[mName]]) ){
                    rptFile <- sprintf("Inspec_H%d_fMtxExt_%s_%s",lastH,mName,extFltName)
                    B.rptFMtx_ext( resultLst ,mName ,extFltName ,rptFile)
                }
            }
            mNameSet <- c("mfLAVrn")
            for( mName in mNameSet ){
                rptFile <- sprintf("Inspec_H%d_fMtxMultiR_%s",lastH,mName)
                B.rptFMtx_multiR( resultLst ,mName ,tgt.scMtx ,rptFile)
            }
            # mNameSet <- c( "crScrN03R" ,"crScrN03E" ,"crScrN03PhEvt" ,"crScrN03Sum" ,"crScrN04R" ,"crScrN04E" ,"crScrN04PhEvt" ,"crScrN04Sum" )
            mNameSet <- c( "crScrN03R" ,"crScrN03E" ,"crScrN03PhEvt" ,"crScrN03Sum" )
            for( crMName in mNameSet ){
                rptFile <- sprintf("Inspec_%s_H%d",crMName,lastH)
                B.rptCrScr( resultLst ,crMName ,tgt.scMtx ,rptFile )
            }
            B.rpt_CutRstClM( resultLst ,tgt.scMtx ,rptfile=sprintf("Inspec_CutRstCLM_%d",lastH) )
        }

        if( FALSE ){    # bSMtx
            mNameSet <- c( "sScore01" ,"sScore02" )
            for( mName in mNameSet ){
                rptFile <- sprintf("Inspec_bSMtx_%s_H%d",mName,lastH)
                B.rpt_bSMtx( resultLst ,mName ,rptFile )
            }

            mNameSet <- c( "sScore09" )
            for( mName in mNameSet ){
                for( extFltName in names(bSMtxExtFltLst[[mName]]) ){
                    rptFile <- sprintf("Inspec_H%d_bSMtxExt_%s_%s",lastH,mName,extFltName)
                    B.rpt_bSMtx_ext( resultLst ,mName ,extFltName ,rptFile )
                }
            }

            mNameSet <- c( "bsMR4567" )
            for( mName in mNameSet ){
                rptFile <- sprintf("Inspec_bSMtxMR_%s_H%d",mName,lastH)
                B.rpt_bSMtx_multiR( resultLst ,mName ,tgt.scMtx ,rptFile )
            }

            mNameSet <- c("bSMScr01R","bSMScr01E")  # names(bSMtxMCfg) or bSMtxCMLst
            for( crMName in mNameSet ){  # bUtil.cut2() 대체
                rptFile <- sprintf("Inspec_bSMtx_%s_H%d",crMName,lastH)
                B.rpt_bSMtx_crScr( resultLst ,crMName ,rptFile )
                # crCutRst <- bS.cut_M( crMName ,scoreMtx.grp ,cut.grp ,anaOnly=T )
                # cutRst.bS$cutInfoLst <- append( cutRst.bS$cutInfoLst ,crCutRst$cutInfoLst )
            }
        }


    }




}