source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")  ;source("B_prll_Hrpt.R")
lastH <- 880

load(sprintf("../Aproject/Obj_allIdxLstZ%d.save",lastH) )
load(sprintf("../Aproject/save/Obj_fRstLstZ%d.save",lastH) )
names(fRstLst) <- names(allIdxLst$stdFiltedCnt)
load(sprintf("../Aproject/save/Obj_gEnvZ%d.save",lastH))
crScrH <- crScrHTool$getData( )     ;crScrH <- crScrHTool$bySpan(crScrH,lastH)


#-[Parallel init work]-------------------------------------------------------------
prllNum <- 8     # 실수가 잦아서 그냥 오류 코드로 놔둔다.
prllLog <- k.getFlogObj( "./log/parallel_log_Hist.txt" )
prll.initHeader <- function( ){
    k <- sfLapply(1:prllNum,function(prllId){
        curWd <- getwd();setwd("..");source("hCommon.R")
        setwd( curWd );source("header.r");source("B_H.R");source("B_prll_H.R")
    })
    source("header.r")  ;source("B_H.R") ;source("B_prll_H.R")  ;source("B_prll_Hrpt.R")# for debug work
}
sfInit( parallel=T, cpus=prllNum )  ;prll.initHeader( ) ;sfExport("prllLog") 

sfExport("lastH")   ;sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst")
sfExport("crScrH")  ;sfExport("crScrHTool")
prllLog$fLogStr("parallel init", pTime=T ,pAppend=F )
cat(sprintf("* Parallel ready... see log : %s \n",prllLog$fileName))



if( FALSE ){    # stdZoid에 대한 cutting 시뮬레이션 예제 코드

    tgt.scMtx <- NULL
    tgt.scMtx <- c( tgt.scMtx  ,Bprll.getTgtScMtx( ) )

    configH <- lastH-20    # configH는 기본 cutting값을 얻기 위하는 시점에 따라 조절.
    testSpan <- (lastH - 19:0)   # configH 보다는 큰 시점에서 시작해야 함을 유의.
    if( TRUE ){ # stdFiltedCnt 0~2내에서만 테스트
        # B.getTgtHIdx()  참고
        sfc.InTest <- allIdxLst$stdFiltedCnt[as.character(testSpan)]
        testSpan <- testSpan[sfc.InTest %in% 0:2]
    }

    if( TRUE ){     # RM_B_prll.R
        load( sprintf("Obj_testData.grp.%d.%s.save",lastH,"all") )
        load( sprintf("Obj_testData_HCR.grp.%d.%s.save",lastH,"all") )
    } else {
        tStmp <- Sys.time()
        testData.grp <- B.get_testData.grp( testSpan ,gEnv ,allIdxLst ,fRstLst ,tgt.scMtx=tgt.scMtx)
        save( testData.grp ,file=sprintf("Obj_testData.grp.%d.%s.save",lastH,ifelse(is.null(tgt.scMtx),"all",tgt.scMtx) ) )
        testData_HCR.grp <- HCR.get_testData.grp( testSpan ,crScrH ,allIdxLst ,fRstLst ,lastH=NULL ,tgt.scMtx=NULL )
        save( testData_HCR.grp ,file=sprintf("Obj_testData_HCR.grp.%d.%s.save",lastH,ifelse(is.null(tgt.scMtx),"all",tgt.scMtx) ) )
        tDiff <- Sys.time() - tStmp
    }
    cutRstLst <- NULL

    cutRstLst <- Bprll.stdCutTest( testData.grp ,tgt.scMtx ,testSpan ,exportObj=TRUE )

    sfExport("tgt.scMtx")       ;sfExport("testData.grp")   ;sfExport("testData_HCR.grp")
    tStmp1 <- Sys.time()
    if( TRUE ){     # HCR
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

            cutRstLst <- lapply( names(cutRstLstHCR) ,function(nIdx){ cutRst <- cutRstLst[[nIdx]] 
                        cutRst$cutInfoLst <- append( cutRst$cutInfoLst ,cutRstLstHCR[[nIdx]]$cutInfoLst )
                        return(cutRst)
            })
            names(cutRstLst) <- names(cutRstLstHCR)
        }

        if( is.null(resultLst) || 0==length(resultLst) ){
            cat("    Warning!! resultLst is empty. \n")
        }
    }
    tDiff1 <- Sys.time() - tStmp1  ;tDiff1     # 2.1min / 7prllNum
    cat( sprintf("    resultLst %d (time cost : %.1f%s)  \n",length(resultLst),tDiff1,units(tDiff1)) )


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
    B.rptCutRstLst( cutRstLst ,file=rptFile ,rptBanTyp=rptBanTyp ,rptBanM=rptBanM )
    lastH   ;tgt.scMtx





    if( FALSE ){    # inspection --------------------------------------------------------------------
        B.rptCutRst1Score(      resultLst ,file=sprintf("CutRst1Score_%d",lastH)    )
        B.rptCutRst1Score_bS(   resultLst ,file=sprintf("CutRst1Score_%d_bS",lastH) )

        if( FALSE ){    # bFMtx
            mNameSet <- c("scoreLAr13","scoreLAr24","scoreLVr13","scoreLVr24")
            for( mName in mNameSet ){
                rptFile <- sprintf("Inspec_H%d_cutRst1Score",lastH)
                B.rptCutRst1Score_byMtx( resultLst ,mName ,rptFile ,transMtx=F )
            }
            mNameSet <- c("score1","score2","scoreLAr13")      # names(bFMtxExtFltLst)
            for( mName in mNameSet ){
                for( extFltName in names(bFMtxExtFltLst[[mName]]) ){
                    rptFile <- sprintf("Inspec_H%d_fMtxExt_%s_%s",lastH,mName,extFltName)
                    B.rptFMtx_ext( resultLst ,mName ,extFltName ,rptFile)
                }
            }
            mNameSet <- c("mfLAVrn")
            for( mfName in mNameSet ){
                rptFile <- sprintf("Inspec_H%d_fMtxMultiR_%s",lastH,mfName)
                B.rptFMtx_multiR( resultLst ,mfName ,tgt.scMtx ,rptFile)
            }
            # mNameSet <- c( "crScrN03R" ,"crScrN03E" ,"crScrN03PhEvt" ,"crScrN03Sum" ,"crScrN04R" ,"crScrN04E" ,"crScrN04PhEvt" ,"crScrN04Sum" )
            mNameSet <- c( "crScrN03R" ,"crScrN03E" ,"crScrN03PhEvt" ,"crScrN03Sum" )
            for( crMName in mNameSet ){
                rptFile <- sprintf("Inspec_%s_H%d",crMName,lastH)
                B.rptCrScr( resultLst ,crMName ,tgt.scMtx ,rptFile )
            }
            B.rpt_CutRstClM( resultLst ,tgt.scMtx ,rptfile=sprintf("Inspec_CutRstCLM_%d",lastH) )

            mNameSet <- c( "bScr01" ,"bScr02" )
            for( mName in mNameSet ){
                rptFile <- sprintf("Inspec_H%d_mf",lastH)
                B.rptCutRst1Score_mfMtx( resultLst ,mName ,rptFile ,transMtx=T )
            }

        }

        if( FALSE ){    # bSMtx
            mNameSet <- c( "sScore04" )
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