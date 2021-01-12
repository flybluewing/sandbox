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
        curWd <- getwd();setwd("..");source("hCommon.R")
        setwd( curWd );source("header.r");source("B_H.R");source("B_prll_H.R")
    })
}
sfInit( parallel=T, cpus=prllNum )  ;prll.initHeader( ) ;sfExport("prllLog") 

sfExport("lastH")   ;sfExport("gEnv")    ;sfExport("fRstLst")    ;sfExport("allIdxLst")
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


    cutRstLst <- Bprll.stdCutTest( testData.grp ,tgt.scMtx ,testSpan ,exportObj=TRUE )

    if( TRUE ){    curHIdx <- testSpan

    }

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
    rptBanTyp <- c("aux_sfc","lastRawPair")
    rptBanM <- c(tgt.scMtx,c("mf4567","bsMR1234","bSMScr02R","bSMScr02E","bSMScr02PhEvt"))
    rptBanM <- c(rptBanM,c("bSMScr04R","bSMScr04E","bSMScr04PhEvt"))
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