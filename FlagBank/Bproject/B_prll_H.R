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



    #   tgt.scMtx <- c("score1")    ;tgt.scMtx <- c( tgt.scMtx  ,c("sScoreNew") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScore01","sScore02","sScore03","sScore04","sScore05","sScore06","sScore07","sScore08") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScoreLAr13","sScoreLAr24","sScoreLVr13","sScoreLVr24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScoreLAe13","sScoreLAe24","sScoreLVe13","sScoreLVe24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScoreLAc13","sScoreLAc24","sScoreLVc13","sScoreLVc24") )
    tgt.scMtx <- c( tgt.scMtx  ,c("sScoreLAf13","sScoreLAf24","sScoreLVf13","sScoreLVf24") )

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
        crCutRst <- bS.cut_M( crMName ,scoreMtx.grp ,cut.grp ,anaOnly=T )
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


