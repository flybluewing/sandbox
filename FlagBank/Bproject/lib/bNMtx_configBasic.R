bNScoreMtxCfg <- list()
if( FALSE ){    # comment for value example & templete
    #         bNScoreMtxCfg[["score1"]] <- list(
    #             mName = mName   ,style=c( freqZero=TRUE )
    #             ,fCol = list(
    #                 "fCol1"=list( rng=matrix(c(0,1,0,1),ncol=2)     # c("min","max"),c("lev1","lev2")
    #                                 ,evt=matrix(c(c(1,1,2,3,4),c(1,2,3,4,5)),ncol=2)  # c("lev","val")
    #                                 ,evtMax.fCol = c( minLev=2 ,maxHpn=2 )       # fCol 별 전체 phase 대상으로 evt 발생 제한.
    #                             ) 
    #                 ,"fCol2"= ...
    #             )
    #           # FCust_stdCut.rawRow---------------------------------
    #             ,evtMax      minLev maxHpn minLevH maxHpnH    # evt in one phase over fCols
    #                     lev1      2      2       3       1
    #                     lev2      2      3       3       2
    #             ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 )  # 3개 이상 발생. 그 중의 2개 이상 low Evt가 존재하거나 희귀 Evt 하나이상 존재
    #                                                       #   하는 상태에서 반복 발생.
    #             ,rowRebDup = c( lowE=4 ,rareE=1 )         # low Evt 4개 이상이거나 희귀 Evt 하나이상의 반복 발생(3연속 발생.)
    #           # FCust_stdCut.hIdx-----------------------------------
    #                 $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
    #                         raw   1  2    2     2          1           1
    #                         evt   1  2    2     2          1           1
    #                 $summMtx.reb    all ph fCol phReb xyCnt.fCol xyCnt.phase
    #                             raw   1  1    1     1          1           1
    #                             evt   1  1    1     1          1           1
    #                 $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
    #                         rebCnt    2      2           1    2      2           1
    #                         rebDup    1      1           1    1      1           1
    #               ,summMtx.sum    <- c(raw=2 ,evt=2)              # 개개 cut은 피했지만 전체 발생 총합은 한계를 넘는 경우.
    #               ,scMtx.sz.sum   <- c(rebCnt.r=2 ,rebCnt.e=2)    # rebCnt.r = r.ph+r.fCol+r.dblHpnFlg    rebCnt.e = e.ph+e.fCol+e.dblHpnFlg
    #             ,isHard=NULL  # use default
    #         )


    # mName <- "template"
    # bNScoreMtxCfg[[mName]] <- list(
    #     mName = mName   ,style=c( freqZero=TRUE )
    #     ,fCol = list(
    #         "col1"=list( rng=matrix( c(NA,NA ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
    #                         ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2 ) # colname c("lev","val")
    #                     )
    #         ,"col2"=list( rng=matrix( c(NA,NA ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
    #                         ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2 ) # colname c("lev","val")
    #                     )
    #     )
    #     ,evtMax = NULL
    #     ,rowReb = NULL  ,rowRebDup = NULL
    #     ,summMtx = NULL ,summMtx.reb = NULL ,summMtx.sum = NULL
    #     ,scMtx.sz = NULL,scMtx.sz.sum = NULL
    #     ,isHard=NULL  # use default
    # )

}

summMtxName <- list( cName=c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" ) 
                    ,rName=c( "raw" ,"evt" )
)
summMtx.rebName <- list( cName=c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
                    ,rName=c( "raw" ,"evt" )
)
scMtx.szName <- list(   cName=c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                        ,rName=c( "rebCnt" ,"rebDup" )
)


mName <- "score1"
if( TRUE ){
    bNScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rem0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"rem0.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"rem0.len.val"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"rem1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"rem1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,2,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"rem1.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"c0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        )
            ,"c0.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"c0.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"c1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        )
            ,"c1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"c1.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"f0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        )
            ,"f0.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"f0.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"f1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        )
            ,"f1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"f1.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"zwNum"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        )
            ,"zwC1Num"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,evtMaxFColTot = NULL
        ,rowReb = NULL
        ,rowRebDup=NULL
        ,summMtx = NULL
        ,summMtx.reb = NULL
        ,summMtx.sum = NULL
        ,scMtx.sz = NULL
        ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "score2"
if( TRUE ){

    bNScoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rebV.r"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,1,2,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        )
            ,"rebL"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ) 
            ,"rebR"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        )
            ,"rebC.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"rebC.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ) 
            ,"rebC.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        )
            ,"rebC2.r"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"rebC2.c"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ) 
            ,"rebC2.f"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        )
            ,"inc.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"inc.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ) 
            ,"inc.f"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"inc.r2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"inc.c2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ) 
            ,"inc.f2"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )

            ,"inc.r3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"inc.c3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ) 
        ) 
        ,evtMax = NULL
        ,evtMaxFColTot = NULL
        ,rowReb = NULL
        ,rowRebDup = NULL
        ,summMtx = NULL
        ,summMtx.reb = NULL
        ,summMtx.sum = NULL
        ,scMtx.sz = NULL
        ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}




for( mName in names( bNScoreMtxCfg ) ){ # naming 추가.

    for( fcName in names(bNScoreMtxCfg[[mName]]$fCol) ){
        colnames(bNScoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
        rownames(bNScoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("min","max")

        colnames(bNScoreMtxCfg[[mName]]$fCol[[fcName]]$evt) <- c("lev","val")

        # ,forbidEvtReb=c(2,3)
        # ,freqVal=c(1,2)

        if( is.null(bNScoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol ) ){
            # fCol 별 전체 phase 대상으로 evt 발생 제한.( >= 기준 cut )
            # bNScoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
            bNScoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
        }
        if( is.null(bNScoreMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb) ){
            bNScoreMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb <- c(2,3,4)
        }
    }

    if( is.null(bNScoreMtxCfg[[mName]]$evtMaxFColTot) ){
        #   fCol 4 all Ph에서 CloseMax 가 나타난 fCol 수.
        bNScoreMtxCfg[[mName]]$evtMaxFColTot  <- c( lev1Max=3 ,lev2Max=3 ,lev3Max=3 )
    }
    if( is.null(bNScoreMtxCfg[[mName]]$evtMax) ){
        #   한 개 phase 내에서의 이벤트 발생 제한.
        #   "minLev"이상 이벤트가 maxHpn 보다 초과할 때 Cut
        evtMax <- matrix( c(2,2,3,1 ,2,3,3,2)
                            ,byrow=T ,ncol=4
                            ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
                        )
        bNScoreMtxCfg[[mName]]$evtMax     <- evtMax
    }
    if( is.null(bNScoreMtxCfg[[mName]]$rowReb) ){
        bNScoreMtxCfg[[mName]]$rowReb <- c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
    }
    if( is.null(bNScoreMtxCfg[[mName]]$rowRebDup) ){  # 조건 : >=
        bNScoreMtxCfg[[mName]]$rowRebDup <- c( lowE=1 ,rareE=1 )
    }
    if( is.null(bNScoreMtxCfg[[mName]]$summMtx) ){
        #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #             raw   0  0    0     0          0           0
        #             evt   0  0    0     0          0           0
        # cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
        # rName <- c( "raw" ,"evt" )
        thldVal <- c(   1 ,2 ,2 ,2 ,2 ,1    # xyCnt.fCol, xyCnt.fCol은 
                        ,1 ,2 ,2 ,2 ,2 ,1 
                    )
        bNScoreMtxCfg[[mName]]$summMtx <- matrix( thldVal ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
                )
    }
    if( is.null(bNScoreMtxCfg[[mName]]$summMtx.reb ) ){
        #     $summMtx.reb    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #                 raw   0  0    0     0          0           0
        #                 evt   0  0    0     0          0           0
        cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
        rName <- c( "raw" ,"evt" )
        thldVal <- c(   1 ,1 ,1 ,1 ,1 ,1    # xyCnt.fCol, xyCnt.fCol은 봐가며 조절해야 할 듯.
                        ,1 ,1 ,1 ,1 ,1 ,1 
                    )
        bNScoreMtxCfg[[mName]]$summMtx.reb <- matrix( thldVal ,byrow=T
                    ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
                )
    }
    if( is.null(bNScoreMtxCfg[[mName]]$scMtx.sz ) ){
        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
        #             rebCnt    0      0           0    0      0           0
        #             rebDup    0      0           0    0      0           0
        # cName <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
        # rName <- c( "rebCnt" ,"rebDup" )
        # thldVal <- c(   2 ,2 ,1 ,2 ,2 ,1
        #                 ,1 ,1 ,1 ,1 ,1 ,1 
        #             )
        thldVal <- c(    1 ,1 ,1 ,1 ,1 ,1
                        ,1 ,1 ,1 ,1 ,1 ,1 
                    )
        bNScoreMtxCfg[[mName]]$scMtx.sz <- matrix( thldVal ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    }
    if( is.null(bNScoreMtxCfg[[mName]]$summMtx.sum) ){
        # 개개 cut은 피했지만 전체 발생 총합은 한계를 넘는 경우.
        bNScoreMtxCfg[[mName]]$summMtx.sum <- c(raw=2 ,evt=2)
    }
    if( is.null(bNScoreMtxCfg[[mName]]$scMtx.sz.sum ) ){
        #   rebCnt.r = r.ph+r.fCol    rebCnt.e = e.ph+e.fCol
        bNScoreMtxCfg[[mName]]$scMtx.sz.sum <- c(rebCnt.r=2 ,rebCnt.e=2)
    }

}

