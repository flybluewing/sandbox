

scoreMtxCfg <- list()
if( FALSE ){    # comment for value example & templete
    #         scoreMtxCfg[["score1"]] <- list(
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
    # scoreMtxCfg[[mName]] <- list(
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
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rem0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=5 ,lev2Max=5 ,lev3Max=2 )
                        ) 
            ,"rem0.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=5 ,lev2Max=5 ,lev3Max=2 )
                            ,forbidEvtReb=c(3,4)
                        )
            ,"rem0.len.val"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"rem1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=7 ,lev2Max=4 ,lev3Max=2 )
                        )
            ,"rem1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,2,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=7 ,lev2Max=4 ,lev3Max=2 )
                        )
            ,"rem1.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"c0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=4 ,lev2Max=4 ,lev3Max=2 )
                            ,forbidEvtReb=c(3,4)
                        )
            ,"c0.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"c0.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"c1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=5 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=5 ,lev2Max=3 ,lev3Max=2 )
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
                            ,evtMax.fCol=c( lev1Max=4 ,lev2Max=4 ,lev3Max=2 )
                            ,forbidEvtReb=c(3,4)
                        )
            ,"zwC1Num"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        )
        )
        #   XXX.tot, XXX.val 관계 때문에 2개 fCol 동시발생이 흔함을 고려.
        ,evtMax = matrix( c(2,3,3,1 ,2,4,3,2)  # evt in row over fCols
                            ,byrow=T ,ncol=4
                            ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
                        )
        ,evtMaxFColTot = c( lev1Max=5 ,lev2Max=3 ,lev3Max=3 )
        ,rowReb = c( rawMin=3 ,lowE=3 ,rareE=2 ,dupESum=1 )
        ,rowRebDup=NULL
        ,summMtx = matrix( c( 1 ,2 ,3 ,2 ,2 ,1     ,1 ,2 ,3 ,2 ,2 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL
        ,summMtx.sum = c(raw=4 ,evt=4)
        ,scMtx.sz = matrix( c( 4 ,4 ,1 ,4 ,4 ,1     ,2 ,3 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=5)
        ,isHard=NULL  # use default
    )
} else {    # 원본 백업.

    # scoreMtxCfg[[mName]] <- list(
    #     mName = mName   ,style=c( freqZero=TRUE )
    #     ,fCol = list(
    #         "rem0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=5 ,lev2Max=5 ,lev3Max=2 )
    #                     ) 
    #         ,"rem0.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=5 ,lev2Max=5 ,lev3Max=2 )
    #                     )
    #         ,"rem0.len.val"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     )
    #         ,"rem1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=7 ,lev2Max=4 ,lev3Max=2 )
    #                     )
    #         ,"rem1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,1,2,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=7 ,lev2Max=4 ,lev3Max=2 )
    #                     )
    #         ,"rem1.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     )
    #         ,"c0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=4 ,lev2Max=4 ,lev3Max=2 )
    #                     )
    #         ,"c0.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                     )
    #         ,"c0.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                     )
    #         ,"c1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=5 ,lev2Max=3 ,lev3Max=2 )
    #                     )
    #         ,"c1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=5 ,lev2Max=3 ,lev3Max=2 )
    #                     )
    #         ,"c1.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                     )
    #         ,"f0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                     )
    #         ,"f0.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     )
    #         ,"f0.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     )
    #         ,"f1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                     )
    #         ,"f1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     )
    #         ,"f1.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     )
    #         ,"zwNum"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=4 ,lev2Max=4 ,lev3Max=2 )
    #                     )
    #         ,"zwC1Num"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                     )
    #     )
    #     #   XXX.tot, XXX.val 관계 때문에 2개 fCol 동시발생이 흔함을 고려.
    #     ,evtMax = matrix( c(2,3,3,1 ,2,4,3,2)  # evt in row over fCols
    #                         ,byrow=T ,ncol=4
    #                         ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
    #                     )
    #     ,evtMaxFColTot = c( lev1Max=5 ,lev2Max=3 ,lev3Max=3 )
    #     ,rowReb = c( rawMin=3 ,lowE=3 ,rareE=1 )
    #     ,rowRebDup=NULL
    #     ,summMtx = matrix( c( 1 ,2 ,3 ,2 ,2 ,1     ,1 ,2 ,3 ,2 ,2 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
    #             ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
    #             ,dimnames=list(summMtxName$rName,summMtxName$cName)
    #         )
    #     ,summMtx.reb = NULL
    #     ,summMtx.sum = c(raw=4 ,evt=4)
    #     ,scMtx.sz = matrix( c( 4 ,4 ,1 ,4 ,4 ,1     ,2 ,3 ,1 ,1 ,1 ,1 ) ,byrow=T
    #                         #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
    #                         #   "rebCnt" ,"rebDup"
    #                     ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
    #                     ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
    #                 )
    #     ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=5)
    #     ,isHard=NULL  # use default
    # )

}

mName <- "score2"
if( TRUE ){

    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rebV.r"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,1,2,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=13 ,lev2Max=3 ,lev3Max=2 )   # lev1Max 0은 아예 없는 듯..
                            # ,freqVal=c(1)
                            # ,forbidEvtReb=c(3)
                        )
            ,"rebL"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,forbidEvtReb=c(1,2,3)
                        ) 
            ,"rebR"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,forbidEvtReb=c(1,2,3)
                        )
            ,"rebC.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=9 ,lev2Max=3 ,lev3Max=2 )
                            # ,forbidEvtReb=c(3)
                        ) 
            ,"rebC.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ) 
            ,"rebC.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        )
            ,"rebC2.r"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            # ,freqVal=c(1)
                        )
            ,"rebC2.c"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            # ,freqVal=c(1)
                        ) 
            ,"rebC2.f"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=5 ,lev2Max=3 ,lev3Max=2 )
                            ,forbidEvtReb=c(3)
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
        ,evtMaxFColTot = c( lev1Max=5 ,lev2Max=3 ,lev3Max=3 )
        # ,rowReb = c( rawMin=4 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,rowReb = c( rawMin=2 ,lowE=4 ,rareE=2 ,dupESum=4 )
        ,rowRebDup = c( lowE=4 ,rareE=1 )
        # ,summMtx = NULL ,summMtx.reb = NULL 
        ,summMtx = matrix( c(   1 ,2 ,2 ,2 ,1 ,1     ,1 ,3 ,2 ,2 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = matrix( c(  1 ,1 ,1 ,1 ,1 ,1    ,1 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.sum = c(raw=2 ,evt=4)
        # ,scMtx.sz = NULL    
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,3 ,3 ,1     ,1 ,1 ,1 ,2 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=4)
        ,isHard=NULL  # use default
    )

} else {    # 원본 백업

    # scoreMtxCfg[[mName]] <- list(
    #     mName = mName   ,style=c( freqZero=TRUE )
    #     ,fCol = list(
    #         "rebV.r"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
    #                         ,evt=matrix( c(c(1,1,2,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=13 ,lev2Max=3 ,lev3Max=2 )   # lev1Max 0은 아예 없는 듯..
    #                     )
    #         ,"rebL"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                     ) 
    #         ,"rebR"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                     )
    #         ,"rebC.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=9 ,lev2Max=3 ,lev3Max=2 )
    #                     ) 
    #         ,"rebC.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                     ) 
    #         ,"rebC.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
    #                     )
    #         ,"rebC2.r"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     )
    #         ,"rebC2.c"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                     ) 
    #         ,"rebC2.f"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=5 ,lev2Max=3 ,lev3Max=2 )
    #                     )
    #         ,"inc.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
    #                     ) 
    #         ,"inc.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                     ) 
    #         ,"inc.f"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     )
    #         ,"inc.r2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     ) 
    #         ,"inc.c2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                     ) 
    #         ,"inc.f2"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     )

    #         ,"inc.r3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
    #                     ) 
    #         ,"inc.c3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
    #                     ) 
    #     ) 
    #     ,evtMax = NULL
    #     ,evtMaxFColTot = c( lev1Max=5 ,lev2Max=3 ,lev3Max=3 )
    #     ,rowReb = c( rawMin=4 ,lowE=1 ,rareE=1 ,dupESum=2 )
    #     ,rowRebDup = c( lowE=4 ,rareE=1 )
    #     # ,summMtx = NULL ,summMtx.reb = NULL 
    #     ,summMtx = matrix( c(   1 ,2 ,2 ,2 ,1 ,1     ,1 ,3 ,2 ,2 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
    #             ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
    #             ,dimnames=list(summMtxName$rName,summMtxName$cName)
    #         )
    #     ,summMtx.reb = matrix( c(  1 ,1 ,1 ,1 ,1 ,1    ,1 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
    #             ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
    #             ,dimnames=list(summMtxName$rName,summMtxName$cName)
    #         )
    #     ,summMtx.sum = c(raw=2 ,evt=4)
    #     # ,scMtx.sz = NULL    
    #     ,scMtx.sz = matrix( c( 2 ,2 ,1 ,3 ,3 ,1     ,1 ,1 ,1 ,2 ,1 ,1 ) ,byrow=T
    #                         #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
    #                         #   "rebCnt" ,"rebDup"
    #                     ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
    #                     ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
    #                 )
    #     ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=4)
    #     ,isHard=NULL  # use default
    # )

}

mName <- "score3"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rebPtn.1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                            ,freqVal=c(1)
                        )
            ,"rebPtn.n"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"snR3" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )    # flag
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"snMax.r"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                            ,forbidEvtReb=c(3)
                        ) 
            ,"snFCnt.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"snMax.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                            ,forbidEvtReb=c(3)
                        ) 
            ,"snFCnt.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"snMax.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                            ,forbidEvtReb=c(3)
                        ) 
            ,"snFCnt.f"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
        ) 
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=1 )
        ,rowRebDup = c( lowE=2 ,rareE=1 )
        ,summMtx = matrix( c(  1 ,2 ,3 ,2 ,2 ,1   ,1 ,2 ,3 ,2 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL
        ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 4 ,2 ,1 ,4 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=4)
        ,isHard=NULL  # use default
    )

} else if( FALSE ) {
    # scoreMtxCfg[[mName]] <- list(
    #     mName = mName   ,style=c( freqZero=TRUE )
    #     ,fCol = list(
    #         "rebPtn.1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
    #                     )
    #         ,"rebPtn.n"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
    #                     ) 
    #         ,"snR3" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )    # flag
    #                         ,evt=matrix( c(c(3),c(1)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
    #                     )
    #         ,"snMax.r"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
    #                     ) 
    #         ,"snFCnt.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
    #                     ) 
    #         ,"snMax.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
    #                     ) 
    #         ,"snFCnt.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
    #                     ) 
    #         ,"snMax.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
    #                     ) 
    #         ,"snFCnt.f"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
    #                     ) 
    #     ) 
    #     ,evtMax = NULL
    #     ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
    #     ,rowRebDup = c( lowE=2 ,rareE=1 )
    #     ,summMtx = matrix( c(  1 ,2 ,3 ,2 ,2 ,1   ,1 ,2 ,3 ,2 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
    #             ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
    #             ,dimnames=list(summMtxName$rName,summMtxName$cName)
    #         )
    #     ,summMtx.reb = NULL
    #     ,summMtx.sum = NULL
    #     ,scMtx.sz = matrix( c( 4 ,2 ,1 ,4 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
    #                         #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
    #                         #   "rebCnt" ,"rebDup"
    #                     ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
    #                     ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
    #                 )
    #     ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=4)
    #     ,isHard=NULL  # use default
    # )

}

mName <- "score4"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "pBanN.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"pBanN.n"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pE3"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pfNum"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"iBanN"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                            # ,forbidEvtReb=c(3)   # 고려 H868
                        ) 
            ,"iLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"FVa.m"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"FVa.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"m4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
        ) 
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,rowRebDup = NULL
        # ,summMtx = NULL
        ,summMtx = matrix( c(  1 ,2 ,2 ,2 ,2 ,2     , 1 ,2 ,2 ,2 ,2 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

} else if( FALSE ){ # 원본백업
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "pBanN.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"pBanN.n"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pE3"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"pfNum"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"iBanN"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"iLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"FVa.m"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"FVa.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
            ,"m4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ) 
        ) 
        ,evtMax = NULL
        ,rowReb = NULL  # use default   c( rawMin=  )
        ,rowRebDup = NULL
        # ,summMtx = NULL
        ,summMtx = matrix( c(  1 ,2 ,2 ,2 ,2 ,2     , 1 ,2 ,2 ,2 ,2 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "score5"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ,freqVal=c(1)
                    )
        ,"pBanN.n"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ,freqVal=c(1)
                    ) 
        ,"pLCol"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pE3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pfNum"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=3 )
                    ) 
        ,"iBanN"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ,freqVal=c(1)
                    ) 
        ,"iLCol"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"ifNum"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"FVa.m"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)  # 2는 생각보다 자주 일어나는 듯.
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ,freqVal=c(1,2)
                    ) 
        ,"FVa.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ,freqVal=c(1)
                    ) 
        ,"m4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
    ) 
    ,evtMax = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
    ,rowRebDup = NULL
    ,summMtx = matrix( c( 1 ,3 ,2 ,2 ,2 ,1   ,1 ,2 ,2 ,2 ,2 ,1 ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
                )
    ,summMtx.reb = matrix( c( 1 ,2 ,2 ,1 ,1 ,1      ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    #   c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
                    #   c( "raw" ,"evt" )
                    ,ncol=length(summMtx.rebName$cName) ,nrow=length(summMtx.rebName$rName) 
                    ,dimnames=list(summMtx.rebName$rName,summMtx.rebName$cName)
                )
    ,summMtx.sum = c(raw=4 ,evt=2)
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "score6"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pLCol"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                        ,forbidEvtReb=c(3) ,freqVal=c(1)   # H880(dupESum=3도 고려..)
                    ) 
        ,"pE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iBanN"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iLCol"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"FVa.m"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"FVa.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"m4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 ) # H880(dupESum=3 고려..)
    ,rowRebDup = NULL
    ,summMtx = matrix( c(   1 ,2 ,2 ,2 ,2 ,1     , 1 ,2 ,2 ,2 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = matrix( c(   1 ,2 ,2 ,2 ,1 ,1     , 1 ,2 ,2 ,2 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.sum = c(raw=3 ,evt=3)
    ,scMtx.sz = matrix( c(   2 ,2 ,1 ,2 ,2 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=3)
    ,isHard=NULL  # use default
)

mName <- "score7"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"pfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iBanN"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"FVa.m"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"FVa.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"m4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
    ,rowRebDup = NULL
    ,summMtx = NULL ,summMtx.reb = NULL ,summMtx.sum = NULL
    ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)

mName <- "score8"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "c31"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                        ,forbidEvtReb=integer(0)
                    )
        ,"c32"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                        ,forbidEvtReb=integer(0)
                    ) 
        ,"c33"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                        ,forbidEvtReb=integer(0)
                    ) 
        ,"c34"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                        ,forbidEvtReb=integer(0)
                    ) 
        ,"c21"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=4 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"c22"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=4 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"c23"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=4 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"c24"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=4 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"c25"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=4 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"max3"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"min3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=4 ,lev2Max=4 ,lev3Max=2 )
                        ,forbidEvtReb=integer(0)
                    ) 
        ,"max2MatCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"min2MatCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=3 )
                        ,forbidEvtReb=integer(0)
                    )
        ,"minMax2MatCnt"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"cTbl"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"fTbl"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
    )
    ,evtMax = NULL
    ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=2 ,dupESum=2 )
    ,rowRebDup = NULL
    ,summMtx = matrix( c(   1 ,2 ,2 ,2 ,2 ,2    ,1 ,2 ,2 ,2 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.reb = NULL ,summMtx.sum = c(raw=3 ,evt=3)
    ,scMtx.sz = matrix( c(  3 ,2 ,1 ,3 ,2 ,1    ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
    )
    ,scMtx.sz.sum = c( rebCnt.r=4 ,rebCnt.e=4 )
    ,isHard=NULL  # use default
)

mName <- "score9"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rCnt" =list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,freqVal=c(1)   ,forbidEvtReb=c(3)
                    )
        ,"rD2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"rDn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"rLr" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"rRl" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"eCnt"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=5 ,lev2Max=5 ,lev3Max=2 )
                        ,freqVal=c(1,2) ,forbidEvtReb=c(3)
                    ) 
        ,"eD2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,freqVal=c(1)
                    ) 
        ,"eDn" =list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,forbidEvtReb=c(3)
                    )
        ,"eLr" =list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,freqVal=c(1)   ,forbidEvtReb=c(3)  # H864
                    ) 
        ,"eRl" =list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,freqVal=c(1)   ,forbidEvtReb=c(3)
                    ) 
        ,"cCnt"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=6 ,lev2Max=6 ,lev3Max=2 )
                        ,freqVal=c(1)   ,forbidEvtReb=c(3)
                    ) 
        ,"cD2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,freqVal=c(1)
                    ) 
        ,"cDn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,freqVal=c(1)
                    ) 
        ,"cLr" =list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,freqVal=c(1)
                    ) 
        ,"cRl" =list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,freqVal=c(1)
                    ) 
        ,"fCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"fD2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"fDn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"fLr" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"fRl" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
    ) 
    ,evtMax = NULL
    # ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=2 )
    ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=2 ,dupESum=3 )
    ,rowRebDup = c( lowE=2 ,rareE=2 )
    ,summMtx = matrix( c(  1 ,3 ,2 ,2 ,2 ,1     , 1 ,3 ,2 ,2 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = matrix( c(  1 ,2 ,1 ,1 ,1 ,1     , 1 ,2 ,2 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.sum = c(raw=3 ,evt=4)
    ,scMtx.sz = matrix( c(  3 ,2 ,1 ,4 ,3 ,1     ,2 ,1 ,1 ,2 ,2 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=4)

    ,isHard=NULL  # use default
)


mName <- "scoreA"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "xaAVLen" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"axAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aMHpn" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aMHpnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aNHpn" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aNHpnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aSHpnVLen_abbA" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aSHpnVLen_abxbA" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH1" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )  # aaA 패턴 발생 가능성...
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH1VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH2" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH2VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH3" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH3VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAHn" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAHnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH1" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH1VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH2" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH2VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH3" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH3VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAHn" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAHnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbbaA" =list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbbaAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbabA" =list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbabAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabxbA" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabxbAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"remTblF" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
    )
    ,evtMax = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup = NULL
    ,summMtx = NULL     ,summMtx.reb = NULL     ,summMtx.sum = NULL
    ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)

mName <- "scoreB"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "xaAVLen" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"axAVLen" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aMHpn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aMHpnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aNHpn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aNHpnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aSHpnVLen_abbA" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aSHpnVLen_abxbA" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH1" =list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH1VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH2" =list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH2VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH3VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAHn" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAHnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH1" =list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH1VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH2" =list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH2VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH3VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAHn" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAHnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbbaA" =list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbbaAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbabA" =list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbabAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabxbA" =list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabxbAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
    )
    ,evtMaxFColTot = c( lev1Max=4 ,lev2Max=4 ,lev3Max=4 )
    ,evtMax = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup = NULL
    ,summMtx = NULL     ,summMtx.reb = NULL     ,summMtx.sum = NULL
    ,scMtx.sz = matrix( c(  1 ,2 ,1 ,1 ,2 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                    #             rebCnt    0      0           0    0      0           0
                    #             rebDup    0      0           0    0      0           0
    )
    ,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)

mName <- "scoreC"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "xaAVLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"axAVLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aMHpn" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aMHpnVLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aNHpn" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aNHpnVLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aSHpnVLen_abbA" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aSHpnVLen_abxbA" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH1" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"paaAH1VLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH2" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH2VLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH3VLen" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAHn" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAHnVLen" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH1" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"pabbAH1VLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH2" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH2VLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH3VLen" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAHn" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAHnVLen" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbbaA" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"pbbaAVLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbabA" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"pbabAVLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabxbA" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabxbAVLen" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
    )
    ,evtMax = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup = NULL
    ,summMtx = matrix( c(  1 ,2 ,2 ,2 ,2 ,1    ,1 ,2 ,2 ,2 ,2 ,2 ) ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
        #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #             raw   0  0    0     0          0           0
        #             evt   0  0    0     0          0           0
    )
    ,summMtx.reb = NULL     ,summMtx.sum = NULL
    ,scMtx.sz = matrix( c(  2 ,1 ,1 ,1 ,2 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                    #             rebCnt    0      0           0    0      0           0
                    #             rebDup    0      0           0    0      0           0
    )
    ,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)

mName <- "scoreD"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "xaAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"axAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aMHpn" =list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aMHpnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aNHpn" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aNHpnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aSHpnVLen_abbA" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"aSHpnVLen_abxbA" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH1" =list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"paaAH1VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH2" =list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH2VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH3" =list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAH3VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAHn" =list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"paaAHnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH1" =list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"pabbAH1VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH2" =list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH2VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH3" =list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAH3VLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAHn" =list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabbAHnVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbbaA" =list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"pbbaAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pbabA" =list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"pbabAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabxbA" =list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pabxbAVLen" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
    )
    ,evtMaxFColTot = c( lev1Max=4 ,lev2Max=4 ,lev3Max=3 )
    ,evtMax = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup = NULL
    ,summMtx = NULL     ,summMtx.reb = NULL     ,summMtx.sum = NULL
    ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,1 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                    #             rebCnt    0      0           0    0      0           0
                    #             rebDup    0      0           0    0      0           0
    )
    ,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)

mName <- "scoreE"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rH0MLen"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rH0Cnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rH0VCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cH0MLen"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cH0Cnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cH0VCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fH0MLen"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fH0Cnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fH0VCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )

        ,"rH1MLen"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rH1Cnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rH1VCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cH1MLen"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cH1Cnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cH1VCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fH1MLen"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fH1Cnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fH1VCnt"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )

        ,"rH2MLen"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rH2Cnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"rH2VCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cH2MLen"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cH2Cnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"cH2VCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fH2MLen"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2) 
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fH2Cnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
        ,"fH2VCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                    )
    )
    ,evtMax = NULL
    ,evtMaxFColTot = NULL
    # ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )    ,rowRebDup=NULL
    ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup = NULL
    ,summMtx = matrix( c(   1 ,3 ,2 ,2 ,2 ,1  ,1 ,2 ,2 ,2 ,2 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = NULL
    ,summMtx.sum = c(raw=3 ,evt=2)
    ,scMtx.sz = matrix( c(  3 ,3 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                    #             rebCnt    0      0           0    0      0           0
                    #             rebDup    0      0           0    0      0           0
    )
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "scoreLAr13"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,rowRebDup=NULL
        ,summMtx = matrix( c( 1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLAr24"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL
        ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVr13"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVr24"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLAe13"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = c( lev1Max=3 ,lev2Max=3 ,lev3Max=4 )
        ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,2 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = c(raw=3 ,evt=2)
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLAe24"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = c( lev1Max=3 ,lev2Max=3 ,lev3Max=4 )
        ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,2 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = c(raw=3 ,evt=2)
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVe13"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = c( lev1Max=3 ,lev2Max=3 ,lev3Max=4 )
        ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,2 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = c(raw=3 ,evt=2)
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVe24"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = c( lev1Max=3 ,lev2Max=3 ,lev3Max=4 )
        ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,2 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = c(raw=3 ,evt=2)
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLAc13"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,2 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = c(raw=3 ,evt=2)
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,1 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLAc24"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,2 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = c(raw=3 ,evt=2)
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,1 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVc13"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     # H898
        ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,2 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = c(raw=3 ,evt=2)
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,1 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVc24"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2) ,freqVal=c(1)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,2 ,2 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = c(raw=3 ,evt=2)
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,1 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLAf13"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )             ,rowRebDup=NULL
        ,summMtx = matrix( c( 1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLAf24"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )             ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVf13"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )             ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVf24"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colA1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colA6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"colB6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )             ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}


mName <- "scoreF"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "r16VReb"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cVReb"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSeqCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSeqMax"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = NULL
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c(   2 ,1 ,1 ,1 ,1 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "scoreFV"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "distR"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"distC"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"distF"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1,2)
                        )
            ,"rNumMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(3,4,5,6,7,8)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(2)
                        )
            ,"rRebNumMax"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            # ---------------------------------------------------------------------
            # rem 은 range체크 안함. reb 체크 용도로만 활용
            ,"eCnt"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1)
                        )
            ,"eNumMax"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(4,5,6,7,8)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(2,3)
                        )
            ,"eRebNumMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1,2)
                        )
            ,"cNumMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(2)
                        )
            ,"cRebNumMax"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fNumMax"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fRebNumMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = NULL      ,evtMaxFColTot = NULL
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx =  matrix( c(  1 ,3 ,3 ,1 ,2 ,1  ,1 ,2 ,2 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.reb = matrix( c(  1 ,2 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T
                    ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
        )
        ,summMtx.sum = c(raw=3 ,evt=3)
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,3 ,3 ,1  ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )    
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=4)
        ,isHard=NULL  # use default
    )
}

mName <- "scoreGS"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rMatCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rExtMax"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            # rem은 rng,evt 적용의미 없음. 오로지 reb 활용목적으로만 쓸 것.
            ,"eMatCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eExtMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSumCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eValCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol= c( lev1Max=4 ,lev2Max=3 ,lev3Max=3 )
                        )
            ,"cMatCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cExtMax"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fMatCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fExtMax"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = NULL      ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL
        ,summMtx = matrix( c( 1 ,4 ,3 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=1)
        ,summMtx.reb = matrix( c( 1 ,2 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 6 ,4 ,1 ,2 ,2 ,1  ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=6 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}
mName <- "scoreGSh2"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rMatCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1)
                        )
            ,"rExtMax"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1)
                        )
            ,"rSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            # rem은 rng,evt 적용의미 없음. 오로지 reb 활용목적으로만 쓸 것.
            ,"eMatCnt"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1)
                        )
            ,"eExtMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1,2)
                        )
            ,"eSumCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1)   # scoreGSh2에서는 나름 빈도있게 발생함.
                        )
            ,"eValCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol= c( lev1Max=4 ,lev2Max=3 ,lev3Max=3 )
                        )
            ,"cMatCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1)
                        )
            ,"cExtMax"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,2,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1)
                        )
            ,"cSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cValCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fMatCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1)
                        )
            ,"fExtMax"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(1)
                        )
            ,"fSumCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fValCnt"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,4,3,2)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL
        ,summMtx = matrix( c( 2 ,3 ,3 ,2 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=7 ,evt=1)  # *H801...
        ,summMtx.reb = matrix( c( 2 ,2 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 6 ,4 ,1 ,2 ,2 ,1  ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
        ,scMtx.sz.sum = c(rebCnt.r=6 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}
mName <- "scoreGS3"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rMatCntH1"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rMatCntH2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rRebMaxH1"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rRebMaxH2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eMatCntH1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eMatCntH2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eRebMaxH1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eRebMaxH2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cMatCntH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cMatCntH2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cRebMaxH1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cRebMaxH2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fMatCntH1"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fMatCntH2"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fRebMaxH1"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fRebMaxH2"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3,3),c(1,2,3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = NULL      ,evtMaxFColTot = NULL
        ,rowReb = NULL      #   H842 reb2(cH2)
        ,rowRebDup=NULL
        ,summMtx = matrix( c( 1 ,3 ,3 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=1)
        ,summMtx.reb = NULL
        ,scMtx.sz = matrix( c( 3 ,3 ,1 ,1 ,1 ,1  ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mName <- "scorePSh"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rSeq0"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSeq1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSeqN"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rNSeq"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSyc0"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rSyc1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"rColCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSeq0"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSeq1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSeqN"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eNSeq"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSyc0"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eSyc1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"eColCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSeq0"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSeq1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSeqN"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cNSeq"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSyc0"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cSyc1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"cColCnt"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSeq0"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSeq1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSeqN"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fNSeq"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSyc0"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fSyc1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"fColCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = NULL      ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL    # ColCnt 컬럼으로 인해 hpn이 2개씩 생성된다.
        ,summMtx = matrix( c( 1 ,3 ,3 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=2)
        ,summMtx.reb = matrix( c( 1 ,1 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 3 ,4 ,1 ,2 ,3 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=6 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}

mName <- "scorePSrp"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "r1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e1TotSize"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c2TotSize"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
        )
        ,evtMax = NULL      ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL    # ColCnt 컬럼으로 인해 hpn이 2개씩 생성된다.
        ,summMtx = matrix( c( 1 ,3 ,3 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=2)
        ,summMtx.reb = matrix( c( 1 ,2 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 4 ,4 ,1 ,2 ,2 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}

mName <- "scorePSrpRaw"
if( TRUE ){
    scoreMtxCfg[[mName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "r1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r1ValSize"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"r2TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"r2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"e1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e1ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"e2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"e2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"c1TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c1ValSize"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c2TotSize"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"c2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"f1TotSize"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f1ValSize"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"f2TotSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
            ,"f2ValSize"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,evtMax.fCol=NULL   # c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=1
                        )
        )
        ,evtMax = NULL      ,evtMaxFColTot = NULL
        ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )      ,rowRebDup=NULL    # ColCnt 컬럼으로 인해 hpn이 2개씩 생성된다.
        ,summMtx = matrix( c( 1 ,2 ,2 ,2 ,2 ,2  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=3 ,evt=2)
        ,summMtx.reb = matrix( c( 1 ,2 ,2 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,scMtx.sz = matrix( c( 4 ,4 ,1 ,1 ,1 ,1  ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                    # c( "rebCnt" ,"rebDup" ) / c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=1)
        ,isHard=NULL  # use default
    )
}


mName <- "bScr01"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
         "raw.1"=list( rng=matrix( c(NA,NA ,1,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"raw.3"=list( rng=matrix( c(NA,NA ,1,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3,3),c(2,3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"raw.4"=list( rng=matrix( c(NA,NA ,1,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,1,3,3,3),c(2,3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"raw.6"=list( rng=matrix( c(NA,NA ,1,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"rem.1"=list( rng=matrix( c(NA,NA ,1,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"rem.3"=list( rng=matrix( c(NA,NA ,1,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"rem.4"=list( rng=matrix( c(NA,NA ,1,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"rem.6"=list( rng=matrix( c(NA,NA ,1,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c.1"=list( rng=matrix( c(NA,NA ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c.3"=list( rng=matrix( c(NA,NA ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c.4"=list( rng=matrix( c(NA,NA ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c.6"=list( rng=matrix( c(NA,NA ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"raw.ZW"=list( rng=matrix( c(NA,NA ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"rem.ZW"=list( rng=matrix( c(NA,NA ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c.ZW"=list( rng=matrix( c(NA,NA ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
    )
    ,evtMax = NULL
    ,rowReb = c( rawMin=1 ,lowE=4 ,rareE=1 ,dupESum=4 )     ,rowRebDup=NULL
    # ,rowRebDup = c( lowE=3 ,rareE=1 )
    ,summMtx = NULL ,summMtx.reb = NULL ,summMtx.sum = NULL
    ,scMtx.sz = NULL,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)

mName <- "bScr02"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "r.lm"=list( rng=matrix( c(NA,NA ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"r.m2"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"r.mN"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq.lma"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq.lmt"=list( rng=matrix( c(NA,NA ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq.lmaRem"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq.lmtRem"=list( rng=matrix( c(NA,NA ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3,3),c(3,4,5,6,7)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq.ma"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq.mt"=list( rng=matrix( c(NA,NA ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,4,4,4),c(3,4,5,6,7,8)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c2.lm"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,4),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c2.m2"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,4),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c2.mN"=list( rng=matrix( c(NA,NA ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq3.lma"=list( rng=matrix( c(NA,NA ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(3,3,4,4),c(1,2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq3.lmt"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3,4),c(2,3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq3.lmaRem"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3,4),c(2,3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq3.lmtRem"=list( rng=matrix( c(NA,NA ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3,4),c(3,4,5,6,7)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq3.ma"=list( rng=matrix( c(NA,NA ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq3.m2"=list( rng=matrix( c(NA,NA ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3,4),c(1,2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"sq3.mt"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c3.lma"=list( rng=matrix( c(NA,NA ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(3,3,3),c(1,2,3)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c3.lmt"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c3.ma"=list( rng=matrix( c(NA,NA ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"c3.mt"=list( rng=matrix( c(NA,NA ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,4),c(2,3,4,5)) ,ncol=2 ) # colname c("lev","val")
                    )
    )
    ,evtMax = NULL
    ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
    ,summMtx = NULL ,summMtx.reb = NULL ,summMtx.sum = NULL
    ,scMtx.sz = NULL,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)




mfName <- "mfABCD"
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "aMHpn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"aNHpn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"paaAH1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"paaAH2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"paaAH3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"paaAHn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pabbAH1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) # H880, H793, H799
                        )
            ,"pabbAH2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pabbAH3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pabbAHn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pbbaA" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pbabA" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pabxbA" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pNearSum" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot = NULL
        ,evtMax     = NULL      
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     ,rowRebDup=NULL
        ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c(  2 ,2 ,1 ,1 ,1 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mfName <- "mf4567"
if( TRUE ){     #   freqVal 재할용 가능성 있음.
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "pBanN.r" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL  ,freqVal=1
                        )
            ,"pBanN.n" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL  ,freqVal=1
                        )
            ,"pLCol" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL  ,freqVal=1
                        )
            ,"pE3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pE4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pMH" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"pfNum" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"iBanN" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"iLCol" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"iE3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"iE4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"iMH" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"ifNum" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"FVa.m" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL  ,freqVal=1
                        )
            ,"FVa.c" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"m4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"xBan.x" =list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"xLCol" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"xEn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"xfNum" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"xMH" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"eSum_FVaM4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot = NULL
        ,evtMax     = NULL      
        # ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,rowRebDup = NULL
        ,summMtx   = matrix( c(  1 ,3 ,2 ,2 ,2 ,1     , 1 ,3 ,2 ,2 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.reb = NULL
        ,summMtx.sum = c(raw=4 ,evt=3)
        ,scMtx.sz = matrix( c(  4 ,4 ,1 ,2 ,2 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=6 ,rebCnt.e=3)
        ,isHard=NULL  # use default
    )
}


mfName <- "mfFreqVal1234"   # freqReb : s2
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(   # freqVal 전용이므로  min/max, evt는 의미없다.
            "s1rem0.len.tot" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s1rem1.len.tot" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s1c0.len.tot" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s1c1.len.tot" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s1f0.len.tot" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s1f1.len.tot" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s1zwNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2rebV.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2rebC.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2rebC.c" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2rebC.f" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2rebC2.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2rebC2.c" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2rebC2.f" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2inc.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2inc.c" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2inc.f" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2inc.r2" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2inc.c2" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2inc.f2" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2inc.r3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s2inc.c3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s3rebPtn.1" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s3snMax.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s3snMax.c" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s3snMax.f" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s4pBanN.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s4pBanN.n" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s4pLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s4pE3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s4pfNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s4iBanN" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s4iLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s4ifNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
            ,"s4FVa.m" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)
                            ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                        )
        )
        ,evtMaxFColTot = NULL
        ,evtMax     = NULL      
        ,rowReb = c( rawMin=2 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,rowRebDup = NULL
        ,summMtx   = matrix( c(  1 ,3 ,5 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.reb = NULL
        ,summMtx.sum = c(raw=5 ,evt=3)
        ,scMtx.sz = matrix( c(  3 ,5 ,1 ,1 ,1 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}


mfName <- "mfFreqVal89"
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "s8_c21" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_c22" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_c23" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_c24" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_c25" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_min3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_min2MatCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s9_rCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=1
                        )
            ,"s9_eCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=1:2
                        )
            ,"s9_cCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=1:2
                        )
            ,"s9_fCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot = NULL       ,evtMax = NULL      
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )             ,rowRebDup = NULL
        ,summMtx   = matrix( c(  1 ,4 ,2 ,2 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.reb = matrix( c(  1 ,2 ,2 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=3)
        ,scMtx.sz = matrix( c(  4 ,4 ,1 ,1 ,1 ,1   ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mfName <- "mfFreqVal259"
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "s2rebV.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2rebC.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2rebC.c" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2rebC.f" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2rebC2.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2rebC2.c" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2rebC2.f" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2inc.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2inc.c" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2inc.f" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2inc.r2" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2inc.c2" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2inc.f2" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2inc.r3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s2inc.c3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(10)) ,ncol=2)     ,evtMax.fCol=NULL
                        )

            ,"s5pBanN.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5pBanN.n" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5pLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5pE3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5pfNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5iBanN" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5iLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5ifNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5FVa.m" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )

            ,"s9_rCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s9_eCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s9_cCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s9_fCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot = NULL       ,evtMax = NULL      
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     # H806 s2rebC2.r(1)
        ,rowRebDup = NULL
        ,summMtx   = matrix( c(  1 ,1 ,4 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.reb = matrix( c(  1 ,1 ,3 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=3)
        ,scMtx.sz = matrix( c(  1 ,4 ,1 ,1 ,1 ,1   ,1 ,3 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )

}


mfName <- "mf9ELecf"
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "rCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                            ,freqVal=1
                        )
            ,"eCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                            ,freqVal=1:2
                        )
            ,"cCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                            ,freqVal=1
                        )
            ,"fCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                            ,freqVal=1
                        )

            ,"cH0MLen" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"cH1MLen" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"cH2MLen" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ae13Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ae13Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ae24Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ae24Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ve13Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ve13Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ve24Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ve24Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )

            ,"Ac13Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ac13Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ac24Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ac24Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vc13Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vc13Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vc24Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vc24Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )

            ,"Af13Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Af13Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Af24Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Af24Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vf13Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vf13Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vf24Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vf24Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot = NULL
        ,evtMax     = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,rowRebDup = NULL
        ,summMtx   = matrix( c(  1 ,3 ,3 ,1 ,2 ,2     , 1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.reb = matrix( c(  1 ,2 ,2 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
        ,summMtx.sum = c(raw=4 ,evt=3)
        ,scMtx.sz = matrix( c(  4 ,4 ,1 ,3 ,3 ,1   ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=4)
        ,isHard=NULL  # use default
    )
}

mfName <- "mfE_59FVLav"  # score5, score9, scoreFV, scoreLAe, scoreLVe
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            # score5
            "s5pBanN.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5pBanN.n" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5pLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5pE3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5pfNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5iBanN" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5iLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5ifNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s5FVa.m" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            # score9
            ,"s9ECnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                            # ,freqVal=1:3
                        )
            # scoreFV
            ,"fvECnt"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                            # ,freqVal=1:3
                        )
            # "scoreLAe13","scoreLAe24","scoreLVe13","scoreLVe24"
            ,"Ae13Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ae13Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ae24Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ae24Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ve13Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ve13Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ve24Hpn1A" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ve24Hpn1B" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3),c(2,3)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot=NULL ,evtMax = NULL
        ,rowReb = c( rawMin=4 ,lowE=2 ,rareE=1 ,dupESum=2 ) # *H860 에서 reb(3)이 나와버려서리.
        ,rowRebDup = NULL
        ,summMtx = matrix( c(  1 ,3 ,3 ,2 ,2 ,2     , 1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,summMtx.reb = matrix( c(  1 ,2 ,2 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,summMtx.sum = c(raw=5 ,evt=2)
        ,scMtx.sz = matrix( c(  4 ,4 ,1 ,3 ,3 ,1   ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=4)
        ,isHard=NULL  # use default
    )

}

mfName <- "mfrcf_N9FVLav"
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            # score467
            "sNpBanN.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sNpBanN.n" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sNpLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sNpE3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sNpfNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sNiBanN" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sNiLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sNifNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sNFVa.m" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            # score9
            ,"s9rCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s9cCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s9fCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            # scoreFV
            ,"fvdistR"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvdistC"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvdistF"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvrCnt"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvcCnt"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvfCnt"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            # "scoreLAx13","scoreLAx24","scoreLVx13","scoreLVx24"
            ,"Ax13Hpn1A" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ax13Hpn1B" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ax24Hpn1A" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ax24Hpn1B" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vx13Hpn1A" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vx13Hpn1B" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vx24Hpn1A" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vx24Hpn1B" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot=NULL ,evtMax = NULL
        ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,rowRebDup = NULL
        ,summMtx = matrix( c(  1 ,4 ,4 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,summMtx.reb = matrix( c(  1 ,2 ,2 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,summMtx.sum = c(raw=5 ,evt=2)
        ,scMtx.sz = matrix( c(  4 ,5 ,1 ,1 ,1 ,1   ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mfName <- "mfrcf_89FVLav"
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            # score8
            "s8_c21" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_c22" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_c23" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_c24" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_c25" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_min3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_min2MatCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            # score9
            ,"s9rCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s9cCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"s9fCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            # scoreFV
            ,"fvdistR"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvdistC"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvdistF"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvrCnt"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvcCnt"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            ,"fvfCnt"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)
                        )
            # "scoreLAx13","scoreLAx24","scoreLVx13","scoreLVx24"
            ,"Ax13Hpn1A" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ax13Hpn1B" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ax24Hpn1A" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Ax24Hpn1B" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vx13Hpn1A" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vx13Hpn1B" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vx24Hpn1A" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
            ,"Vx24Hpn1B" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(9)) ,ncol=2)     ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot=NULL ,evtMax = NULL
        ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=2 )
        ,rowRebDup = NULL
        ,summMtx = matrix( c(  1 ,4 ,4 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,summMtx.reb = matrix( c(  1 ,2 ,2 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,summMtx.sum = c(raw=4 ,evt=2)
        ,scMtx.sz = matrix( c(  4 ,4 ,1 ,1 ,1 ,1   ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
        ,isHard=NULL  # use default
    )
}

mfName <- "mfEreb1"
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
             "sFVeCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGSeExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGSh2eExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGS3eRebMaxHn" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSheColCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrpeNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrpRaweNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot=NULL ,evtMax = NULL
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup = NULL
        ,summMtx = matrix( c(  1 ,4 ,3 ,2 ,1 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,summMtx.sum =  c(raw=4 ,evt=1)
        ,summMtx.reb = NULL
        ,scMtx.sz = matrix( c(  5 ,3 ,1 ,1 ,1 ,1   ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=1)
        ,isHard=NULL  # use default
    )
}

mfName <- "mfEreb0"
if( TRUE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "s5HpnCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=c(1,2)
                        )
            ,"s9eCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=c(1,2)
                        )
            ,"AVe13Hpn1A" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=c(1,2)
                        )
            ,"AVe13Hpn1B" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=c(1,2)
                        )
            ,"AVe24Hpn1A" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=c(1,2)
                        )
            ,"AVe24Hpn1B" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=c(1,2)
                        )
            ,"sFVeCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=c(1,2)
                        )
            ,"sGSeExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGSh2eExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGS3eRebMaxHn" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSheColCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrpeNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrpRaweNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot=NULL ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup = NULL
        ,summMtx = matrix( c(  1 ,2 ,2 ,1 ,1 ,1     , 1 ,2 ,2 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,summMtx.sum =  c(raw=3 ,evt=1)
        ,summMtx.reb = NULL
        ,scMtx.sz = matrix( c(  2 ,2 ,1 ,1 ,1 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=1)
        ,isHard=NULL  # use default
    )
}


mfName <- "mfRF"
if( FALSE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "s4HpnCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sBHpnCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_c2nCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_min3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s8_min2MatCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s9rCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            ,freqVal=c(1,2)
                        )
            ,"AVr13Hpn1A" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVr13Hpn1B" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVr24Hpn1A" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVr24Hpn1B" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sFVrCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGSrExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGSh2rExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGS3rRebMaxHn" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPShrColCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrprNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrpRawrNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot=NULL ,evtMax = NULL
        ,rowReb = NULL
        ,rowRebDup = NULL
        ,summMtx = matrix( c(  1 ,3 ,2 ,2 ,2 ,2     , 1 ,2 ,2 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,summMtx.sum =  c(raw=3 ,evt=1)
        ,summMtx.reb = NULL
        ,summMtx = matrix( c(  1 ,2 ,1 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )     
        ,scMtx.sz = matrix( c(  3 ,3 ,1 ,1 ,1 ,1   ,2 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                        #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                        #             rebCnt    0      0           0    0      0           0
                        #             rebDup    0      0           0    0      0           0
        )
        ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=1)
        ,isHard=NULL  # use default
    )
}

mfName <- "mfC"
if( FALSE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "s6HpnCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sCHpnCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s9cCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVc13Hpn1A" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVc13Hpn1B" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVc24Hpn1A" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVc24Hpn1B" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sFVcCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGScExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGSh2cExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGS3cRebMaxHn" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPShcColCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrpcNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrpRawcNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot=NULL ,evtMax = NULL
        ,rowReb = NULL
        ,rowRebDup = NULL
        ,summMtx = NULL ,summMtx.sum = NULL
        ,summMtx.reb = NULL
        ,scMtx.sz = NULL
        ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mfName <- "mfF"
if( FALSE ){
    scoreMtxCfg[[mfName]] <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "s7HpnCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sBHpnCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"s9rCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVf13Hpn1A" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVf13Hpn1B" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVf24Hpn1A" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"AVf24Hpn1B" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sFVfCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGSfExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGSh2fExtMax" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sGS3fRebMaxHn" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPShfColCnt" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrpfNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
            ,"sPSrpRawfNTotSize" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)      ,evtMax.fCol=NULL
                        )
        )
        ,evtMaxFColTot=NULL ,evtMax = NULL
        ,rowReb = NULL
        ,rowRebDup = NULL
        ,summMtx = NULL ,summMtx.sum = NULL
        ,summMtx.reb = NULL
        ,scMtx.sz = NULL
        ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

#   추가 요소
#       - mf4567 : freqVal 재활용 가능성 있음.
#       - mfLArn, mfLVrn 등.. : reb는 의미없으나 evt와 hpn1에 대한 min/max 제한가능.
#       - mfLAV_hpn1 : rcf에서의 hpn1 통합(col1~col6). freqVal로서의 재활용 가능성 있음.
#       - mf25ELecf : 발생 빈도 확인 후 추가. ELecf 필터링이 목적.
#       - mf2_8c2x  : score2 에서의 cStep 일치와 score8의 c2x 컬럼 동시일치 가능성 (fMtx를 새로 짜야 할 수도..)



for( mName in names( scoreMtxCfg ) ){ # naming 추가.

    for( fcName in names(scoreMtxCfg[[mName]]$fCol) ){
        colnames(scoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
        rownames(scoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("min","max")

        colnames(scoreMtxCfg[[mName]]$fCol[[fcName]]$evt) <- c("lev","val")

        # ,forbidEvtReb=c(2,3)
        # ,freqVal=c(1,2)

        if( is.null(scoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol ) ){
            # fCol 별 전체 phase 대상으로 evt 발생 제한.( >= 기준 cut )
            # scoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
            scoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
        }
        if( is.null(scoreMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb) ){
            scoreMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb <- c(2,3,4)
        }
    }

    if( is.null(scoreMtxCfg[[mName]]$evtMaxFColTot) ){
        #   fCol 4 all Ph에서 CloseMax 가 나타난 fCol 수.
        scoreMtxCfg[[mName]]$evtMaxFColTot  <- c( lev1Max=3 ,lev2Max=3 ,lev3Max=3 )
    }
    if( is.null(scoreMtxCfg[[mName]]$evtMax) ){
        #   한 개 phase 내에서의 이벤트 발생 제한.
        #   "minLev"이상 이벤트가 maxHpn 보다 초과할 때 Cut
        evtMax <- matrix( c(2,2,3,1 ,2,3,3,2)
                            ,byrow=T ,ncol=4
                            ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
                        )
        scoreMtxCfg[[mName]]$evtMax     <- evtMax
    }
    if( is.null(scoreMtxCfg[[mName]]$rowReb) ){
        scoreMtxCfg[[mName]]$rowReb <- c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
    }
    if( is.null(scoreMtxCfg[[mName]]$rowRebDup) ){  # 조건 : >=
        scoreMtxCfg[[mName]]$rowRebDup <- c( lowE=1 ,rareE=1 )
    }
    if( is.null(scoreMtxCfg[[mName]]$summMtx) ){
        #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #             raw   0  0    0     0          0           0
        #             evt   0  0    0     0          0           0
        # cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
        # rName <- c( "raw" ,"evt" )
        thldVal <- c(   1 ,2 ,2 ,2 ,2 ,1    # xyCnt.fCol, xyCnt.fCol은 
                        ,1 ,2 ,2 ,2 ,2 ,1 
                    )
        scoreMtxCfg[[mName]]$summMtx <- matrix( thldVal ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
                )
    }
    if( is.null(scoreMtxCfg[[mName]]$summMtx.reb ) ){
        #     $summMtx.reb    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #                 raw   0  0    0     0          0           0
        #                 evt   0  0    0     0          0           0
        cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
        rName <- c( "raw" ,"evt" )
        thldVal <- c(   1 ,1 ,1 ,1 ,1 ,1    # xyCnt.fCol, xyCnt.fCol은 봐가며 조절해야 할 듯.
                        ,1 ,1 ,1 ,1 ,1 ,1 
                    )
        scoreMtxCfg[[mName]]$summMtx.reb <- matrix( thldVal ,byrow=T
                    ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
                )
    }
    if( is.null(scoreMtxCfg[[mName]]$scMtx.sz ) ){
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
        scoreMtxCfg[[mName]]$scMtx.sz <- matrix( thldVal ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    }
    if( is.null(scoreMtxCfg[[mName]]$summMtx.sum) ){
        # 개개 cut은 피했지만 전체 발생 총합은 한계를 넘는 경우.
        scoreMtxCfg[[mName]]$summMtx.sum <- c(raw=2 ,evt=2)
    }
    if( is.null(scoreMtxCfg[[mName]]$scMtx.sz.sum ) ){
        #   rebCnt.r = r.ph+r.fCol    rebCnt.e = e.ph+e.fCol
        scoreMtxCfg[[mName]]$scMtx.sz.sum <- c(rebCnt.r=2 ,rebCnt.e=2)
    }

}


# =============================================================================================================
#   sfcMtxCfg
# =============================================================================================================
#   FCust_stdCut_AllM( ) 에서 사용되고... bUtil.cut2( ) 에서 사용되는데, bUtil.cut2() 는 bS로 대체되었던 거 같은디..

sfcMtxCfg <- list()   # B.makeHMtxLst() 의 stdFilter참고
if( 0==length(sfcMtxCfg) ){
    sfcName <- "sfcLate"
    sfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "sfc0"
    sfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "sfc1"
    sfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "sfc2"
    sfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "NGD0000.A"
    sfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "NGA0100.A"
    sfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "NGAP000.E"
    sfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )
}

for( hName in names(sfcMtxCfg) ){

    if( is.null(sfcMtxCfg[[hName]]$basic$prime) ){
        sfcMtxCfg[[hName]]$basic$prime <- matrix( c(1,8,0,1 ,0,1,0,1 ,0,9,0,4 ,0,0,0,1 ) 
                            ,ncol=2 ,byrow=T
                            ,dimnames=list(c(   "zeroCntM_raw" ,"zeroCntPh_raw" 
                                                ,"rebMtxM_raw" ,"rebMtxPh_raw" 
                                                ,"zeroCntM_evt","zeroCntPh_evt"
                                                ,"rebMtxM_evt" ,"rebMtxPh_evt"  # 전제조건 : hpn > 1
                                            )
                                            ,c("min","max")
                            ) 
                        )
    }

    # if( is.null(sfcMtxCfg[[hName]]$basic$zeroCnt) ){
    #     sfcMtxCfg[[hName]]$basic$zeroCntM <- matrix( c(0,0 ,0,1) 
    #                         ,nrow=2 ,byrow=T 
    #                         ,dimnames=list(c("zeroCntM","zeroCntPh"),c("min","max")) 
    #                     )
    # }

    # if( is.null(sfcMtxCfg[[hName]]$evtMax) ){   }

}










# =============================================================================================================
#   참고용 백업 코드
# =============================================================================================================
if( FALSE ){

    mfName <- "mfFreqVal567"    # freqReb : s5 (폐지... s5에서 reb2 독식해서 효력이 없어지는 거 같다.)
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "s5pBanN.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s5pBanN.n" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s5pLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s5pE3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s5pfNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s5iBanN" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s5iLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s5ifNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s5FVa.m" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s6pBanN.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s6pBanN.n" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s6pLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s6pE3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s6pfNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s6iBanN" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s6iLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s6ifNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s6FVa.m" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s7pBanN.r" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s7pBanN.n" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s7pLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s7pE3" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s7pfNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s7iBanN" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s7iLCol" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s7ifNum" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
                ,"s7FVa.m" =list( rng=matrix( c(0,9 ,0,9) ,ncol=2 )
                                ,evt=matrix( c(c(1),c(9)) ,ncol=2)      ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      
            ,rowReb = c( rawMin=3 ,lowE=1 ,rareE=1 ,dupESum=1 )     # s5에서 모두 reb2가 나오고 있다... 젭라.
            ,rowRebDup = NULL
            ,summMtx   = matrix( c(  1 ,2 ,2 ,2 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
            ,summMtx.reb = matrix( c(  1 ,2 ,2 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
            ,summMtx.sum = c(raw=4 ,evt=3)
            ,scMtx.sz = matrix( c(  3 ,3 ,1 ,1 ,1 ,1   ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                            #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                            #             rebCnt    0      0           0    0      0           0
                            #             rebDup    0      0           0    0      0           0
            )
            ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=2)
            ,isHard=NULL  # use default
        )
    }


    mfName <- "mfLArn"
    if( FALSE ){    # hpn1-col1HpnN 에서 hpn(2) 발생. 의미없다. (hpn1, hpnE의 min/max만 의미있다..)
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL        ,evtMax = NULL
            ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     # H787 hpn(1) ,col1Hpn(1)
            ,rowRebDup = NULL
            ,summMtx   = matrix( c(  1 ,2 ,2 ,1 ,2 ,2     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
            ,summMtx.reb = NULL     
            ,summMtx.sum = c(raw=3 ,evt=2)
            ,scMtx.sz = matrix( c(  3 ,3 ,1 ,2 ,2 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLVrn"
    if( FALSE ){    # hpn1-col1HpnN 에서 hpn(2) 발생. 의미없다. (hpn1, hpnE의 min/max만 의미있다..)
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL        ,evtMax = NULL      
            ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )     # H787 hpn(1) ,col1Hpn(1)
            ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLAVrn"
    if( FALSE ){    # hpn1-col1HpnN 에서 hpn(2) 발생. 의미없다. (hpn1, hpnE의 min/max만 의미있다..)
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }


    mfName <- "mfLAen"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(3,3,3),c(5,6,7)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn6" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL        ,evtMax = NULL      
            ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
            ,rowRebDup = NULL
            ,summMtx   = matrix( c(  1 ,3 ,2 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
            ,summMtx.sum = c(raw=4 ,evt=2)
            ,summMtx.reb = NULL
            ,scMtx.sz = matrix( c(  3 ,4 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=5 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLVen"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(3,3,3),c(5,6,7)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn6" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }


    mfName <- "mfLAcn"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLVcn"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }


    mfName <- "mfLAfn"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLVfn"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLAVfn"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpn1" =list( rng=matrix( c(0,4 ,0,4) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnE" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"col1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"colEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLAecf13"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpnA1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnAE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnB1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnBE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLAecf24"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpnA1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnAE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnB1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnBE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLVecf13"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpnA1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnAE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnB1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnBE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )

    }

    mfName <- "mfLVecf24"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "hpnA1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnAE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnB1" =list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"hpnBE" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aCol1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"aColEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn1" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn3" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn4" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn5" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bCol1Hpn6" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn1" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn3" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn4" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn5" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
                ,"bColEHpn6" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
            ,summMtx    = NULL      ,summMtx.reb = NULL     ,summMtx.sum = NULL
            ,scMtx.sz = matrix( c(  2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                                #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                                #   "rebCnt" ,"rebDup"
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
            )
            ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=2 )
            ,isHard=NULL  # use default
        )
    }


    mfName <- "mfLAVr_hpn1AB"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "Ar13Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ar13Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ar24Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ar24Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vr13Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vr13Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vr24Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vr24Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      
            ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
            ,rowRebDup = NULL
            ,summMtx   = matrix( c(  1 ,1 ,1 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
            ,summMtx.reb = NULL
            ,summMtx.sum = c(raw=4 ,evt=3)
            ,scMtx.sz = matrix( c(  1 ,1 ,1 ,1 ,1 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                            #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                            #             rebCnt    0      0           0    0      0           0
                            #             rebDup    0      0           0    0      0           0
            )
            ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLAVe_hpn1AB"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "Ae13Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ae13Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ae24Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ae24Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ve13Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ve13Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ve24Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ve24Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      
            ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
            ,rowRebDup = NULL
            ,summMtx   = matrix( c(  1 ,1 ,1 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
            ,summMtx.reb = NULL
            ,summMtx.sum = c(raw=4 ,evt=3)
            ,scMtx.sz = matrix( c(  1 ,1 ,1 ,1 ,1 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                            #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                            #             rebCnt    0      0           0    0      0           0
                            #             rebDup    0      0           0    0      0           0
            )
            ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLAVc_hpn1AB"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "Ac13Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ac13Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ac24Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Ac24Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vc13Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vc13Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vc24Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vc24Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      
            ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
            ,rowRebDup = NULL
            ,summMtx   = matrix( c(  1 ,1 ,1 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
            ,summMtx.reb = NULL
            ,summMtx.sum = c(raw=4 ,evt=3)
            ,scMtx.sz = matrix( c(  1 ,1 ,1 ,1 ,1 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                            #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                            #             rebCnt    0      0           0    0      0           0
                            #             rebDup    0      0           0    0      0           0
            )
            ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
            ,isHard=NULL  # use default
        )
    }

    mfName <- "mfLAVf_hpn1AB"
    if( FALSE ){
        scoreMtxCfg[[mfName]] <- list(
            mName = mName   ,style=c( freqZero=TRUE )
            ,fCol = list(
                "Af13Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Af13Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Af24Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Af24Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vf13Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vf13Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vf24Hpn1A" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
                ,"Vf24Hpn1B" =list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                                ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                                ,evtMax.fCol=NULL   # ,freqVal=c(1) ,forbidEvtReb=c(3)
                            )
            )
            ,evtMaxFColTot = NULL
            ,evtMax     = NULL      
            ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=2 )
            ,rowRebDup = NULL
            ,summMtx   = matrix( c(  1 ,1 ,1 ,1 ,1 ,1     , 1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
            ,summMtx.reb = NULL
            ,summMtx.sum = c(raw=4 ,evt=3)
            ,scMtx.sz = matrix( c(  1 ,1 ,1 ,1 ,1 ,1   ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                            ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                            #     $scMtx.sz      r.ph r.fCol r.dblHpnFlg e.ph e.fCol e.dblHpnFlg
                            #             rebCnt    0      0           0    0      0           0
                            #             rebDup    0      0           0    0      0           0
            )
            ,scMtx.sz.sum = c(rebCnt.r=2 ,rebCnt.e=2)
            ,isHard=NULL  # use default
        )
    }

}

