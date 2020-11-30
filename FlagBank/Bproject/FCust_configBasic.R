

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
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rem0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=5 ,lev2Max=5 ,lev3Max=2 )
                    ) 
        ,"rem0.len.tot"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=5 ,lev2Max=5 ,lev3Max=2 )
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
    ,rowReb = c( rawMin=3 ,lowE=3 ,rareE=1 )
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

mName <- "score2"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rebV.r"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,1,2,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=13 ,lev2Max=3 ,lev3Max=2 )   # lev1Max 0은 아예 없는 듯..
                    )
        ,"rebL"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"rebR"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"rebC.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=9 ,lev2Max=3 ,lev3Max=2 )
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
                        ,evtMax.fCol=c( lev1Max=5 ,lev2Max=3 ,lev3Max=2 )
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
    ,rowReb = c( rawMin=4 ,lowE=1 ,rareE=1 )
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

mName <- "score3"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rebPtn.1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
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
                    ) 
        ,"snFCnt.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"snMax.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"snFCnt.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"snMax.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"snFCnt.f"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 )
    ,rowRebDup = c( lowE=2 ,rareE=1 )
    # ,summMtx = NULL
    ,summMtx = matrix( c(  1 ,2 ,3 ,2 ,2 ,1   ,1 ,2 ,3 ,2 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = NULL
    ,summMtx.sum = NULL
    # ,summMtx.sum = c(raw=4 ,evt=4)
    # ,scMtx.sz = NULL
    ,scMtx.sz = matrix( c( 4 ,2 ,1 ,4 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    # ,scMtx.sz.sum = NULL
    ,scMtx.sz.sum = c(rebCnt.r=4 ,rebCnt.e=4)
    ,isHard=NULL  # use default
)

mName <- "score4"
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

mName <- "score5"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
        ,"pBanN.n"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
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
                    ) 
        ,"FVa.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"m4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                    )
    ) 
    ,evtMax = NULL
    ,rowReb = c( rawMin=4 ,lowE=2 ,rareE=1 )
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
    ,rowReb = NULL  # use default   c( rawMin=  )
    ,rowRebDup = NULL
    # ,summMtx = NULL ,summMtx.reb = NULL 
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
        # ,"aFV.m"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
        #                 ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
        #             ) 
        # ,"aFV.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
        #                 ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
        #             ) 
        ,"m4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = NULL  # use default   c( rawMin=  )
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
                    )
        ,"c32"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    ) 
        ,"c33"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                    ) 
        ,"c34"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
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
                    ) 
        ,"max2MatCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                    ) 
        ,"min2MatCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=3 ,lev2Max=3 ,lev3Max=3 )
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
    ,rowReb = NULL
    ,rowRebDup = NULL
    ,summMtx = matrix( c(   1 ,2 ,2 ,2 ,2 ,2    ,1 ,2 ,2 ,2 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
    )
    ,summMtx.reb = NULL ,summMtx.sum = NULL
    ,scMtx.sz = matrix( c(  3 ,2 ,1 ,3 ,2 ,1    ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
    )
    ,scMtx.sz.sum = c( rebCnt.r=3 ,rebCnt.e=3 )
    ,isHard=NULL  # use default
)

mName <- "score9"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rCnt" =list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
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
                    ) 
        ,"eD2" =list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"eDn" =list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"eLr" =list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"eRl" =list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"cCnt"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=c( lev1Max=6 ,lev2Max=6 ,lev3Max=2 )
                    ) 
        ,"cD2" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"cDn" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"cLr" =list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"cRl" =list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
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
    ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 )
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
    ,evtMax = NULL      ,rowReb = NULL          ,rowRebDup = NULL
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
    ,evtMax = NULL      ,rowReb = NULL          ,rowRebDup = NULL
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
    ,evtMax = NULL      ,rowReb = NULL          ,rowRebDup = NULL
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
    ,evtMax = NULL      ,rowReb = NULL          ,rowRebDup = NULL
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
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
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
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
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
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
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
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
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
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
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
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
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
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
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
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
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
                        ,evt=matrix( c(c(2,3,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
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
    ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 )    ,rowRebDup=NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=3)
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=3)
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=3)
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,2 ,2  ,1 ,1 ,1 ,1 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = matrix( c( 2 ,2 ,1 ,2 ,2 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                            #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                            #   "rebCnt" ,"rebDup"
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=3)
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLAc24"
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVc13"
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "scoreLVc24"
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
        ,summMtx = matrix( c(   1 ,1 ,1 ,1 ,1 ,1  ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
                ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                ,dimnames=list(summMtxName$rName,summMtxName$cName)
            )
        ,summMtx.reb = NULL     ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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
        ,rowReb = NULL      ,rowRebDup=NULL
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
                        ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"rem.3"=list( rng=matrix( c(NA,NA ,1,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"rem.4"=list( rng=matrix( c(NA,NA ,1,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
                    )
        ,"rem.6"=list( rng=matrix( c(NA,NA ,1,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2 ) # colname c("lev","val")
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
    ,rowReb = NULL  ,rowRebDup = c( lowE=3 ,rareE=1 )
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
    ,rowReb = NULL  ,rowRebDup = NULL
    ,summMtx = NULL ,summMtx.reb = NULL ,summMtx.sum = NULL
    ,scMtx.sz = NULL,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)




mfName <- "mfABCD"
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
                        ,evtMax.fCol=NULL
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
    ,evtMax     = NULL      ,rowReb = NULL          ,rowRebDup = NULL
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


mfName <- "mf4567"
scoreMtxCfg[[mfName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pBanN.n" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"pLCol" =list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
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
                        ,evtMax.fCol=NULL
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
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
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
    ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 )
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


mfName <- "mfLArn"
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

mfName <- "mfLVrn"
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

mfName <- "mfLAVrn"
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


mfName <- "mfLAen"
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

mfName <- "mfLVen"
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


mfName <- "mfLAcn"
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

mfName <- "mfLVcn"
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


mfName <- "mfLAfn"
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

mfName <- "mfLVfn"
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

mfName <- "mfLAVfn"
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

mfName <- "mfLAecf13"
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

mfName <- "mfLAecf24"
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

mfName <- "mfLVecf13"
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

mfName <- "mfLVecf24"
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


for( mName in names( scoreMtxCfg ) ){ # naming 추가.

    for( fcName in names(scoreMtxCfg[[mName]]$fCol) ){
        colnames(scoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
        rownames(scoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("min","max")

        colnames(scoreMtxCfg[[mName]]$fCol[[fcName]]$evt) <- c("lev","val")

        # ,forbidEvtReb=c(2,3)    # just for dev
        # ,freqVal=c(1,2)   # just for dev

        if( is.null(scoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol ) ){
            # fCol 별 전체 phase 대상으로 evt 발생 제한.( >= 기준 cut )
            # scoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
            scoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
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






