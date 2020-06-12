

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
    #   XXX.tot, XXX.val 관계 때문에 2개 fCol 동시발생이 흔함을 고려.
    ,evtMax <- matrix( c(2,1,3,1 ,2,2,3,2)  # evt in row over fCols
                        ,byrow=T ,ncol=4
                        ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
                    )
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
                    )
        ,"rebPtn.n"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"snR3" =list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )    # flag
                        ,evt=matrix( c(c(1),c(3)) ,ncol=2)
                    )
        ,"snMax.r"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"snFCnt.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"snMax.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"snFCnt.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"snMax.f"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"snFCnt.f"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 )
    ,rowRebDup = c( lowE=2 ,rareE=1 )
    # ,summMtx = NULL
    ,summMtx = matrix( c(  1 ,2 ,3 ,2 ,2 ,1   ,1 ,2 ,3 ,2 ,2 ,1 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = NULL
    ,summMtx.sum = c(raw=4 ,evt=4)
    ,scMtx.sz = matrix( c( 4 ,3 ,1 ,4 ,3 ,1     ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=5 ,rebCnt.e=5)
    ,isHard=NULL  # use default
)

mName <- "score4"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    )
        ,"pBanN.n"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"pLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pE3"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pfNum"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iBanN"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"FVa.m"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"FVa.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        # ,"aFV.m"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )  aFV.max aFV.hpnCnt 사실상 중복되는 경우가 많아 폐지.
        #                 ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
        #             ) 
        # ,"aFV.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
        #                 ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
        #             ) 
        ,"m4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
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
                    )
        ,"pBanN.n"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"pLCol"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"pE3"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pfNum"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iBanN"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"iLCol"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    )
        ,"ifNum"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"FVa.m"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3),c(3,4,5)) ,ncol=2)  # 2는 생각보다 자주 일어나는 듯.
                    ) 
        ,"FVa.c"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        # ,"aFV.m"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
        #                 ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
        #             ) 
        # ,"aFV.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
        #                 ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
        #             ) 
        ,"m4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )
    ) 
    ,evtMax = NULL
    ,rowReb = c( rawMin=4 ,lowE=2 ,rareE=1 )
    ,rowRebDup = NULL
    ,summMtx = NULL 
    ,summMtx.reb = matrix( c( 1 ,2 ,2 ,1 ,1 ,1      ,1 ,1 ,1 ,1 ,1 ,1 ) ,byrow=T
                    #   c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
                    #   c( "raw" ,"evt" )
                    ,ncol=length(summMtx.rebName$cName) ,nrow=length(summMtx.rebName$rName) 
                    ,dimnames=list(summMtx.rebName$rName,summMtx.rebName$cName)
                )
    ,summMtx.sum = NULL
    ,scMtx.sz = matrix( c( 3 ,3 ,1 ,2 ,2 ,1     ,2 ,2 ,1 ,1 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=2)
    ,isHard=NULL  # use default
)

mName <- "score6"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "pBanN.r"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    )
        ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"pLCol"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iBanN"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iLCol"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"FVa.m"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"FVa.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        # ,"aFV.m"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
        #                 ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
        #             ) 
        # ,"aFV.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
        #                 ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
        #             ) 
        ,"m4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
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
                    )
        ,"pBanN.n"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"pLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"pfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iBanN"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iE3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iE4"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"iMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"ifNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"FVa.m"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"FVa.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        # ,"aFV.m"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
        #                 ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
        #             ) 
        # ,"aFV.c"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
        #                 ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
        #             ) 
        ,"m4"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
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
                    )
        ,"c32"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"c33"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"c34"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"c21"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"c22"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"c23"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"c24"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"c25"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"max3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"min3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"max2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"min2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"cTbl"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"fTbl"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
    )
    ,evtMax = NULL
    ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 )
    ,rowRebDup = NULL
    # ,summMtx = NULL 
    ,summMtx = matrix( c(   1 ,2 ,2 ,2 ,2 ,2     , 1 ,2 ,2 ,2 ,2 ,2 ) ,byrow=T # all ph fCol phReb xyCnt.fCol xyCnt.phase
            ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
            ,dimnames=list(summMtxName$rName,summMtxName$cName)
        )
    ,summMtx.reb = NULL 
    ,summMtx.sum = c(raw=3 ,evt=3)
    ,scMtx.sz = matrix( c( 3 ,2 ,1 ,3 ,2 ,1     ,2 ,1 ,1 ,2 ,1 ,1 ) ,byrow=T
                        #   "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg"
                        #   "rebCnt" ,"rebDup"
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    ,scMtx.sz.sum = c(rebCnt.r=3 ,rebCnt.e=3)
    ,isHard=NULL  # use default
)

mName <- "score9"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    )
        ,"rD2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"rDn"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"rLr"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"rRl"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"eCnt"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                    ) 
        ,"eD2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"eDn"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"eLr"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"eRl"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"cCnt"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"cD2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"cDn"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"cLr"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"cRl"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"fD2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"fDn"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"fLr"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    ) 
        ,"fRl"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
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

for( mName in names( scoreMtxCfg ) ){ # naming 추가.

    for( fcName in names(scoreMtxCfg[[mName]]$fCol) ){
        colnames(scoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
        rownames(scoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("min","max")

        colnames(scoreMtxCfg[[mName]]$fCol[[fcName]]$evt) <- c("lev","val")

        if( is.null(scoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol ) ){
            # fCol 별 전체 phase 대상으로 evt 발생 제한.
            scoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( minLev=2 ,maxHpn=2 )
        }
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
        scoreMtxCfg[[mName]]$rowReb <- c( rawMin=1 ,lowE=2 ,rareE=1 )
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
        sfcMtxCfg[[hName]]$basic$prime <- matrix( c(1,5,0,1 ,0,1,0,1 ,0,5,0,4 ,0,0,0,1 ) 
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






