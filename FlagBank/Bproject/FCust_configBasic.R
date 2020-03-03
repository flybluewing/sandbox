

scoreMtxCfg <- list()

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
#             ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 )  # use default   c( rawMin=  )
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


mName <- "score1"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rem0.num"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                    ) 
        ,"rem0.len.tot"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
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
        ,"c0.num"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"c0.len.tot"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3),c(2,3,4,5)) ,ncol=2)
                    )
        ,"c0.len.val"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3),c(2,3,4,5)) ,ncol=2)
                    )
        ,"c1.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"c1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                    )
        ,"c1.len.val"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                    )

        ,"f0.num"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"f0.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(2,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                    )
        ,"f0.len.val"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                    )
        ,"f1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"f1.len.tot"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                    )
        ,"f1.len.val"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                    )

        ,"zwNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
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
    ,isHard=NULL  # use default
)

mName <- "score2"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rebV.r"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    )
        ,"rebL"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"rebR"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    )

        ,"rebC.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    ) 
        ,"rebC.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"rebC.f"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    )
        ,"rebC2.r"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    )
        ,"rebC2.c"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"rebC2.f"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    )

        ,"inc.r"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    ) 
        ,"inc.c"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"inc.f"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    )
        ,"inc.r2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    ) 
        ,"inc.c2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
        ,"inc.f2"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    )

        ,"inc.r3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,2,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                    ) 
        ,"inc.c3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                        ,evt=matrix( c(c(1,2,2,3,3),c(1,2,3,4,5)) ,ncol=2)
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = NULL  # use default   c( rawMin=  )
    ,isHard=NULL  # use default
)

mName <- "score3"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "fCol1"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"fCol2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = NULL  # use default   c( rawMin=  )
    ,isHard=NULL  # use default
)

mName <- "score4"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "fCol1"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"fCol2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = NULL  # use default   c( rawMin=  )
    ,isHard=NULL  # use default
)

mName <- "score5"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "fCol1"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"fCol2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = NULL  # use default   c( rawMin=  )
    ,isHard=NULL  # use default
)

mName <- "score6"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "fCol1"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"fCol2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = NULL  # use default   c( rawMin=  )
    ,isHard=NULL  # use default
)

mName <- "score7"
scoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "fCol1"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    )
        ,"fCol2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(1,1,2,3,4),c(1,2,3,4,5)) ,ncol=2)
                    ) 
    ) 
    ,evtMax = NULL
    ,rowReb = NULL  # use default   c( rawMin=  )
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
        scoreMtxCfg[[mName]]$rowReb.dup <- c( rawMin=1 ,lowE=2 ,rareE=1 )
    }
    if( is.null(scoreMtxCfg[[mName]]$summMtx) ){
        #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #             raw   0  0    0     0          0           0
        #             evt   0  0    0     0          0           0
        cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
        rName <- c( "raw" ,"evt" )
        thldVal <- c(   1 ,2 ,2 ,2 ,1 ,1    # xyCnt.fCol, xyCnt.fCol은 봐가며 조절해야 할 듯.
                        ,1 ,2 ,2 ,2 ,1 ,1 
                    )
        scoreMtxCfg[[mName]]$summMtx <- matrix( thldVal ,byrow=T
                    ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
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
        cName <- c( "r.ph" ,"r.fCol" ,"r.dblHpnFlg" ,"e.ph" ,"e.fCol" ,"e.dblHpnFlg" )
        rName <- c( "rebCnt" ,"rebDup" )
        # thldVal <- c(   2 ,2 ,1 ,2 ,2 ,1
        #                 ,1 ,1 ,1 ,1 ,1 ,1 
        #             )
        thldVal <- c(    1 ,1 ,1 ,1 ,1 ,1
                        ,1 ,1 ,1 ,1 ,1 ,1 
                    )
        scoreMtxCfg[[mName]]$scMtx.sz <- matrix( thldVal ,byrow=T
                    ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
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






