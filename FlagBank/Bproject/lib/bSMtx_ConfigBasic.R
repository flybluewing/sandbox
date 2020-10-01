bsScoreMtxCfg <- list()
if( FALSE ){    # Document
    # bsScoreMtxCfg$fCol
    #   - 일단 Lev1, Lev2를 남겨놓고 Lev2만 사용한다. 나중에 필요해질 수도 있을 듯.

    # mName <- "template"
    # bsScoreMtxCfg[[mName]] <- list(
    #     mName = mName   ,style=c( freqZero=TRUE )
    #     ,fCol = list(
    #         "xxx"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=NULL
    #                     ) 
    #         ,"xxx"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
    #                         ,evtMax.fCol=NULL
    #                     )
    #     )
    #     ,evtMax = NULL  ,evtMaxFColTot = NULL
    #     ,rowReb = NULL  ,rowRebDup=NULL
    #     ,summMtx = NULL ,summMtx.reb = NULL ,summMtx.sum = NULL
    #     ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
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

mName <- "sScore01"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rem0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rem0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rem0.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rem1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rem1.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rem1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"c0.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"c0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"c0.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"c1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"c1.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"c1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"f0.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"f0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"f0.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"f1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"f1.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"f1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL  ,evtMaxFColTot = NULL
    ,rowReb = NULL  ,rowRebDup=NULL
    ,summMtx = NULL ,summMtx.reb = NULL ,summMtx.sum = NULL
    ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)


mName <- "sScore02"
bsScoreMtxCfg[[mName]] <- list(
    mName = mName   ,style=c( freqZero=TRUE )
    ,fCol = list(
        "rebC.r"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    ) 
        ,"rebC.c"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rebC.f"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rebC2.r"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rebC2.c"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"rebC2.f"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.r"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.c"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.f"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.r2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.c2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
        ,"inc.f2"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                        ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                        ,evtMax.fCol=NULL
                    )
    )
    ,evtMax = NULL  ,evtMaxFColTot = NULL
    ,rowReb = NULL  ,rowRebDup=NULL
    ,summMtx = NULL ,summMtx.reb = NULL ,summMtx.sum = NULL
    ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
    ,isHard=NULL  # use default
)


for( mName in names( bsScoreMtxCfg ) ){ # naming 추가.

    for( fcName in names(bsScoreMtxCfg[[mName]]$fCol) ){
        colnames(bsScoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
        rownames(bsScoreMtxCfg[[mName]]$fCol[[fcName]]$rng) <- c("min","max")

        colnames(bsScoreMtxCfg[[mName]]$fCol[[fcName]]$evt) <- c("lev","val")

        if( is.null(bsScoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol ) ){
            # fCol 별 전체 phase 대상으로 evt 발생 제한.( >= 기준 cut )
            # bsScoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
            bsScoreMtxCfg[[mName]]$fCol[[fcName]]$evtMax.fCol <- c( lev1Max=4 ,lev2Max=3 ,lev3Max=2 )
        }
    }

    if( is.null(bsScoreMtxCfg[[mName]]$evtMaxFColTot) ){
        #   fCol 4 all Ph에서 CloseMax 가 나타난 fCol 수.
        bsScoreMtxCfg[[mName]]$evtMaxFColTot  <- c( lev1Max=3 ,lev2Max=3 ,lev3Max=3 )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$evtMax) ){
        #   한 개 phase 내에서의 이벤트 발생 제한.
        #   "minLev"이상 이벤트가 maxHpn 보다 초과할 때 Cut
        evtMax <- matrix( c(2,2,3,1 ,2,3,3,2)
                            ,byrow=T ,ncol=4
                            ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
                        )
        bsScoreMtxCfg[[mName]]$evtMax     <- evtMax
    }
    if( is.null(bsScoreMtxCfg[[mName]]$rowReb) ){
        bsScoreMtxCfg[[mName]]$rowReb <- c( rawMin=1 ,lowE=2 ,rareE=1 )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$rowRebDup) ){  # 조건 : >=
        bsScoreMtxCfg[[mName]]$rowRebDup <- c( lowE=1 ,rareE=1 )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$summMtx) ){
        #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #             raw   0  0    0     0          0           0
        #             evt   0  0    0     0          0           0
        # cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
        # rName <- c( "raw" ,"evt" )
        thldVal <- c(   1 ,2 ,2 ,2 ,2 ,1    # xyCnt.fCol, xyCnt.fCol은 
                        ,1 ,2 ,2 ,2 ,2 ,1 
                    )
        bsScoreMtxCfg[[mName]]$summMtx <- matrix( thldVal ,byrow=T
                    ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                    ,dimnames=list(summMtxName$rName,summMtxName$cName)
                )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$summMtx.reb ) ){
        #     $summMtx.reb    all ph fCol phReb xyCnt.fCol xyCnt.phase
        #                 raw   0  0    0     0          0           0
        #                 evt   0  0    0     0          0           0
        cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
        rName <- c( "raw" ,"evt" )
        thldVal <- c(   1 ,1 ,1 ,1 ,1 ,1    # xyCnt.fCol, xyCnt.fCol은 봐가며 조절해야 할 듯.
                        ,1 ,1 ,1 ,1 ,1 ,1 
                    )
        bsScoreMtxCfg[[mName]]$summMtx.reb <- matrix( thldVal ,byrow=T
                    ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
                )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$scMtx.sz ) ){
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
        bsScoreMtxCfg[[mName]]$scMtx.sz <- matrix( thldVal ,byrow=T
                    ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                    ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                )
    }
    if( is.null(bsScoreMtxCfg[[mName]]$summMtx.sum) ){
        # 개개 cut은 피했지만 전체 발생 총합은 한계를 넘는 경우.
        bsScoreMtxCfg[[mName]]$summMtx.sum <- c(raw=2 ,evt=2)
    }
    if( is.null(bsScoreMtxCfg[[mName]]$scMtx.sz.sum ) ){
        #   rebCnt.r = r.ph+r.fCol    rebCnt.e = e.ph+e.fCol
        bsScoreMtxCfg[[mName]]$scMtx.sz.sum <- c(rebCnt.r=2 ,rebCnt.e=2)
    }

}




# =============================================================================================================
#   bsSfcMtxCfg
# =============================================================================================================

bsSfcMtxCfg <- list()   # B.makeHMtxLst() 의 stdFilter참고
if( 0==length(bsSfcMtxCfg) ){
    sfcName <- "sfcLate"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "sfc0"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "sfc1"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "sfc2"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "NGD0000.A"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "NGA0100.A"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )

    sfcName <- "NGAP000.E"
    bsSfcMtxCfg[[sfcName]] <- list(
        basic=list( prime=NULL
        )
        ,bScr=list(  )
    )
}

for( hName in names(bsSfcMtxCfg) ){

    if( is.null(bsSfcMtxCfg[[hName]]$basic$prime) ){
        bsSfcMtxCfg[[hName]]$basic$prime <- matrix( c(1,8,0,1 ,0,1,0,1 ,0,9,0,4 ,0,0,0,1 ) 
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

    # if( is.null(bsSfcMtxCfg[[hName]]$basic$zeroCnt) ){
    #     bsSfcMtxCfg[[hName]]$basic$zeroCntM <- matrix( c(0,0 ,0,1) 
    #                         ,nrow=2 ,byrow=T 
    #                         ,dimnames=list(c("zeroCntM","zeroCntPh"),c("min","max")) 
    #                     )
    # }

    # if( is.null(bsSfcMtxCfg[[hName]]$evtMax) ){   }

}



