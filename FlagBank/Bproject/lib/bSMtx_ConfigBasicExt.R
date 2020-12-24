bsScrExtMtxCfg <- list()


mName <- "sScore01"
if( TRUE ){

    bsScrExtMtxCfg[[mName]] <- list()

    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rem0.len.tot"=list( rng=matrix( c(0,1 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                            ,evtMax.fCol = c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(2)
                        )
            ,"rem1.len.tot"=list( rng=matrix( c(0,1 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                            ,evtMax.fCol = c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                            ,freqVal=c(2)
                        )
            ,"c0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol = c( lev1Max=3 ,lev2Max=3 ,lev3Max=2 )
                        )
            ,"c1.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                            ,evtMax.fCol = c( lev1Max=3 ,lev2Max=2 ,lev3Max=2 )
                            ,freqVal=c(2)
                        )
            ,"f0.len.tot"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
            ,"f1.len.tot"=list( rng=matrix( c(0,1 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(3,4,5)) ,ncol=2)
                            ,evtMax.fCol=NULL
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c(rawMin=1,lowE=1,rareE=1,dupESum=1 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    bsScrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "remN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"remN.len.tot"=list( rng=matrix( c(0,2 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,3,3),c(3,4,5,6)) ,ncol=2)
                            ,freqVal=c(2)
                        )
            ,"remN.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"cN.num"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"cN.len.tot"=list( rng=matrix( c(0,0 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,freqVal=c(2)
                        )
            ,"cN.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=c(2)
                        )
            ,"fN.num"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        )
            ,"fN.len.tot"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"fN.len.val"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c(rawMin=2,lowE=2,rareE=1,dupESum=1 )   # *.val에서 발생이 일어나면 *.tot에서도 값이 발생하기 때문에..
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    bsScrExtMtxCfg[[mName]]$filter03 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "evt0.num"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                        ) 
            ,"evt0.len.tot"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"evt0.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"evt1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        )
            ,"evt1.len.tot"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"evt1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
        )
        ,evtMax = NULL        ,rowReb = c(rawMin=2,lowE=2,rareE=1,dupESum=1 )  ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

} else if( FALSE ){

    bsScrExtMtxCfg[[mName]] <- list()

    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "remN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=c(2)
                        ) 
            ,"remN.len.tot"=list( rng=matrix( c(0,2 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(2,2,3,3),c(3,4,5,6)) ,ncol=2)
                            ,freqVal=c(2)
                            ,forbidEvtReb=c(2,3)
                        )
            ,"remN.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"cN.num"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=c(2)
                        )
            ,"cN.len.tot"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,freqVal=c(2)
                        )
            ,"cN.len.val"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"fN.num"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        )
            ,"fN.len.tot"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                            ,freqVal=c(2)
                        )
            ,"fN.len.val"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        # ,rowReb = c(rawMin=3,lowE=1,rareE=1)    # *.val에서 발생이 일어나면 *.tot에서도 값이 발생하기 때문에..
        ,rowReb = c(rawMin=1,lowE=1,rareE=1,dupESum=1 )   # *.val에서 발생이 일어나면 *.tot에서도 값이 발생하기 때문에..
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

    bsScrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "evt0.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        ) 
            ,"evt0.len.tot"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        )
            ,"evt0.len.val"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        )
            ,"evt1.num"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        )
            ,"evt1.len.tot"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        )
            ,"evt1.len.val"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = c(rawMin=2,lowE=1,rareE=1)  ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "sScore02"
if( FALSE ){

    bsScrExtMtxCfg[[mName]] <- list()

    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rebCN.r"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,5),c(3,4,5)) ,ncol=2)
                            # ,freqVal=c(1,2)   # 적용 고려 중. H873,H832
                        ) 
            ,"rebCN.c"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3),c(3,4)) ,ncol=2)
                            ,freqVal=c(1,2)
                        )
            ,"rebCN.f"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,5),c(3,4,5)) ,ncol=2)
                        )
            ,"incN.r"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,5),c(3,4,5)) ,ncol=2)
                        )
            ,"incN.c"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,5),c(3,4,5)) ,ncol=2)
                        )
            ,"incN.f"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,5),c(3,4,5)) ,ncol=2)
                        )
            ,"matCntRebC12"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"matCntInc12"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"matCntInc123"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c(rawMin=1,lowE=1,rareE=1,dupESum=1 )             ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

    bsScrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "evtRebC"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(4,5,6,7)) ,ncol=2)
                        )
            ,"evtRebC2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(4,5,6,7)) ,ncol=2)
                        )
            ,"evtInc"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(4,5,6,7)) ,ncol=2)
                        )
            ,"evtInc2"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(4,5,6,7)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c(rawMin=1,lowE=1,rareE=1,dupESum=1 )             ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

} else if( FALSE ){

    # bsScrExtMtxCfg[[mName]] <- list()

    # bsScrExtMtxCfg[[mName]]$filter01 <- list(
    #     mName = mName   ,style=c( freqZero=TRUE )
    #     ,fCol = list(
    #         "rebCN.r"=list( rng=matrix( c(0,0 ,0,5) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
    #                         ,evt=matrix( c(c(3,3,3,5),c(2,3,4,5)) ,ncol=2)
    #                     ) 
    #         ,"rebCN.c"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"rebCN.f"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"incN.r"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"incN.c"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"incN.f"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"matCntRebC12"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"matCntInc12"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"matCntInc123"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #     )
    #     ,evtMax = NULL        ,rowReb = NULL                        ,rowRebDup=NULL
    #     ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
    #     ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
    #     ,isHard=NULL  # use default
    # )

    # bsScrExtMtxCfg[[mName]]$filter02 <- list(
    #     mName = mName   ,style=c( freqZero=TRUE )
    #     ,fCol = list(
    #         "evtRebC"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"evtRebC2"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"evtInc"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #         ,"evtInc2"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
    #                         ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
    #                     )
    #     )
    #     ,evtMax = NULL        ,rowReb = NULL                        ,rowRebDup=NULL
    #     ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
    #     ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
    #     ,isHard=NULL  # use default
    # )

}

mName <- "sScore03"
if( TRUE ){

    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rebPtnESum"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"snR3E"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
                        )
            ,"snMaxESum"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"snFCntESum"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"snRESum"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"snCESum"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"snFESum"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "sScore04"
if( TRUE ){

    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xBan.x"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3),c(2,3,4)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"xLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)  # H864(evt2 for hpn1)
                            ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"xEn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"xfNum"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"xMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                        )
            ,"eSum_FVaM4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                            ,evtMax.fCol=c( lev1Max=2 ,lev2Max=2 ,lev3Max=2 )
                        )
        )
        ,evtMax = NULL        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "sScore05"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xBan.x"=list( rng=matrix( c(0,2 ,0,4) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        )
            ,"xLCol"=list( rng=matrix( c(0,1 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=c(1,2)
                        )
            ,"xEn"=list( rng=matrix( c(0,0 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xfNum"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xMH"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)                        )
            ,"eSum_FVaM4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                            ,freqVal=c(1)
                        )
        )
        ,evtMax = matrix( c(2,1,3,0 ,2,2,3,2) ,byrow=T ,ncol=4
                            ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=1 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore06"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xBan.x"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,3,3),c(2,3,4)) ,ncol=2)
                            # ,freqVal=c(1)     # 고려
                        )
            ,"xLCol"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                            # ,freqVal=c(1)     # 고려
                        )
            ,"xEn"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xfNum"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"eSum_FVaM4"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,1,3,3),c(2,3,4,5)) ,ncol=2)
                            # ,freqVal=c(1,2)     # 고려
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c( rawMin=1,lowE=1,rareE=1 ,dupESum=1 )        
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore07"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xBan.x"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xEn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xfNum"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xMH"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"eSum_FVaM4"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )   # 5
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,0 ,2,1,3,1)   ,byrow=T ,ncol=4
                        ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1,lowE=1,rareE=1 ,dupESum=1 )        
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore08"
if( FALSE ){
    # 폐지. cStep의 폭이 일정치 않아서 score8처럼 유지할 수가 없다.
    #   그냥 sScore08 자체에 score8 ext 개념을 적용했음.
}

mName <- "sScore09"
if( TRUE ){

    bsScrExtMtxCfg[[mName]] <- list()

    bsScrExtMtxCfg[[mName]]$freqValReb <- list(
        # c( "rD2" ,"rLr" ,"rRl" ,"eD2" ,"eLr" ,"eRl" ,"cD2" ,"cLr" ,"cRl" ,"fCnt" )
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rD2"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
            ,"rLr"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
            ,"rRl"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
            ,"eD2"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
            ,"eLr"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
            ,"eRl"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
            ,"cD2"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
            ,"cLr"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
            ,"cRl"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
            ,"fCnt"=list( rng=matrix( c(0,7 ,0,7) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3),c(4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL
        ,rowReb = c(rawMin=2,lowE=1,rareE=1,dupESum=1)
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xCntECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                            ,freqVal=c(1)
                        ) 
            ,"xD2ECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"xDnECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"xLrECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"xRlECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"r2EvtCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"e2EvtCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"c2EvtCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"f2EvtCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c(rawMin=1,lowE=1,rareE=1,dupESum=1)
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}


mName <- "sScore0LAr13"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = NULL    ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LAr24"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = NULL    ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LVr13"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = NULL    ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LVr24"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = NULL    ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LAe13"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = NULL    ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LAe24"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = NULL    ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LVe13"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = NULL    ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LVe24"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = NULL    ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LAc13"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LAc24"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LVc13"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LVc24"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2) ,freqVal=c(1)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb =c( rawMin=1,lowE=1,rareE=1,dupESum=1 )   ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LAf13"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )  # H829 colBn_hpn1(2) hpn1All(2)
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 ) ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LAf24"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )  # H829 colBn_hpn1(2) hpn1All(2)
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 ) ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LVf13"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )  # H829 colBn_hpn1(2) hpn1All(2)
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 ) ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "sScore0LVf24"
if( TRUE ){
    bsScrExtMtxCfg[[mName]] <- list()
    bsScrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "colAn_hpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"colBn_hpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colAn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colBn_hpnE"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn1"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )  # H829 colBn_hpn1(2) hpn1All(2)
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=1 ,rareE=1 ,dupESum=1 ) ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

for( mName in names( bsScrExtMtxCfg ) ){              # mName <- names( bsScrExtMtxCfg )[1]
    for( fName in names(bsScrExtMtxCfg[[mName]]) ){   # fName <- names(bsScrExtMtxCfg[[mName]])[1]
        # bsScrExtMtxCfg[[mName]][[fName]]
        for( fcName in names(bsScrExtMtxCfg[[mName]][[fName]]$fCol) ){    # fcName <- names(bsScrExtMtxCfg[[mName]][[fName]]$fCol)[1]
            colnames(bsScrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
            rownames(bsScrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$rng) <- c("min","max")

            colnames(bsScrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$evt) <- c("lev","val")

            if( is.null(bsScrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$evtMax.fCol ) ){
                # fCol 별 전체 phase 대상으로 evt 발생 제한.
                bsScrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$evtMax.fCol <- c( minLev=2 ,maxHpn=1 )
            }
            if( is.null(bsScrExtMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb) ){
                bsScrExtMtxCfg[[mName]]$fCol[[fcName]]$forbidEvtReb <- c(1,2,3,4)
            }

        }

        if( is.null(bsScrExtMtxCfg[[mName]][[fName]]$evtMax) ){
            #   한 개 phase 내에서의 이벤트 발생 제한.
            #   "minLev"이상 이벤트가 maxHpn 보다 초과할 때 Cut
            evtMax <- matrix( c(2,1,3,0 ,2,2,3,1)
                                ,byrow=T ,ncol=4
                                ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
                            )
            bsScrExtMtxCfg[[mName]][[fName]]$evtMax     <- evtMax
        }
        if( is.null(bsScrExtMtxCfg[[mName]][[fName]]$rowReb) ){
            bsScrExtMtxCfg[[mName]][[fName]]$rowReb <- c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=1 )
        }
        if( is.null(bsScrExtMtxCfg[[mName]][[fName]]$rowRebDup) ){  # 조건 : >=
            bsScrExtMtxCfg[[mName]][[fName]]$rowRebDup <- c( lowE=1 ,rareE=1 )
        }
        if( is.null(bsScrExtMtxCfg[[mName]][[fName]]$summMtx) ){
            #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
            #             raw   0  0    0     0          0           0
            #             evt   0  0    0     0          0           0
            # cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
            # rName <- c( "raw" ,"evt" )
            thldVal <- c(   1 ,2 ,2 ,2 ,2 ,1    # xyCnt.fCol, xyCnt.fCol은 
                            ,1 ,2 ,2 ,2 ,2 ,1 
                        )
            bsScrExtMtxCfg[[mName]][[fName]]$summMtx <- matrix( thldVal ,byrow=T
                        ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                        ,dimnames=list(summMtxName$rName,summMtxName$cName)
                    )
        }
        if( is.null(bsScrExtMtxCfg[[mName]][[fName]]$summMtx.reb ) ){
            #     $summMtx.reb    all ph fCol phReb xyCnt.fCol xyCnt.phase
            #                 raw   0  0    0     0          0           0
            #                 evt   0  0    0     0          0           0
            cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
            rName <- c( "raw" ,"evt" )
            thldVal <- c(   1 ,1 ,1 ,1 ,1 ,1    # xyCnt.fCol, xyCnt.fCol은 봐가며 조절해야 할 듯.
                            ,1 ,1 ,1 ,1 ,1 ,1 
                        )
            bsScrExtMtxCfg[[mName]][[fName]]$summMtx.reb <- matrix( thldVal ,byrow=T
                        ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
                    )
        }
        if( is.null(bsScrExtMtxCfg[[mName]][[fName]]$scMtx.sz ) ){
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
            bsScrExtMtxCfg[[mName]][[fName]]$scMtx.sz <- matrix( thldVal ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        }
        if( is.null(bsScrExtMtxCfg[[mName]][[fName]]$summMtx.sum) ){
            # 개개 cut은 피했지만 전체 발생 총합은 한계를 넘는 경우.
            bsScrExtMtxCfg[[mName]][[fName]]$summMtx.sum <- c(raw=2 ,evt=2)
        }
        if( is.null(bsScrExtMtxCfg[[mName]][[fName]]$scMtx.sz.sum ) ){
            #   rebCnt.r = r.ph+r.fCol    rebCnt.e = e.ph+e.fCol
            bsScrExtMtxCfg[[mName]][[fName]]$scMtx.sz.sum <- c(rebCnt.r=2 ,rebCnt.e=2)
        }

    }   # fName
}   # mName

