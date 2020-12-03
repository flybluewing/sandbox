scrExtMtxCfg <- list()

mName <- "score1"
scrExtMtxCfg[[mName]] <- list()
if( TRUE ){

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "remN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"remN.len.tot"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"remN.len.val"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"cN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"cN.len.tot"=list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"cN.len.val"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"fN.num"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"fN.len.tot"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"fN.len.val"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=1 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    scrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "evt0.num"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        ) 
            ,"evt0.len.tot"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt0.len.val"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt1.num"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt1.len.tot"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt1.len.val"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=1 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

} else if( FALSE ) {    # 기존 코드 백업.
    
    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "remN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"remN.len.tot"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"remN.len.val"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"cN.num"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"cN.len.tot"=list( rng=matrix( c(0,4 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"cN.len.val"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        )
            ,"fN.num"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"fN.len.tot"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"fN.len.val"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(3,4,5,6)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    scrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "evt0.num"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        ) 
            ,"evt0.len.tot"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt0.len.val"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt1.num"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt1.len.tot"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
            ,"evt1.len.val"=list( rng=matrix( c(0,4 ,0,6) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3,3),c(2,3,4,5,6,7,8)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "score2"
scrExtMtxCfg[[mName]] <- list()
if( TRUE ){
    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rebCN.r"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(3,4,5,6)) ,ncol=2)
                        ) 
            ,"rebCN.c"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"rebCN.f"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"incN.r"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(3,4,5,6)) ,ncol=2)
                        )
            ,"incN.c"=list( rng=matrix( c(0,3 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"incN.f"=list( rng=matrix( c(0,4 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"matCntRebC12"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
            ,"matCntInc12"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        )
            ,"matCntInc123"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3),c(1,2,3,4,5,6)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,2,3,1)   ,byrow=T    ,ncol=4
                        ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    scrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "evtRebLR"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        ) 
            ,"evtRebC"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        )
            ,"evtRebC2"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        )
            ,"evtInc"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        )
            ,"evtInc2"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3,3),c(2,3,4,5,6,7)) ,ncol=2)
                        )
            ,"evtInc3"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(2,3,4,5,6)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "score3"
scrExtMtxCfg[[mName]] <- list()
if( TRUE ){
    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rebPtnESum"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"snR3E"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3),c(1)) ,ncol=2)
                        )
            ,"snMaxESum"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3,3),c(3,4,5,6,7)) ,ncol=2)
                        )
            ,"snFCntESum"=list( rng=matrix( c(0,3 ,0,4) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
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
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "score4"
if( TRUE ){    # sample
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xBan.x"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3),c(2,3,4)) ,ncol=2)
                        )
            ,"xLCol"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xEn"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xfNum"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"eSum_FVaM4"=list( rng=matrix( c(0,2 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "score5"
if( TRUE ){    # sample
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xBan.x"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,2,3),c(2,3,4)) ,ncol=2)
                        )
            ,"xLCol"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xEn"=list( rng=matrix( c(0,0 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xfNum"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)                        )
            ,"eSum_FVaM4"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,0 ,2,2,3,2) ,byrow=T ,ncol=4
                            ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "score6"
if( TRUE ){    # sample
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xBan.x"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xLCol"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xEn"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xfNum"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"xMH"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"eSum_FVaM4"=list( rng=matrix( c(0,5 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "score7"
if( TRUE ){    # sample
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xBan.x"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
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
            ,"eSum_FVaM4"=list( rng=matrix( c(0,3 ,0,5) ,ncol=2 )
                            ,evt=matrix( c(c(1,2,3,3),c(2,3,4,5)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "score8"
if( TRUE ){    # sample
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "c3x"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"c2x"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
                        )
            ,"c3x2xOvLAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"c3x2xOvLPar"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(1,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"c3x2xOvLNo"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )           # H800_0
                            ,evt=matrix( c(c(3,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
            ,"maxMin3"=list( rng=matrix( c(0,0 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
                        )
            # ,"maxMin2MatCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
            #                 ,evt=matrix( c(c(2,3,3,3,3),c(2,3,4,5,6)) ,ncol=2)
            #             )
            ,"max3min2"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
                        )
            ,"max2min3"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
                        )
            ,"cfTbl"=list( rng=matrix( c(0,0 ,0,0) ,ncol=2 )
                            ,evt=matrix( c(c(3,3),c(1,2)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,2,3,1)       ,byrow=T ,ncol=4
                            ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 )
        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}

mName <- "score9"
if( TRUE ){    # sample
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xCntECnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3),c(2,3,4)) ,ncol=2)
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
        ,rowReb = c(rawMin=2,lowE=2,rareE=1,dupESum=2)              ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL                   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}





mName <- "scoreE"
if( TRUE ){    # sample
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "xH0MLen"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"xH0Cnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"xH0VCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"xH1MLen"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"xH1Cnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"xH1VCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"xH2MLen"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"xH2Cnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"xH2VCnt"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL           ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

    scrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "rHxMLen"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        ) 
            ,"cHxMLen"=list( rng=matrix( c(0,2 ,0,3) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"fHxMLen"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"rHxCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"cHxCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"fHxCnt"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"rHxVCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"cHxVCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"fHxVCnt"=list( rng=matrix( c(0,1 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,0 ,2,2,3,1) ,byrow=T ,ncol=4
                                ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=3 ,lowE=2 ,rareE=1 ,dupESum=2 )     ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL               ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    
}



mName <- "scoreLAr13"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
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

mName <- "scoreLAr24"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
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

mName <- "scoreLVr13"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
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

mName <- "scoreLVr24"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
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


mName <- "scoreLAe13"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL             ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "scoreLAe24"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL           ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "scoreLVe13"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL           ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "scoreLVe24"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL           ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}


mName <- "scoreLAc13"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL             ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "scoreLAc24"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL             ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "scoreLVc13"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"colHpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL             ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "scoreLVc24"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"colHpn1"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn2"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn3"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn4"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn5"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"colHpn6"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpn1All"=list( rng=matrix( c(0,2 ,0,2) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,2,3,1 ,2,2,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=2 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL             ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}


mName <- "scoreLAf13"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL             ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "scoreLAf24"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL             ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "scoreLVf13"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL             ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}

mName <- "scoreLVf24"
if( TRUE ){
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
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
            ,"hpn1All"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3),c(3,4,5,6,7,8,9)) ,ncol=2)
                        )
            ,"hpnEAll"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(3,3,3,3,3,3,3,3),c(2,3,4,5,6,7,8,9)) ,ncol=2)
                        )
        )
        ,evtMax = matrix( c(2,1,3,1 ,2,1,3,1)   ,byrow=T ,ncol=4
                    ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
        )
        ,rowReb = c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 ) ,rowRebDup=NULL
        ,summMtx = NULL     ,summMtx.reb = NULL             ,summMtx.sum = NULL
        ,scMtx.sz = NULL    ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )

}





mName <- "codeSample"
if( FALSE ){    # sample
    scrExtMtxCfg[[mName]] <- list()

    scrExtMtxCfg[[mName]]$filter01 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "col02"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"col02"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
    scrExtMtxCfg[[mName]]$filter02 <- list(
        mName = mName   ,style=c( freqZero=TRUE )
        ,fCol = list(
            "col02"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 ,dimnames=list(c("min","max"),c("lev1","lev2")) )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        ) 
            ,"col02"=list( rng=matrix( c(0,1 ,0,1) ,ncol=2 )
                            ,evt=matrix( c(c(2,3,3,3),c(1,2,3,4)) ,ncol=2)
                        )
        )
        ,evtMax = NULL        ,rowReb = NULL        ,rowRebDup=NULL
        ,summMtx = NULL       ,summMtx.reb = NULL   ,summMtx.sum = NULL
        ,scMtx.sz = NULL      ,scMtx.sz.sum = NULL
        ,isHard=NULL  # use default
    )
}


for( mName in names( scrExtMtxCfg ) ){              # mName <- names( scrExtMtxCfg )[1]
    for( fName in names(scrExtMtxCfg[[mName]]) ){   # fName <- names(scrExtMtxCfg[[mName]])[1]
        # scrExtMtxCfg[[mName]][[fName]]
        for( fcName in names(scrExtMtxCfg[[mName]][[fName]]$fCol) ){    # fcName <- names(scrExtMtxCfg[[mName]][[fName]]$fCol)[1]
            colnames(scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$rng) <- c("lev1","lev2")
            rownames(scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$rng) <- c("min","max")

            colnames(scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$evt) <- c("lev","val")

            if( is.null(scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$evtMax.fCol ) ){
                # fCol 별 전체 phase 대상으로 evt 발생 제한.
                scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$evtMax.fCol <- c( minLev=2 ,maxHpn=1 )
            }
            if( is.null(scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$forbidEvtReb) ){
                scrExtMtxCfg[[mName]][[fName]]$fCol[[fcName]]$forbidEvtReb <- c(2,3,4)
            }
        }

        if( is.null(scrExtMtxCfg[[mName]][[fName]]$evtMax) ){
            #   한 개 phase 내에서의 이벤트 발생 제한.
            #   "minLev"이상 이벤트가 maxHpn 보다 초과할 때 Cut
            evtMax <- matrix( c(2,1,3,0 ,2,2,3,1)
                                ,byrow=T ,ncol=4
                                ,dimnames=list(c("lev1","lev2"),c("minLev","maxHpn","minLevH","maxHpnH")) 
                            )
            scrExtMtxCfg[[mName]][[fName]]$evtMax     <- evtMax
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$rowReb) ){
            scrExtMtxCfg[[mName]][[fName]]$rowReb <- c( rawMin=1 ,lowE=2 ,rareE=1 ,dupESum=2 )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$rowRebDup) ){  # 조건 : >=
            scrExtMtxCfg[[mName]][[fName]]$rowRebDup <- c( lowE=1 ,rareE=1 )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$summMtx) ){
            #     $summMtx    all ph fCol phReb xyCnt.fCol xyCnt.phase
            #             raw   0  0    0     0          0           0
            #             evt   0  0    0     0          0           0
            # cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
            # rName <- c( "raw" ,"evt" )
            thldVal <- c(   1 ,2 ,2 ,2 ,2 ,1    # xyCnt.fCol, xyCnt.fCol은 
                            ,1 ,2 ,2 ,2 ,2 ,1 
                        )
            scrExtMtxCfg[[mName]][[fName]]$summMtx <- matrix( thldVal ,byrow=T
                        ,ncol=length(summMtxName$cName) ,nrow=length(summMtxName$rName)
                        ,dimnames=list(summMtxName$rName,summMtxName$cName)
                    )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$summMtx.reb ) ){
            #     $summMtx.reb    all ph fCol phReb xyCnt.fCol xyCnt.phase
            #                 raw   0  0    0     0          0           0
            #                 evt   0  0    0     0          0           0
            cName <- c( "all" ,"ph" ,"fCol" ,"phReb" ,"xyCnt.fCol" ,"xyCnt.phase" )
            rName <- c( "raw" ,"evt" )
            thldVal <- c(   1 ,1 ,1 ,1 ,1 ,1    # xyCnt.fCol, xyCnt.fCol은 봐가며 조절해야 할 듯.
                            ,1 ,1 ,1 ,1 ,1 ,1 
                        )
            scrExtMtxCfg[[mName]][[fName]]$summMtx.reb <- matrix( thldVal ,byrow=T
                        ,ncol=length(cName) ,nrow=length(rName) ,dimnames=list(rName,cName)
                    )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$scMtx.sz ) ){
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
            scrExtMtxCfg[[mName]][[fName]]$scMtx.sz <- matrix( thldVal ,byrow=T
                        ,ncol=length(scMtx.szName$cName) ,nrow=length(scMtx.szName$rName) 
                        ,dimnames=list(scMtx.szName$rName,scMtx.szName$cName)
                    )
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$summMtx.sum) ){
            # 개개 cut은 피했지만 전체 발생 총합은 한계를 넘는 경우.
            scrExtMtxCfg[[mName]][[fName]]$summMtx.sum <- c(raw=2 ,evt=2)
        }
        if( is.null(scrExtMtxCfg[[mName]][[fName]]$scMtx.sz.sum ) ){
            #   rebCnt.r = r.ph+r.fCol    rebCnt.e = e.ph+e.fCol
            scrExtMtxCfg[[mName]][[fName]]$scMtx.sz.sum <- c(rebCnt.r=2 ,rebCnt.e=2)
        }

    }   # fName
}   # mName

